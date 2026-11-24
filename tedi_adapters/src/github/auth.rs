use std::{
	collections::{BTreeMap, HashMap},
	sync::Arc,
};

use reqwest::Client;
use serde::Deserialize;

use super::GithubError;

pub(super) enum Auth {
	Token(String),
	App(App),
	/// A person's token on each account they brought one of, `rest` on any other.
	Keys {
		keys: BTreeMap<String, String>,
		rest: Arc<Auth>,
	},
}
impl Auth {
	/// `owner` is the account the request acts on: an App holds one installation token per account.
	pub(super) async fn token(&self, http: &Client, owner: Option<&str>) -> Result<String, GithubError> {
		let mut auth = self;
		//LOOP each step is one layer of keys down
		loop {
			match auth {
				Auth::Token(token) => return Ok(token.clone()),
				Auth::App(app) => return app.installation_token(http, owner.expect("an App's request names the account it acts on")).await,
				Auth::Keys { keys, rest } => match owner.and_then(|o| keys.get(&o.to_lowercase())) {
					Some(token) => return Ok(token.clone()),
					None => auth = rest,
				},
			}
		}
	}
}

pub(super) struct App {
	id: u64,
	key: jsonwebtoken::EncodingKey,
	/// by lowercased login: GitHub matches logins case-insensitively
	tokens: tokio::sync::Mutex<HashMap<String, InstallationToken>>,
}
impl App {
	pub(super) fn try_new(id: u64, private_key: &str) -> Result<Self, GithubError> {
		Ok(Self {
			id,
			key: jsonwebtoken::EncodingKey::from_rsa_pem(private_key.as_bytes())?,
			tokens: Default::default(),
		})
	}

	/// The App's bot account, whom its installation tokens act as.
	pub(super) async fn login(&self, http: &Client) -> Result<String, GithubError> {
		#[derive(Deserialize)]
		struct AppInfo {
			slug: String,
		}
		let res = self.as_app(http, reqwest::Method::GET, "https://api.github.com/app")?.send().await?;
		Ok(format!("{}[bot]", ok(res, "fetch the App").await?.json::<AppInfo>().await?.slug))
	}

	async fn installation_token(&self, http: &Client, owner: &str) -> Result<String, GithubError> {
		let owner = owner.to_lowercase();
		let mut tokens = self.tokens.lock().await; // held across the mint, so concurrent requests wait for one token rather than each minting
		if let Some(t) = tokens.get(&owner)
			&& t.expires_at > jiff::Timestamp::now() + jiff::SignedDuration::from_mins(5)
		{
			return Ok(t.token.clone());
		}
		let installation = self.installation(http, &owner).await?;
		let res = self
			.as_app(http, reqwest::Method::POST, &format!("https://api.github.com/app/installations/{installation}/access_tokens"))?
			.send()
			.await?;
		let minted = ok(res, &format!("mint an installation token for {owner}")).await?.json::<InstallationToken>().await?;
		let token = minted.token.clone();
		tokens.insert(owner, minted);
		Ok(token)
	}

	/// Listed afresh on every mint, so an installation made since is found without a restart.
	async fn installation(&self, http: &Client, owner: &str) -> Result<u64, GithubError> {
		#[derive(Deserialize)]
		struct Installation {
			id: u64,
			account: super::GithubUser,
		}
		for page in 1.. {
			let res = self
				.as_app(http, reqwest::Method::GET, &format!("https://api.github.com/app/installations?per_page=100&page={page}"))?
				.send()
				.await?;
			let batch = ok(res, "list the App's installations").await?.json::<Vec<Installation>>().await?;
			if let Some(i) = batch.iter().find(|i| i.account.login.eq_ignore_ascii_case(owner)) {
				return Ok(i.id);
			}
			if batch.len() < 100 {
				break;
			}
		}
		Err(GithubError::new_other(format!("GitHub App {} is not installed on {owner}", self.id)))
	}

	fn as_app(&self, http: &Client, method: reqwest::Method, url: &str) -> Result<reqwest::RequestBuilder, GithubError> {
		#[derive(serde::Serialize)]
		struct Claims {
			iat: i64,
			exp: i64,
			iss: String,
		}
		let now = jiff::Timestamp::now().as_second();
		// backdated against clock drift; GitHub refuses an `exp` past 10 minutes
		let claims = Claims {
			iat: now - 60,
			exp: now + 9 * 60,
			iss: self.id.to_string(),
		};
		let jwt = jsonwebtoken::encode(&jsonwebtoken::Header::new(jsonwebtoken::Algorithm::RS256), &claims, &self.key)?;
		Ok(http
			.request(method, url)
			.header("User-Agent", "Rust Github Client")
			.header("Accept", "application/vnd.github+json")
			.bearer_auth(jwt))
	}
}

#[derive(Deserialize)]
struct InstallationToken {
	token: String,
	expires_at: jiff::Timestamp,
}

async fn ok(res: reqwest::Response, context: &str) -> Result<reqwest::Response, GithubError> {
	match res.status().is_success() {
		true => Ok(res),
		false => {
			let status = res.status();
			let body = res.text().await.unwrap_or_default(); // the status alone still names the failure
			Err(GithubError::new_api(status, body, context.to_string()))
		}
	}
}
