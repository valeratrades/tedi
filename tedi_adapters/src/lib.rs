#![feature(error_generic_member_access)]

pub mod clockify;
pub mod github;

// reqwest is built without a bundled TLS provider; `Err` only means one is already installed
pub(crate) fn client_builder() -> reqwest::ClientBuilder {
	rustls::crypto::ring::default_provider().install_default().ok();
	reqwest::Client::builder()
}
