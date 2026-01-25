#![allow(clippy::bool_comparison)] // harder to read otherwise
#![allow(non_snake_case)]
use std::{
	fs::{self, OpenOptions},
	io::Write,
	os::unix::fs::PermissionsExt as _,
	path::{Path, PathBuf},
};

use clap::{Args, Subcommand};
use color_eyre::eyre::{Result, bail, ensure};
use serde::{Deserialize, Serialize, de::DeserializeOwned};
use v_utils::{Percent, io::file_open::OpenMode, time::Timelike};

use crate::utils;

pub async fn update_or_open(settings: &crate::config::LiveSettings, args: ManualArgs) -> Result<()> {
	let date = utils::format_date(args.days_back, settings);

	let target_file_path = Day::path(&date);

	match &args.command {
		ManualSubcommands::PrintEv(_) => {
			let day = Day::load(&date)?;
			println!("{}", day.ev);
			return Ok(());
		}
		ManualSubcommands::LastEvUpdateHours(_) => {
			let day = Day::load(&date).map_err(|_| color_eyre::eyre::eyre!("Day object not initialized"))?;
			let last_update_ts = day
				.last_ev_change
				.ok_or_else(|| color_eyre::eyre::eyre!("No last_ev_change recorded\nSuggestion: try to manually remove and re-initialize with `todo manual ev -r`"))?;

			let now = jiff::Timestamp::now();
			let full_hours_ago = (now - last_update_ts).get_hours();
			println!("{full_hours_ago}");
			return Ok(());
		}
		ManualSubcommands::Open(open_args) => match open_args.pbs {
			false => {
				if !target_file_path.exists() {
					bail!("Tried to open ev file of a day that was not initialized");
				}
				v_utils::io::file_open::open(&target_file_path).await?;
				return process_manual_updates(&target_file_path, settings);
			}
			true => {
				let pbs_path = target_file_path.parent().unwrap().join(PBS_FILENAME);
				return v_utils::io::file_open::Client::default().mode(OpenMode::Pager).open(&pbs_path).await;
			}
		},
		ManualSubcommands::Ev(_) | ManualSubcommands::CounterStep(_) => {}
	}

	let ev_override = match &args.command {
		ManualSubcommands::Ev(ev) => Some(ev.validate()?),
		_ => None,
	};

	let day = match Day::load(&date) {
		Ok(d) => {
			let mut d: Day = d;

			if let Some(ev_args) = &ev_override {
				d.ev = match ev_args.change {
					true => d.ev + ev_args.ev,
					false => ev_args.ev,
				};
			} else if let ManualSubcommands::CounterStep(step) = &args.command {
				if step.cargo_watch {
					d.counters.cargo_watch += 1;
				}
				if step.dev_runs {
					d.counters.dev_runs += 1;
				}
			}
			d
		}
		Err(_) => {
			let mut d = Day::default();
			//? should this not be a match?
			if let Some(ev_args) = &ev_override {
				ensure!(ev_args.replace, "The day object is not initialized, so `ev` argument must be provided with `-r --replace` flag");
				d.ev = ev_args.ev;
			} else if let ManualSubcommands::CounterStep(step) = args.command {
				if step.cargo_watch {
					d.counters.cargo_watch = 1;
				}
				if step.dev_runs {
					d.counters.dev_runs = 1;
				}
				eprintln!("Initialized day object automatically. EV is set to 0. Don't forget to set it properly today.");
			}

			d.date = date.to_owned();
			d
		}
	};

	let mut day = day;
	if !matches!(&args.command, &ManualSubcommands::CounterStep(_)) {
		day.last_ev_change = Some(jiff::Timestamp::now());
	}

	day.update_pbs(target_file_path.parent().unwrap(), settings);

	let formatted_json = serde_json::to_string_pretty(&day).unwrap();
	let mut file = OpenOptions::new().read(true).write(true).create(true).truncate(true).open(&target_file_path).unwrap();
	file.write_all(formatted_json.as_bytes()).unwrap();
	fs::set_permissions(&target_file_path, fs::Permissions::from_mode(0o666))?;

	if ev_override.is_some_and(|ev_args| ev_args.open) {
		v_utils::io::file_open::open(&target_file_path).await?;
		process_manual_updates(&target_file_path, settings)?;
	}

	Ok(())
}
#[derive(Args)]
pub struct ManualArgs {
	#[arg(short, long, default_value = "0")]
	pub days_back: usize,
	#[command(subcommand)]
	pub command: ManualSubcommands,
}
#[derive(Subcommand)]
pub enum ManualSubcommands {
	/// All evokations will be considered as updates in the `last-ev-update` command, even if we write exact same values.
	Ev(EvArgs),
	Open(OpenArgs),
	PrintEv(PrintArgs),
	/// Full hours since last update
	LastEvUpdateHours(LastEvUpdateArgs),
	CounterStep(CounterStepArgs),
}
#[derive(Args)]
pub struct EvArgs {
	#[arg(allow_hyphen_values = true)]
	pub ev: i32,
	#[arg(short, long)]
	pub open: bool,
	#[arg(short, long)]
	pub change: bool,
	#[arg(short, long, default_value = "true")]
	pub replace: bool,
}
impl EvArgs {
	//? This seems ugly. There has to be a way to do this natively with clap, specifically with the `conflicts_with` attribute
	fn validate(&self) -> Result<Self> {
		let replace = match self.change {
			true => false,
			false => self.replace,
		};
		if !self.change && !self.replace {
			bail!("Exactly one of {{'change', 'replace'}} must be specified.");
		}
		Ok(Self {
			ev: self.ev,
			open: self.open,
			change: self.change,
			replace,
		})
	}
}

#[derive(Args)]
pub struct OpenArgs {
	#[arg(short, long)]
	pub pbs: bool,
}
#[derive(Args)]
pub struct PrintArgs;
#[derive(Args)]
pub struct LastEvUpdateArgs;
#[derive(Args, Clone, Copy, Debug, Default, Deserialize, Serialize, derive_new::new)]
pub struct CounterStepArgs {
	/// Counter specifically for cargo_watch recompiles, as the metric is incocmpatible with workflow of other languages.
	#[arg(long)]
	pub cargo_watch: bool,
	/// Counter of dev test runs of a code in any language.
	#[arg(long)]
	pub dev_runs: bool,
}
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
/// Unless specified otherwise, all times are in minutes
pub struct Day {
	date: String,
	ev: i32,
	last_ev_change: Option<jiff::Timestamp>,
	morning: Morning,
	midday: Midday,
	evening: Evening,
	sleep: Sleep,
	counters: Counters,
	jofv_mins: Option<usize>,    // other types are self-regulating or even net positive (when work for v)
	non_negotiables_done: usize, // currently having 2 non-negotiables set for each day; but don't want to fix the value to that range, in case it changes.
	percent_focused: Percent,
	caffeine_only_during_work: bool,
	math_hours: Option<f64>,
	checked_messages_only_during_social_window: bool,
	number_of_rejections: usize,
	phone_locked_away: bool,
}
impl Day {
	pub fn path(date: &str) -> PathBuf {
		let data_storage_dir = v_utils::xdg_data_dir!(MANUAL_PATH_APPENDIX);
		data_storage_dir.join(format!("{date}.json"))
	}

	pub fn load(date: &str) -> Result<Self> {
		let target_file_path = Day::path(date);
		let file_contents: String = match std::fs::read_to_string(&target_file_path) {
			Ok(s) => s,
			Err(_) => "".to_owned(),
		};

		Ok(serde_json::from_str::<Day>(&file_contents)?)
	}

	fn update_pbs<T: AsRef<Path>>(&self, data_storage_dir: T, settings: &crate::config::LiveSettings) {
		//TODO!!: fix error with adding extra brackets to ~/.data/personal/manual_stats/.pbs.json
		fn announce_new_pb<T: std::fmt::Display>(new_value: &T, old_value: Option<&T>, name: &str) {
			let old_value = match old_value {
				Some(v) => v.to_string(),
				None => "None".to_owned(),
			};
			let announcement = format!("New pb on {name}! ({old_value} -> {new_value})");
			println!("{announcement}");
			std::process::Command::new("notify-send").arg(announcement).spawn().unwrap().wait().unwrap();
		}

		let pbs_path = data_storage_dir.as_ref().join(PBS_FILENAME);
		let yd_date = utils::format_date(1, settings); // no matter what file is being checked, we only ever care about physical yesterday
		let mut pbs_as_value = match std::fs::read_to_string(&pbs_path) {
			Ok(s) => serde_json::from_str::<serde_json::Value>(&s).unwrap(), // Value so we don't need to rewrite everything on `Day` struct changes. Both in terms of extra code, and recorded pb values. Previously had a Pbs struct, but that has proven to be unnecessary.
			Err(_) => serde_json::Value::Null,
		};

		fn conditional_update<T>(pbs_as_value: &mut serde_json::Value, metric: &str, new_value: T, condition: fn(&T, &T) -> bool)
		where
			T: Serialize + DeserializeOwned + PartialEq + Clone + std::fmt::Display + std::fmt::Debug, {
			let old_value = pbs_as_value.get(metric).and_then(|v| T::deserialize(v.clone()).ok()).map(|v| Some(v)).unwrap_or(None);

			match old_value {
				Some(old) =>
					if condition(&new_value, &old) {
						announce_new_pb(&new_value, Some(&old), metric);
						pbs_as_value[metric] = serde_json::to_value(&new_value).unwrap();
					} else {
						pbs_as_value[metric] = serde_json::to_value(&old).unwrap();
					},
				None => {
					announce_new_pb(&new_value, None, metric);
					pbs_as_value[metric] = serde_json::to_value(new_value).unwrap();
				}
			}
		}

		if self.ev >= 0 {
			conditional_update(&mut pbs_as_value, "ev", self.ev, |new, old| new > old);
		}

		if let Some(new_alarm) = &self.morning.alarm_to_run_M_colon_S {
			conditional_update(&mut pbs_as_value, "alarm_to_run", *new_alarm, |new, old| new < old);
		}

		if let Some(new_run_to_shower) = &self.morning.run_to_shower_M_colon_S {
			conditional_update(&mut pbs_as_value, "run_to_shower", *new_run_to_shower, |new, old| new < old);
		}

		if let Some(new_hours_of_work) = self.midday.hours_of_work {
			conditional_update(&mut pbs_as_value, "midday_hours_of_work", new_hours_of_work, |new, old| new > old);
		}

		let new_cw_counter = self.counters.cargo_watch;
		conditional_update(&mut pbs_as_value, "cw_counter", new_cw_counter, |new, old| *new >= old + 10); // 10-step increments to prevent spam

		// Returns bool for convienience of recursing some of these
		let mut streak_update = |metric: &str, condition: &dyn Fn(&Day) -> bool| -> bool {
			let load_streaks_from = data_storage_dir.as_ref().join(format!("{yd_date}.json"));
			let yd_streaks_source = match std::fs::read_to_string(&load_streaks_from) {
				Ok(s) => Some(serde_json::from_str::<Day>(&s).unwrap()),
				Err(_) => None,
			};

			let pb_streaks = pbs_as_value.get("streaks").unwrap_or(&serde_json::Value::Null);
			let read_streak: Streak = pb_streaks
				.get(metric)
				.map_or_else(Streak::default, |v| serde_json::from_value::<Streak>(v.clone()).unwrap_or_default());

			let is_validated: bool = yd_streaks_source.is_some() && condition(&yd_streaks_source.unwrap());
			let skip = match pb_streaks.get("__last_date_processed") {
				Some(v) => v.as_str().expect("The only way this panics is if user manually changes pbs file") == yd_date,
				None => false,
			};
			if !skip {
				let mut new_streak = if is_validated {
					Streak {
						pb: read_streak.pb,
						current: read_streak.current + 1,
					}
				} else {
					Streak { pb: read_streak.pb, current: 0 }
				};
				if new_streak.current > read_streak.pb {
					announce_new_pb(&new_streak.current, Some(&read_streak.current), metric);
					new_streak.pb = new_streak.current;
				}
				pbs_as_value["streaks"][metric] = serde_json::to_value(new_streak).unwrap();
			} else {
				pbs_as_value["streaks"][metric] = serde_json::to_value(read_streak).unwrap();
			}

			is_validated
		};

		let jofv_condition = |d: &Day| d.jofv_mins.is_some_and(|x| x == 0);
		let _ = streak_update("no_jofv", &jofv_condition);

		let stable_sleep_condition = |d: &Day| d.sleep.yd_to_bed_t_plus == Some(0) && d.sleep.from_bed_t_plus == Some(0) && d.sleep.from_bed_abs_diff_from_day_before == Some(0);
		let _ = streak_update("stable_sleep", &stable_sleep_condition);

		let meditation_condition = |d: &Day| d.evening.focus_meditation > 0;
		let _ = streak_update("focus_meditation", &meditation_condition);

		let math_condition = |d: &Day| d.math_hours.is_some_and(|q| q > 0.);
		let _ = streak_update("math", &math_condition);

		let nsdr_condition = |d: &Day| d.evening.nsdr > 0;
		let _ = streak_update("nsdr", &nsdr_condition);

		let perfect_morning_condition = |d: &Day| {
			d.morning.alarm_to_run_M_colon_S.is_some_and(|v| v.inner() < 10) //? is_some_and consumes self, why?
				&& d.morning.run_to_shower_M_colon_S.is_some_and(|v| v.inner() <= 5)
				&& d.morning.transcendential.eating_food.is_some_and(|v| v < 20)
				&& d.morning.breakfast_to_work.is_some_and(|v| v <= 5)
		};
		let _ = streak_update("perfect_morning", &perfect_morning_condition);

		let marafon_focus_condition = |d: &Day| d.percent_focused > 0.5;
		let _ = streak_update("NOs_streak", &marafon_focus_condition);

		let responsible_caffeine_condition = |d: &Day| d.caffeine_only_during_work == true;
		let _ = streak_update("responsible_caffeine", &responsible_caffeine_condition);

		let responsible_messengers_condition = |d: &Day| d.checked_messages_only_during_social_window == true;
		let _ = streak_update("responsible_messengers", &responsible_messengers_condition);

		let running_streak_condition = |d: &Day| d.morning.run == true;
		let _ = streak_update("running_streak", &running_streak_condition);

		let rejection_streak_condition = |d: &Day| d.number_of_rejections > 0;
		let _ = streak_update("rejection_streak", &rejection_streak_condition);

		let locked_phone_streak_condition = |d: &Day| d.phone_locked_away;
		let _ = streak_update("locked_phone_streak", &locked_phone_streak_condition);

		pbs_as_value["streaks"]["__last_date_processed"] = serde_json::Value::from(yd_date);

		let formatted_json = serde_json::to_string_pretty(&pbs_as_value).unwrap();
		let mut file = OpenOptions::new()
			.read(true)
			.write(true)
			.create(true)
			.truncate(true) //? what the hell does this do?
			.open(&pbs_path)
			.unwrap();
		file.write_all(formatted_json.as_bytes()).unwrap();
	}
}

static PBS_FILENAME: &str = ".pbs.json";

use crate::MANUAL_PATH_APPENDIX;

fn process_manual_updates<T: AsRef<Path>>(path: T, settings: &crate::config::LiveSettings) -> Result<()> {
	if !path.as_ref().exists() {
		bail!("File does not exist, likely because you manually changed something.");
	}
	let day: Day = serde_json::from_str(&std::fs::read_to_string(&path)?)?;
	day.update_pbs(path.as_ref().parent().unwrap(), settings);
	Ok(())
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct Transcendential {
	making_food: Option<usize>,
	eating_food: Option<usize>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct Sleep {
	yd_to_bed_t_plus: Option<i32>,
	from_bed_t_plus: Option<i32>,
	from_bed_abs_diff_from_day_before: Option<i32>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct Morning {
	alarm_to_run_M_colon_S: Option<Timelike>,
	run: bool,
	run_to_shower_M_colon_S: Option<Timelike>,
	#[serde(flatten)]
	transcendential: Transcendential,
	breakfast_to_work: Option<usize>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
// could be called `_8h`
struct Midday {
	hours_of_work: Option<usize>,
	#[serde(flatten)]
	transcendential: Transcendential,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
struct Evening {
	focus_meditation: usize, // fixed at 13m under current sota, but why not keep it flexible
	nsdr: usize,
	#[serde(flatten)]
	transcendential: Transcendential,
}

///// Accounts only for the time that is objectively wasted, aggregate positive ev situtations are not counted here.
//#[derive(Clone, Debug, Default, Deserialize, Serialize, derive_new::new)]
//struct Wasted {
//	jofv: usize
//	quazi_informational_content: usize,
//}

#[derive(Clone, Debug, Default, Deserialize, Serialize, derive_new::new)]
struct Counters {
	cargo_watch: usize,
	dev_runs: usize,
}

#[derive(Debug, Default, Deserialize, Serialize)]
struct Streak {
	pb: usize,
	current: usize,
}
