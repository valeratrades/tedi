#![doc = include_str!("../README.md")]

pub mod api;
#[cfg(feature = "server")]
mod server;
#[cfg(feature = "web")]
pub mod ui;

#[cfg(feature = "server")]
pub use server::{Settings, serve};
