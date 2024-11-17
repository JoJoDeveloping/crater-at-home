#![feature(btree_extract_if)]
use clap::Parser;
use color_eyre::Result;
use indicatif::MultiProgress;
use indicatif_log_bridge::LogWrapper;
use std::fmt;

mod analyze;
mod client;
mod db_dump;
mod run;
mod sync;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser)]
enum Commands {
    Run(run::Args),
    Sync(sync::Args),
    Analyze(analyze::Args),
}

fn main() -> Result<()> {
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }
    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info");
    }
    let logger = env_logger::Builder::from_env(env_logger::Env::default()).build();
    let multi = MultiProgress::new();

    LogWrapper::new(multi.clone(), logger).try_init().unwrap();
    color_eyre::install()?;

    let args = Cli::parse();
    match args.command {
        Commands::Run(args) => run::run(args),
        Commands::Sync(args) => sync::run(args),
        Commands::Analyze(args) => analyze::run(args, multi),
    }
}

#[derive(Clone, Debug)]
pub struct Crate {
    pub name: String,
    pub version: Version,
    pub recent_downloads: Option<u64>,
    pub status: Status,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Version {
    Parsed(semver::Version),
    Unparsed(String),
}

impl serde::Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Version {
    pub fn parse(s: &str) -> Self {
        semver::Version::parse(s)
            .map(Version::Parsed)
            .unwrap_or_else(|_| Version::Unparsed(s.to_string()))
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Version::Parsed(v) => write!(f, "{}", v),
            Version::Unparsed(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Status {
    Unknown,
    Passing,
    Error(String),
    UB { cause: Vec<Cause> },
}

#[derive(Clone, Debug, Ord, Eq, PartialEq, PartialOrd)]
pub struct Cause {
    pub kind: String,
    pub source_crate: Option<String>,
}
