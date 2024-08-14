use crate::{Crate, Status, Version};
use aws_sdk_s3::model::{CompletedMultipartUpload, CompletedPart, Object};
use aws_smithy_types_convert::date_time::DateTimeExt;
use backoff::Error;
use backoff::ExponentialBackoff;
use color_eyre::Result;
use futures_util::StreamExt;
use futures_util::TryFutureExt;
use std::collections::HashMap;
use std::fs::File;
use std::future::Future;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

#[derive(Clone)]
pub struct Client {}

fn mkpath(key: &str) -> PathBuf {
    let pp = Path::new("persistent").join(key);
    std::fs::create_dir_all(pp.parent().unwrap());
    pp
}

impl Client {
    pub async fn new() -> Result<Self> {
        Ok(Self {})
    }

    pub fn write_persistent(&self, key: &str, data: &[u8]) -> Result<()> {
        let mut file = File::create(mkpath(key))?;
        file.write_all(data)?;
        Ok(())
    }

    pub fn read_persistent(&self, key: &str) -> Result<Vec<u8>> {
        let mut file = File::open(mkpath(key))?;
        let mut buf = Vec::new();
        let res = file.read_to_end(&mut buf)?;
        Ok(buf)
    }

    pub async fn get_crate_downloads(&self) -> Result<HashMap<String, Option<u64>>> {
        let blob = self.read_persistent("downloads.json")?;
        let crates: HashMap<_, _> = serde_json::from_slice(&blob)?;
        Ok(crates)
    }

    pub async fn get_crate_versions(&self) -> Result<Vec<Crate>> {
        let blob = self.read_persistent("crates.json")?;
        let crates: Vec<(String, String)> = serde_json::from_slice(&blob)?;
        let crates = crates
            .into_iter()
            .map(|krate| Crate {
                name: krate.0,
                version: Version::parse(&krate.1),
                status: Status::Unknown,
                recent_downloads: None,
            })
            .collect();
        Ok(crates)
    }
}
