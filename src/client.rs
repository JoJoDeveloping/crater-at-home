use crate::{Crate, Status, Version};
use color_eyre::Result;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

#[derive(Clone)]
pub struct Client {}

fn mkpath(key: &str) -> PathBuf {
    let pp = Path::new("output").join(key);
    std::fs::create_dir_all(pp.parent().unwrap()).unwrap();
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
        let _res = file.read_to_end(&mut buf)?;
        Ok(buf)
    }

    #[allow(unused)]
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
