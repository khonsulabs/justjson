use khonsu_tools::universal::clap::Parser;
use khonsu_tools::universal::{anyhow, DefaultConfig};
use khonsu_tools::Commands;

fn main() -> anyhow::Result<()> {
    Commands::parse().execute::<Config>()
}

enum Config {}

impl khonsu_tools::Config for Config {
    type Publish = Self;
    type Universal = Self;
}

impl khonsu_tools::universal::Config for Config {
    type Audit = DefaultConfig;
    type CodeCoverage = Self;
}

impl khonsu_tools::publish::Config for Config {
    fn paths() -> Vec<String> {
        vec![String::from(".")]
    }
}

impl khonsu_tools::universal::code_coverage::Config for Config {
    fn cargo_args() -> Vec<String> {
        vec![
            String::from("test"),
            String::from("--workspace"),
            String::from("--all-targets"),
        ]
    }
}
