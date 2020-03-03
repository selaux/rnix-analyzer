use rayon::prelude::*;
use rnix_analyzer::{AnalysisOptions, AnalysisResult};
use std::env;
use std::fs;
use std::process;

fn analyze(file_name: &str) -> Result<(), String> {
    let file_contents =
        fs::read_to_string(file_name).map_err(|e| format!("{}: error reading {}", file_name, e))?;
    let parsed = rnix::parse(&file_contents);
    let analyzed = AnalysisResult::from(&parsed, &AnalysisOptions {});
    let parse_errors: Vec<_> = parsed.errors();
    let analysis_errors: Vec<_> = analyzed.errors().collect();

    for error in parse_errors.iter() {
        eprintln!("{}: {}", file_name, error);
    }
    for error in analysis_errors.iter() {
        eprintln!("{}: {:?}", file_name, error);
    }

    if parse_errors.is_empty() && analysis_errors.is_empty() {
        Ok(())
    } else {
        Err(format!("{}: encountered errors", file_name))
    }
}

pub fn main() {
    let file_names: Vec<_> = env::args().skip(1).collect();

    if file_names.is_empty() {
        eprintln!("Usage: rnix-analyze <file1> <file2> ...");
        process::exit(1);
    }

    file_names.par_iter().for_each(|file_name| {
        if let Err(e) = analyze(&file_name) {
            eprintln!("{}", e);
        }
    });
}
