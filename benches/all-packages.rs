use criterion::{criterion_group, criterion_main, Benchmark, Criterion, Throughput};
use rnix::parse;
use rnix_analyzer::{AnalysisOptions, AnalysisResult};

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let parsed = parse(input);
    let options = AnalysisOptions {};
    c.bench(
        "all-packages",
        Benchmark::new("all-packages", move |b| {
            b.iter(|| AnalysisResult::from(&parsed, &options))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(30),
    );
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
