use criterion::{criterion_group, criterion_main, Benchmark, Criterion, Throughput};
use rand::Rng;
use rnix::{parse, TextRange, TextSize};
use rnix_analyzer::AnalysisResult;

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    c.bench(
        "all-packages parsing",
        Benchmark::new("parsing", move |b| b.iter(|| parse(input)))
            .throughput(Throughput::Bytes(input.len() as u64))
            .sample_size(10),
    );
    let parsed = parse(input);
    c.bench(
        "all-packages parsing",
        Benchmark::new("analyze", move |b| b.iter(|| AnalysisResult::from(&parsed)))
            .throughput(Throughput::Bytes(input.len() as u64))
            .sample_size(10),
    );

    let number_of_bytes = input.as_bytes().len() as u32;
    let parsed = parse(input);
    let result = AnalysisResult::from(&parsed);
    c.bench(
        "all-packages querying",
        Benchmark::new("scopes_at", move |b| {
            let mut rng = rand::thread_rng();
            let from = rng.gen_range(0u32..number_of_bytes);
            let to = rng.gen_range(from..number_of_bytes);

            b.iter(|| {
                result
                    .scopes_at(TextRange::new(TextSize::from(from), TextSize::from(to)))
                    .collect::<Vec<_>>()
            })
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
    let parsed = parse(input);
    let result = AnalysisResult::from(&parsed);
    c.bench(
        "all-packages querying",
        Benchmark::new("variables_at", move |b| {
            let mut rng = rand::thread_rng();
            let from = rng.gen_range(0u32..number_of_bytes);
            let to = rng.gen_range(from..number_of_bytes);

            b.iter(|| {
                result
                    .variables_at(TextRange::new(TextSize::from(from), TextSize::from(to)))
                    .collect::<Vec<_>>()
            })
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
