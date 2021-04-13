use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use rand::Rng;
use rnix::{parse, TextRange, TextSize};
use rnix_analyzer::AnalysisResult;

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let mut all_packages_analysis = c.benchmark_group("all-packages analysis");

    all_packages_analysis
        .bench_function("parsing", move |b| b.iter(|| parse(input)))
        .sample_size(10);
    let parsed = parse(input);
    all_packages_analysis
        .bench_function("analyzing", move |b| {
            b.iter(|| AnalysisResult::from(&parsed))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10);
    all_packages_analysis.finish();

    let number_of_bytes = input.as_bytes().len() as u32;
    let parsed = parse(input);
    let result = AnalysisResult::from(&parsed);
    let mut all_packages_querying = c.benchmark_group("all-packages querying");
    all_packages_querying
        .bench_function("scopes_at", move |b| {
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
        .sample_size(10);

    let parsed = parse(input);
    let result = AnalysisResult::from(&parsed);
    all_packages_querying
        .bench_function("variables_at", move |b| {
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
        .sample_size(10);
    all_packages_querying.finish();
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
