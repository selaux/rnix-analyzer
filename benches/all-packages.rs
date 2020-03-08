use criterion::{criterion_group, criterion_main, Benchmark, Criterion, Throughput};
use rand::Rng;
use rnix::{parse, TextRange, TextUnit};
use rnix_analyzer::{references, scope, AnalysisResult};

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let parsed = parse(input);
    c.bench(
        "all-packages parsing",
        Benchmark::new("scopes", move |b| b.iter(|| scope::collect_scopes(&parsed)))
            .throughput(Throughput::Bytes(input.len() as u64))
            .sample_size(10),
    );
    let parsed = parse(input);
    let (scopes, _) = scope::collect_scopes(&parsed);
    c.bench(
        "all-packages parsing",
        Benchmark::new("references", move |b| {
            b.iter(|| references::References::from_ast_and_scope_tree(&parsed, &scopes))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
    let parsed = parse(input);
    c.bench(
        "all-packages parsing",
        Benchmark::new("full", move |b| b.iter(|| AnalysisResult::from(&parsed)))
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
            let from = rng.gen_range(0u32, number_of_bytes);
            let to = rng.gen_range(from, number_of_bytes);

            b.iter(|| {
                result
                    .scopes_at(TextRange::from_to(TextUnit::from(from), TextUnit::from(to)))
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
            let from = rng.gen_range(0u32, number_of_bytes);
            let to = rng.gen_range(from, number_of_bytes);

            b.iter(|| {
                result
                    .variables_at(TextRange::from_to(TextUnit::from(from), TextUnit::from(to)))
                    .collect::<Vec<_>>()
            })
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
