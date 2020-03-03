use criterion::{criterion_group, criterion_main, Benchmark, Criterion, Throughput};
use rnix::parse;
use rnix_analyzer::{references, scope, AnalysisResult};

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let parsed = parse(input);
    c.bench(
        "all-packages scopes",
        Benchmark::new("all-packages scopes", move |b| {
            b.iter(|| scope::collect_scopes(&parsed))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
    let parsed = parse(input);
    let (scopes, _) = scope::collect_scopes(&parsed);
    c.bench(
        "all-packages references",
        Benchmark::new("all-packages references", move |b| {
            b.iter(|| references::References::from_ast_and_scope_tree(&parsed, &scopes))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
    let parsed = parse(input);
    c.bench(
        "all-packages full",
        Benchmark::new("all-packages full", move |b| {
            b.iter(|| AnalysisResult::from(&parsed))
        })
        .throughput(Throughput::Bytes(input.len() as u64))
        .sample_size(10),
    );
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
