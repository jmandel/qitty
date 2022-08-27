extern crate qitty;

use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let max_results = 1000;

    qitty::q_internal("start", 1);
    let queries = [
        "test",
        "AB;BA",
        "`bonge",
        "....i[!stz]e",
        "x*a",
        "ace>",
        "?`str.g.ly",
        "/triangle",
        "/triangle.",
        "kn* & <",
    ];
    for q in queries {
        c.bench_function(q, |b| b.iter(|| qitty::q_internal(q, max_results)));
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
