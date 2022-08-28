extern crate qitty;

use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let max_results = 1000;

    qitty::q_internal("start", 1);
    let queries = [
        "test",
        "l.......v",
        "..i[sz]e",
        "[l-p].[m-r].[w-z]",
        "....i[!stz]e",
        "#@#@#@#@#@#@#@",
        "xo*",
        "x*a",
        "*xj*",
        ".j*k*",
        "*a*e*i*o*u*",
        "*@@@@@*",
        "ace>",
        "-6:x*a",
        "7-9:x*a",
        "10-:x*a",
        "9:.<.",
        "`bonge",
        "?`str.g.ly",
        "/triangle",
        "/triangle.",
        "7-:*/rpoyesdif",
        "10:q*s/squarepeginaroundhole",
        "/qxz*",
        "-8:/aeiou*",
        "*j.|*j",
        "kn*&<",
        "12-:!*<*",
        "!>*.&(b*|*b)",
        "AA",
        "ACBC;|A|=1;|B|=1;!=AB",
        "AABB",
        "A~A",
        "A.~A",
        "AB;BA",
        "AB;BA;!=AB",
        "AkB;AlB",
        "AaB;AeB;AiB;AoB;AuB",
        "AaB;AeB;AiB;AoB;AuB;|A|=0-;|B|=0-",
        "A###B;A@@@B",
        "A###B;A@@@B;|A|=0-;|B|=0-;|AB|=3",
        "Ared;Agreen",
        "draA;poAy",
        "A;B;C;|A|=5;|B|=5;|C|=5;ABC",
        "A(/blue)B;A(/orange)B", // needed to add parens here
        "|B|=6;AB;A~B",
        //TODO
        //"A=(*/turquoise);|A|=4-;|B|=2-;AB;BA	",
        //"A=(/lilac);A.;A..;A...;A....;A.....",
        // "ACB;ADB;AEB;AFB;AGB;AHB;|ACB|=8;|H|=1;!=C<D<E<F<G<H",
        // "ABCDEF=......;!=A<B<C<D<E<F",
    ];
    for q in queries {
        c.bench_function(q, |b| b.iter(|| qitty::q_internal(q, max_results)));
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
