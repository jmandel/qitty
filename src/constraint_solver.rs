use itertools::Itertools;
use std::{env, time::SystemTime};

type CoverGroup = usize;
type CoverSpan = (usize, usize);
fn propagate(constraints: &Vec<Option<Vec<(CoverGroup, CoverSpan)>>>) -> bool {
    let unsolved = constraints.iter().find(|c| c.is_some()).is_some();
    if !unsolved {
        return true;
    }

    let most_constrained = constraints
        .iter()
        .min_by_key(|c| c.as_ref().map(|v| v.len()).unwrap_or(255))
        .unwrap()
        .as_ref()
        .unwrap();

    if most_constrained.len() == 0 {
        return false;
    }

    for (cover_group, cover_span) in most_constrained {
        let next_constraints = constraints
            .iter()
            .enumerate()
            .map(|(i, c)| match c {
                None => None,
                Some(vs) => {
                    if i >= cover_span.0 && i < cover_span.1 {
                        None
                    } else {
                        Some(
                            vs.iter()
                                .filter(|(vtag, vrange)| {
                                    *vtag != *cover_group
                                        && !(vrange.1 > cover_span.0 && vrange.0 < cover_span.1)
                                })
                                .copied()
                                .collect_vec(),
                        )
                    }
                }
            })
            .collect_vec();
        if propagate(&next_constraints) {
            return true;
        }
    }
    false
}

pub(crate) fn evaluate_covers(covers: &Vec<(CoverGroup, CoverSpan)>, places: usize) -> bool {
    let constraints = (0..places)
        .map(|i| {
            Some(
                covers
                    .iter()
                    .filter(|(_, (a, b))| i >= *a && i < *b)
                    .copied()
                    .collect(),
            )
        })
        .collect();
    propagate(&constraints)
}

#[test]
fn propagate_test() {
    let covers = vec![
        (100, (0, 4)),
        (100, (0, 6)),
        (100, (0, 2)),
        (107, (3, 5)),
        (102, (0, 3)),
        (102, (0, 4)),
        (102, (4, 6)),
        (103, (5, 7)),
    ];

    let t0 = SystemTime::now();
    println!("Prop: {}", evaluate_covers(&covers, 7));
    println!("{:?}", t0.elapsed());
}
