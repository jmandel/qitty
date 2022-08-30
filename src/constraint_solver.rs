use itertools::Itertools;
use rustc_hash::FxHashMap;
use serde::__private::de;
use std::{env, time::SystemTime};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ConstraintStatus {
    Satisfied,
    MaySatisfy,
    NeedToSatisfy,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChoiceStatus {
    Available,
    Unavailable,
    Chosen,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Tag(pub usize, pub Option<char>);
type Span = (usize, usize);
fn propagate<'a>(
    candidate: &'a str,
    bindings: &mut FxHashMap<char, &'a str>,
    places: &mut Vec<(
        ConstraintStatus,
        usize, /* # choices cached*/
        Vec<(
            ChoiceStatus,
            Tag,
            Span,
            usize, /* constrained at depth */
        )>,
    )>,
    tags: &mut Vec<(ConstraintStatus, Tag)>,
    tag_requirement: ConstraintStatus,
    place_requirement: ConstraintStatus,
    depth: usize,
) -> bool {
    // println!(
    //     "{} @depth {} --\n{:?},\np{} {:?},\n{:?},\n",
    //     candidate, depth, bindings, places.len(), places, tags
    // );
    if places
        .iter()
        .all(|p| p.0 != ConstraintStatus::NeedToSatisfy)
        && tags.iter().all(|p| p.0 != ConstraintStatus::NeedToSatisfy)
    {
        // println!("All satisfie");
        return true;
    }

    let mut most_constrained_tag: &  Vec<(
            ChoiceStatus,
            Tag,
            Span,
            usize, /* constrained at depth */
        )>;
    let mut choices: Vec<_> = vec![];
    let mut most_constrained_place = usize::MAX;
    if place_requirement == ConstraintStatus::NeedToSatisfy {
        let most_constrained_place_search = places
            .iter()
            .enumerate()
            .filter(|(i, p)| p.0 != ConstraintStatus::Satisfied)
            .min_by_key(|(i, p)| p.1);

        if most_constrained_place_search.is_none() && place_requirement == ConstraintStatus::NeedToSatisfy {
            // println!("most constraine dplce has no optiosn");
            return false;
        }
        let (res, (_, _, c)) = most_constrained_place_search.unwrap();
        most_constrained_place = res;
        choices = c
        .into_iter()
        .filter(|c| c.0 == ChoiceStatus::Available)
        .unique_by(|c| match c.1 {
            Tag(_id, Some(c)) => 1 << 10 + c as usize,
            Tag(id, None) => id,
        })
        .cloned()
        .collect_vec();

    } else {
        let most_constrained_tag_search = tags.iter().filter(|t|t.0 == ConstraintStatus::NeedToSatisfy).next();
        if most_constrained_tag_search.is_none() {
            // println!("Failed, most constrained tag has n options");
            return false;
        }
        let most_constrained_tag = most_constrained_tag_search.unwrap().1;
        choices =places.iter().flat_map(|p|p.2.iter().filter(|c|c.1 == most_constrained_tag && c.0 == ChoiceStatus::Available)).copied().collect_vec();
    // println!("initia lcohicse {:?}", choices);
    }
    // println!("Chicopes {:?}", choices);

    for (_status, selected_tag, selected_span, _) in choices {
        let mut binding_to_restore = None;
        // println!("Pick {:?} at {:?}", selected_tag, selected_span);

        if let Some(v) = selected_tag.1 {
            let bound_to = &candidate[selected_span.0..selected_span.1];
            binding_to_restore = Some((v, bindings.get(&v).copied()));
            if bindings
                .get(&v)
                .map(|bound| bound != &&candidate[selected_span.0..selected_span.1])
                .unwrap_or(false)
            {
                // println!("Binding fialed");
                continue;
            }
            bindings.insert(v, bound_to);
        }

        let mut places_modified = 0usize;
        let mut tags_modified = 0usize;

        for (i, p) in places.iter_mut().enumerate() {
            if p.0 == ConstraintStatus::Satisfied {
                continue;
            }

            if i == most_constrained_place {
                p.0 = ConstraintStatus::Satisfied;
                continue;
            }

            if i >= selected_span.0 && i < selected_span.1 {
                p.0 = ConstraintStatus::Satisfied;
                places_modified |= 1 << i;
            }

            for t in p.2.iter_mut() {
                if t.0 == ChoiceStatus::Available
                    && (t.2 .1 > selected_span.0 && t.2 .0 < selected_span.1 || t.1 == selected_tag)
                {
                    places_modified |= 1 << i;
                    t.0 = ChoiceStatus::Unavailable;
                    t.3 = depth;
                    p.1 -= 1;
                }
            }
        }

        for (i, t) in tags.iter_mut().enumerate() {
            if t.0 == ConstraintStatus::Satisfied {
                continue;
            }

            if t.1 == selected_tag {
                t.0 = ConstraintStatus::Satisfied;
                tags_modified |= 1 << i;
            }
        }

        let success = propagate(
            candidate,
            bindings,
            places,
            tags,
            tag_requirement,
            place_requirement,
            depth + 1,
        );

        for (i, p) in places.iter_mut().enumerate() {
            if i == most_constrained_place {
                p.0 = place_requirement;
                continue;
            }
            if places_modified & 1 << i > 0 {
                p.0 = place_requirement;
                for t in p.2.iter_mut() {
                    if t.3 == depth {
                        t.0 = ChoiceStatus::Available;
                        t.3 = 0;
                        p.1 += 1;
                    }
                }
            }
        }

        for (i, p) in tags.iter_mut().enumerate() {
            if tags_modified & 1 << i > 0 {
                p.0 = tag_requirement;
            }
        }

        match binding_to_restore {
            Some((v, None)) => {
                bindings.remove(&v);
            }
            Some((v, Some(s))) => {
                bindings.insert(v, s);
            }
            _ => {}
        };

        if success {
            // println!("Recursive acll suscceede");
            return true;
        }
    }
    // println!("Exhaused all choices, popping up");
    false
}

pub(crate) fn evaluate_covers(
    candidate: &str,
    covers: &Vec<(Tag, Span)>,
    places: usize,
    use_all_tags: bool,
    use_all_places: bool,
) -> bool {
    if !use_all_places && !use_all_tags {
        unreachable!("Need to use all places and/or all tags.");
    }
    let tag_requirement = if use_all_tags {
        ConstraintStatus::NeedToSatisfy
    } else {
        ConstraintStatus::MaySatisfy
    };
    let place_requirement = if use_all_places {
        ConstraintStatus::NeedToSatisfy
    } else {
        ConstraintStatus::MaySatisfy
    };
    let tags = &mut covers
        .iter()
        .map(|c| c.0)
        .unique()
        .map(|c| {
            (tag_requirement, c) // fix the tag
        })
        .collect_vec();

    propagate(
        candidate,
        &mut FxHashMap::default(),
        &mut (0..places)
            .map(|i| {
                let cover_choices = covers
                    .iter()
                    .filter(|c| i >= c.1 .0 && i < c.1 .1)
                    .map(|c| (ChoiceStatus::Available, c.0, c.1, 0))
                    .collect_vec();
                (
                    ConstraintStatus::NeedToSatisfy,
                    cover_choices.len(),
                    cover_choices,
                )
            })
            .collect(),
        tags,
        tag_requirement,
        place_requirement,
        1,
    )
}

#[test]
fn propagate_test() {
    let covers = vec![
        (Tag(100, None), (0, 4)),
        (Tag(100, None), (0, 6)),
        (Tag(100, None), (0, 2)),
        (Tag(100, None), (3, 5)),
        (Tag(100, None), (0, 3)),
        (Tag(100, None), (0, 4)),
        (Tag(100, None), (4, 6)),
        (Tag(100, None), (5, 7)),
    ];

    let t0 = SystemTime::now();
    println!(
        "Prop: {}",
        evaluate_covers("abcdefg", &covers, 7, false, true)
    );
    println!("{:?}", t0.elapsed());
}
