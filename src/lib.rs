use dict::{SUBSTRINGS, WORDS};
use wasm_bindgen::prelude::*;

pub mod dict;
pub mod examples;
pub mod parser;

#[macro_use]
extern crate lazy_static;

use itertools::Itertools;

use std::ops::Range;
use std::{
    fmt::Write as FmtWrite,
    ops::{Index, IndexMut},
};

use Constraint::*;

#[derive(Eq, PartialEq, Clone, Debug, Hash, Ord, PartialOrd)]
pub enum Constraint {
    Star,
    Literal(char),
    LiteralFrom(Vec<char>),
    Anagram(bool, Vec<Constraint>),
    Variable(char),
    Disjunction((Vec<Constraint>, Vec<Constraint>)),
    Conjunction((Vec<Constraint>, Vec<Constraint>)),
    Subpattern(Vec<Constraint>),
}

use rustc_hash::FxHashSet;

use std::str;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableMap<T>(pub [T; 26]);

impl<T> Default for VariableMap<T>
where
    T: Default + Copy,
{
    fn default() -> Self {
        Self([T::default(); 26])
    }
}

impl<T> Index<char> for VariableMap<T> {
    type Output = T;

    fn index(&self, index: char) -> &Self::Output {
        self.0.get(index as usize - 65).unwrap()
    }
}

impl<T> IndexMut<char> for VariableMap<T> {
    fn index_mut(&mut self, index: char) -> &mut Self::Output {
        self.0.get_mut(index as usize - 65).unwrap()
    }
}

fn length_range(
    p: &Constraint,
    bindings: &VariableMap<Option<&str>>,
    spec: &VariableMap<(usize, usize)>,
) -> (usize, usize) {
    match p {
        Literal(_) | LiteralFrom(_) => (1, 1),
        Anagram(open, v) => v
            .iter()
            .map(|i| length_range(i, bindings, spec))
            .fold((0, if *open { 255 } else { 0 }), |acc, v| {
                (acc.0 + v.0, acc.1 + v.1)
            }),
        &Variable(v) => {
            if let Some(b) = bindings[v] {
                (b.len(), b.len())
            } else {
                (spec[v].0, spec[v].1)
            }
        }
        Subpattern(v) => v
            .iter()
            .map(|i| length_range(i, bindings, spec))
            .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1)),

        Disjunction((a, b)) => {
            let arange = a
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            let brange = b
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            (arange.0.min(brange.0), arange.1.max(brange.1))
        }
        Conjunction((a, b)) => {
            let arange = a
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            let brange = b
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            (arange.0.max(brange.0), arange.1.min(brange.1))
        }
        Star => (0, 255),
    }
}

#[derive(Clone, Copy, Debug)]
enum Branch {
    Left,
    Right,
    Straight,
    AnagramIndex(usize),
}

#[derive(Clone, Debug)]
struct PatternIndex {
    layer: usize,
    path: Vec<(usize, Branch)>, // TODO decide on smallvec strategy
    bound: Range<usize>,
}

impl PatternIndex {
    fn index(&self, index: Range<usize>) -> PatternIndex {
        PatternIndex {
            layer: self.layer,
            path: self.path.clone(),
            bound: self.bound.start + index.start
                ..(self.bound.start + index.end).min(self.bound.end),
        }
    }

    fn step_in(&self, index: usize, branch: Branch, len: usize) -> PatternIndex {
        PatternIndex {
            layer: self.layer,
            path: self
                .path
                .iter()
                .cloned()
                .chain(std::iter::once((self.bound.start + index, branch)))
                .collect(),
            bound: 0..len,
        }
    }
}

pub struct ExecutionContext<'a, 'b> {
    candidate_stack: Vec<&'a str>,
    candidate_stack_goal: usize,
    bindings: VariableMap<Option<&'a str>>,
    patterns: Vec<Vec<Constraint>>,
    spec_var_length: VariableMap<(usize, usize)>,
    spec_var_inequality: Vec<Vec<char>>,
    spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    active: bool,
    count: usize,
    // sum: usize,
    subexpr_pattern_stack: Vec<(&'a str, PatternIndex, bool)>, // replace with a simple int index
    config_no_binding_in_subexpressions: bool,
    callback: Option<&'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<&'a str>>) -> bool>,
}

impl<'a, 'b> Index<&PatternIndex> for ExecutionContext<'a, 'b> {
    type Output = [Constraint];
    fn index(&self, index: &PatternIndex) -> &Self::Output {
        &index.path.iter().fold(
            &self.patterns[index.layer][..],
            |acc, (i, direction)| match acc[*i] {
                Conjunction((ref l, ref r)) | Disjunction((ref l, ref r)) => match direction {
                    Branch::Left => &l[..],
                    Branch::Right => &r[..],
                    _ => panic!("Invalid branch direction"),
                },
                Subpattern(ref l) => &l[..],
                Anagram(idx, ref v) => match direction {
                    Branch::AnagramIndex(idx) => &v[*idx..*idx + 1],
                    _ => panic!("Invalid branch direction"),
                },
                _ => panic!("Invalid path {:?} in PatternIndex", acc[*i]),
            },
        )[index.bound.clone()]
    }
}

impl<'a, 'b, 'c> ExecutionContext<'a, 'b> {
    fn new(
        patterns: Vec<Vec<Constraint>>,
        spec_var_length: VariableMap<(usize, usize)>,
        spec_var_inequality: Vec<Vec<char>>,
        spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    ) -> Self {
        ExecutionContext {
            candidate_stack: vec![],
            candidate_stack_goal: patterns.len(),
            bindings: VariableMap::default(),
            patterns,
            spec_var_length,
            spec_var_inequality,
            spec_var_sets_length,
            active: true,
            subexpr_pattern_stack: vec![],
            callback: None,
            count: 0,
            config_no_binding_in_subexpressions: true, // sum: 0,
        }
    }

    fn nested_constraints_execute(&'c mut self, candidate: &'a str, pattern_idx: PatternIndex) {
        let pattern_len = pattern_idx.bound.len();
        // for i in 0..self.subexpr_pattern_stack.len() {
        //     print!("  ");
        // }
        // println!(
        //     "Patt {:?} cand {} in {:?}", &self[&pattern_idx], candidate, self.subexpr_pattern_stack
        // );

        if candidate.len() == 0
            && pattern_len == 0
            && self.candidate_stack_goal == self.candidate_stack.len()
            && self.subexpr_pattern_stack.len() == 0
        {
            self.active = (self.callback.as_mut().unwrap())(&self.candidate_stack, &self.bindings);
            self.count += 1;
            return;
        }

        if candidate.len() == 0 && pattern_len == 0 && self.subexpr_pattern_stack.len() > 0 {
            // println!("Succese D{:?}", self.subexpr_pattern_stack);
            let (next_candidate, next_pattern, capped) = self.subexpr_pattern_stack.pop().unwrap();
            if !capped {
                self.nested_constraints_execute(next_candidate, next_pattern.clone());
            } else {
                self.count += 1; //TODO use a self.cap_hit
            }
            self.subexpr_pattern_stack
                .push((next_candidate, next_pattern, capped));
            return;
        }

        if pattern_len == 0 {
            if candidate.len() > 0 {
                return;
            }
            if self.subexpr_pattern_stack.len() > 0 {
                return;
            }

            let failed: FxHashSet<usize> = FxHashSet::default();
            let next_pattern = self.patterns.get(self.candidate_stack.len()).unwrap();
            let next_candidates: Box<dyn Iterator<Item = &&str>>;
            let mut started = false;
            let mut in_streak = true;
            let mut probe = "".to_string();
            let mut probes: Vec<String> = vec![];
            for c in next_pattern {
                let now_in_streak = match c {
                    Literal(_) | Variable(_) => true,
                    _ => false,
                } && {
                    //TODO move length_range into the executino context?
                    let range = length_range(&c, &self.bindings, &self.spec_var_length);
                    range.0 == range.1
                };

                if !now_in_streak && in_streak && probe != "" {
                    probes.push(probe.clone());
                    probe.clear();
                }

                in_streak = now_in_streak;

                if in_streak {
                    if !started {
                        probe.push('^');
                    }

                    match c {
                        Literal(c) => {
                            probe.push(*c);
                        }
                        Variable(v) => match self.bindings[*v] {
                            Some(bound) => {
                                probe += bound;
                            }
                            None => {
                                probe.clear();
                                in_streak = false;
                            }
                        },
                        _ => panic!("Can't find specific streak string"),
                    };
                }
                started = true;
            }

            if in_streak && probe != "" {
                probe.push('$');
                probes.push(probe);
            }

            // println!("Check next patt {:?}", probes);
            let empty: Option<FxHashSet<usize>> = None;
            let ss = probes
                .into_iter()
                .map(|p| SUBSTRINGS.get(&p).unwrap_or(&failed))
                .fold(empty, |acc, s| {
                    Some(match acc {
                        None => s.clone(),
                        Some(acc) => acc.intersection(&s).into_iter().copied().collect(),
                    })
                })
                .map(|w| w.into_iter().map(|k| &WORDS[k]));

            if ss.is_some() {
                next_candidates = Box::new(ss.unwrap().into_iter());
            } else {
                next_candidates = Box::new(WORDS.iter());
            }

            return self.execute_pattern(next_candidates);
        }

        let mut constraint_index = 0usize;
        let mut anchored_left = true;
        let mut streak_start = 0;
        // println!("NEXT {}  {:?} Get ready to subp {} recurse {:?}", candidate, self.patterns, constraint_index, pattern_idx);
        let mut streak_length_bound = length_range(
            &self[&pattern_idx].iter().next().unwrap(),
            &self.bindings,
            &self.spec_var_length,
        );

        if streak_length_bound.0 != streak_length_bound.1 {
            let right_anchor = length_range(
                &self[&pattern_idx].iter().last().unwrap(),
                &self.bindings,
                &self.spec_var_length,
            );
            if right_anchor.0 == right_anchor.1 {
                streak_length_bound = right_anchor;
                anchored_left = false;
                constraint_index = pattern_len.saturating_sub(1);
                streak_start = candidate.len().saturating_sub(streak_length_bound.0);
            }
        }
        streak_length_bound.1 = streak_length_bound.1.min(candidate.len());
        if candidate.len() < streak_length_bound.0 {
            return;
        }
        if pattern_len == 1 && streak_length_bound.1 < candidate.len() {
            return;
        }

        // println!("{:?}, {:?} {:?}", candidate, streak_length_bound, &self[&pattern_idx]);
        'streaks: for streak_len in (streak_length_bound.0..=streak_length_bound.1).rev() {
            if !self.active {
                return;
            }
            let streak_end = (streak_start + streak_len).min(candidate.len());
            let remainder_len = candidate.len().saturating_sub(streak_len);
            let remainder_start = if anchored_left { streak_end } else { 0 };
            let remainder_end = remainder_start + remainder_len;

            // println!("Matching {:?}", &pattern[constraint_index]);
            match &self[&pattern_idx][constraint_index] {
                Subpattern(v) => {
                    let vlen = v.len();
                    let v = ();

                    self.subexpr_pattern_stack.push((
                        &candidate[remainder_start..remainder_end],
                        pattern_idx.index(if anchored_left {
                            1..pattern_len
                        } else {
                            0..pattern_len - 1
                        }),
                        false,
                    ));
                    let sub_candidate = &candidate[streak_start..streak_end];
                    // println!("{}  {:?} Get ready to subp {} recurse {:?}", candidate, self.patterns, constraint_index, pattern_idx);
                    self.nested_constraints_execute(
                        sub_candidate,
                        pattern_idx.step_in(constraint_index, Branch::Straight, vlen),
                    );
                    self.subexpr_pattern_stack.pop();
                }
                Star => {
                    self.nested_constraints_execute(
                        &candidate[remainder_start..remainder_end],
                        pattern_idx.index(if anchored_left {
                            1..pattern_len
                        } else {
                            0..pattern_len - 1
                        }),
                    );
                }

                Disjunction((b, a)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];
                    // for i in 0..self.subexpr_pattern_stack.len() {
                    //     print!("  ");
                    // }
                    // println!("DISJ {} onm {} P{:?} curpat\n  {:?}\n  {:?}", streak_len, candidate, self.subexpr_pattern_stack, a,b);
                    let count_pre = self.count;
                    let a = a.len();
                    let b = b.len();

                    if streak_len > 0 || pattern_len == 1 {
                        self.subexpr_pattern_stack.push((
                            &candidate[remainder_start..remainder_end],
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                            false,
                        ));
                    }

                    let a_pattern = pattern_idx.step_in(constraint_index, Branch::Right, a);
                    self.nested_constraints_execute(sub_candidate, a_pattern);

                    if self.config_no_binding_in_subexpressions && self.count > count_pre {
                        self.subexpr_pattern_stack.pop();
                        return;
                    }

                    let b_pattern = pattern_idx.step_in(constraint_index, Branch::Left, b);
                    self.nested_constraints_execute(sub_candidate, b_pattern);

                    if streak_len > 0 || pattern_len == 1 {
                        self.subexpr_pattern_stack.pop();
                    }
                }

                Conjunction((b, a)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];

                    let a = a.len();
                    let b = b.len();

                    let later_constraints_exist = streak_len > 0 || pattern_len == 1;
                    if later_constraints_exist {
                        self.subexpr_pattern_stack.push((
                            &candidate[remainder_start..remainder_end],
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                            false,
                        ));
                    }
                    let a_pattern = pattern_idx.step_in(constraint_index, Branch::Right, a);
                    let b_pattern = pattern_idx.step_in(constraint_index, Branch::Left, b);
                    self.subexpr_pattern_stack
                        .push((sub_candidate, b_pattern, false));
                    self.nested_constraints_execute(sub_candidate, a_pattern);
                    self.subexpr_pattern_stack.pop();

                    if later_constraints_exist {
                        self.subexpr_pattern_stack.pop();
                    }
                }
                Literal(v) => {
                    if candidate[streak_start..streak_end].starts_with(*v) {
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                        );
                    }
                }
                LiteralFrom(v) => {
                    if v.contains(&candidate[streak_start..streak_end].chars().next().unwrap()) {
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                        );
                    }
                }
                Variable(v) => {
                    let v = *v;
                    // println!("Check {:?} var {} {:?} {:?} seg {:?}", pattern, v, self.bindings, candidate, &candidate[streak_start..streak_end]);
                    let reset_var = match self.bindings[v] {
                        Some(val) if val == &candidate[streak_start..streak_end] => Some(val),
                        None => {
                            self.bindings[v] = Some(&candidate[streak_start..streak_end]);
                            None
                        }
                        _ => continue 'streaks,
                    };

                    /* !=AB */
                    for var_distinct_constraint in &self.spec_var_inequality {
                        for (&a, &b) in var_distinct_constraint.into_iter().tuple_combinations() {
                            if self.bindings[a].is_some() && self.bindings[a] == self.bindings[b] {
                                self.bindings[v] = reset_var;
                                continue 'streaks;
                            }
                        }
                    }

                    /* |ABC|=3-5 */
                    for (vset, a, b) in &self.spec_var_sets_length {
                        let (bound_count, bound_sum): (usize, usize) = vset
                            .iter()
                            .map(|v| self.bindings[*v])
                            .filter_map(|b| b.map(|v| v.len()))
                            .fold((0, 0), |acc, v| (acc.0 + 1, acc.1 + v));
                        if bound_sum > *b
                            || (bound_count == vset.len() && (bound_sum < *a || bound_sum > *b))
                        {
                            self.bindings[v] = reset_var;
                            continue 'streaks;
                        }
                    }
                    self.nested_constraints_execute(
                        &candidate[remainder_start..remainder_end],
                        pattern_idx.index(if anchored_left {
                            1..pattern_len
                        } else {
                            0..pattern_len - 1
                        }),
                    );

                    self.bindings[v] = reset_var;
                }
                Anagram(open, fodder) => {
                    let open = *open; // shadow bindings to &self so we can recurse in this block
                    let fodder = fodder.clone();
                    let sub_candidate = &candidate[streak_start..streak_end];

                    let (fixed_len, _variable_len): (Vec<_>, Vec<_>) = fodder
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(i, c)| {
                            let len = length_range(&c, &self.bindings, &self.spec_var_length);
                            (c, i, len)
                        })
                        .partition(|(_c, _i, flen)| flen.0 == flen.1);

                    let mut last_c = &Star;
                    let mut last_c_index = 255;
                    'flc: for (flc, i, (a, _b)) in fixed_len.iter().sorted() {
                        for flc_start in 0..sub_candidate.len() {
                            if last_c == flc && flc_start <= last_c_index {
                                continue;
                            }
                            if flc_start + a > sub_candidate.len() {
                                continue 'streaks;
                            }
                            let next_pattern_idx =
                                pattern_idx.step_in(constraint_index, Branch::AnagramIndex(*i), 1);

                            self.subexpr_pattern_stack.push((
                                sub_candidate,
                                PatternIndex {
                                    bound: 0..0,
                                    layer: 0,
                                    path: vec![],
                                },
                                true,
                            ));
                            let pc = self.count;
                            self.nested_constraints_execute(
                                &sub_candidate[flc_start..flc_start + a],
                                next_pattern_idx,
                            );
                            self.subexpr_pattern_stack.pop();
                            if self.count > pc {
                                last_c = &flc;
                                last_c_index = flc_start;
                                continue 'flc;
                            }
                        }
                        continue 'streaks;
                    }

                    // println!("************PIECES EXIST {}", sub_candidate);
                        // continue 'streaks;

                    let later_constraints_exist = streak_len > 0 || pattern_len == 1;
                    if later_constraints_exist {
                        self.subexpr_pattern_stack.push((
                            &candidate[remainder_start..remainder_end],
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                            false,
                        ));
                    }

                    for fodder_order in fodder.iter().permutations(fodder.len()) {
                        let mut sub_pattern = fodder_order
                            .into_iter()
                            .cloned()
                            .flat_map(|c| {
                                if open {
                                    vec![Star, c].into_iter()
                                } else {
                                    vec![c].into_iter()
                                }
                            })
                            .collect_vec();
                        if open {
                            sub_pattern.push(Star);
                        }

                        let sub_pattern_len = sub_pattern.len();
                        self.patterns.push(sub_pattern);
                        self.nested_constraints_execute(
                            sub_candidate,
                            PatternIndex {
                                bound: 0..sub_pattern_len,
                                layer: self.patterns.len() - 1,
                                path: vec![],
                            },
                        );

                        self.patterns.pop();
                    }

                    if later_constraints_exist {
                        self.subexpr_pattern_stack.pop();
                    }
                }
            }
        }
    }

    pub fn execute_pattern<T>(&'c mut self, candidates: impl Iterator<Item = T>)
    where
        T: std::ops::Deref<Target = &'a str>,
    {
        let pattern_depth = self.candidate_stack.len();
        for c in candidates {
            self.candidate_stack.push(&*c);
            // println!("Try next {:?}. {:?}", self.candidate_stack, self.subexpr_pattern_stack);
            self.nested_constraints_execute(
                *c,
                PatternIndex {
                    layer: pattern_depth,
                    path: vec![],
                    bound: 0..self.patterns[pattern_depth].len(),
                },
            );
            self.candidate_stack.pop();
            if !self.active {
                return;
            }
        }
    }

    pub fn execute<T>(
        &'c mut self,
        candidates: impl Iterator<Item = T>,
        callback: &'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<&'a str>>) -> bool,
    ) where
        T: std::ops::Deref<Target = &'a str>,
    {
        self.callback = Some(callback);
        self.execute_pattern(candidates);
        self.callback = None;
    }
}

#[wasm_bindgen]
pub fn q(query: &str) -> String {
    let _wc = SUBSTRINGS.len(); // trigger lazy static
    let mut results = vec![];

    let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<&str>>| {
        println!(
            "Bound sol'n {:?} @{:?}",
            a,
            b.0.iter().cloned().filter_map(|v| v).collect_vec()
        );
        results.push(a.iter().map(|v| v.to_string()).collect_vec());
        true
    };

    parser::parser_exec(&query).execute(WORDS.iter(), &mut cb);

    let mut s = String::new();

    write!(&mut s, "[").unwrap();
    for (i, r) in results.iter().enumerate() {
        let rep = r.iter().join(";");
        write!(&mut s, "{:?}", rep).unwrap();
        if i < results.len() - 1 {
            writeln!(&mut s, ",").unwrap();
        }
    }
    write!(&mut s, "]").unwrap();
    s
}
