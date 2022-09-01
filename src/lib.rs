use constraint_solver::Tag;
use dict::{SUBSTRINGS, WORDS};
use wasm_bindgen::prelude::*;

pub mod constraint_solver;
pub mod dict;
pub mod examples;
pub mod parser;

#[macro_use]
extern crate lazy_static;

use itertools::Itertools;

use std::collections::HashMap;
use std::ops::Range;
use std::ops::{Index, IndexMut};

use Constraint::*;

#[derive(Eq, PartialEq, Clone, Debug, Hash, Ord, PartialOrd)]
pub enum WordDirection {
    Forwards,
    Backwards,
}

#[derive(Eq, PartialEq, Clone, Debug, Hash, Ord, PartialOrd)]
pub enum Constraint {
    Star,
    Literal(char),
    LiteralFrom(Vec<char>),
    Anagram(bool /* match all places */, bool /* match all tags */, Vec<Constraint>),
    Variable(char),
    Disjunction((Vec<Constraint>, Vec<Constraint>)),
    Conjunction((Vec<Constraint>, Vec<Constraint>)),
    Subpattern(Vec<Constraint>),
    Word(WordDirection),
    Reverse(Vec<Constraint>),
    Negate(Vec<Constraint>),
}

use rustc_hash::FxHashSet;

use std::str;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableMap<T>(pub [T; 26]);

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct CharCounter(pub [usize; 26]);

impl Default for VariableMap<(usize, usize)> {
    fn default() -> Self {
        Self([
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
            (0, 0),
        ])
    }
}

impl<T> Default for VariableMap<Option<T>> {
    fn default() -> Self {
        Self([
            None, None, None, None, None, None, None, None, None, None, None, None, None, None,
            None, None, None, None, None, None, None, None, None, None, None, None,
        ])
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

impl Index<char> for CharCounter {
    type Output = usize;
    fn index(&self, index: char) -> &Self::Output {
        self.0.get(index as usize - 97).unwrap()
    }
}

impl IndexMut<char> for CharCounter {
    fn index_mut(&mut self, index: char) -> &mut Self::Output {
        self.0.get_mut(index as usize - 97).unwrap()
    }
}


fn letters_possible_recurse(p: &Constraint, cc: &mut CharCounter) {
    match p {
        Literal(c)  => {cc[*c] += 1;}
        LiteralFrom(vs) => {
            for c in vs {
                cc[*c] += 1;
            }
        }
        Anagram(false, _, _) | Variable(_) | Word(_) | Star | Negate(_)=> {
            for c in 'a'..='z' {
                cc[c] = 255;
            }
        }
        Anagram(true, _, v) | Subpattern(v) | Reverse(v)=> {
            for c in v {
                letters_possible_recurse(c, cc);
            }
        }
        Disjunction((a, b)) | Conjunction((a, b)) => {
            for c in a.iter().chain(b.iter()) {
                letters_possible_recurse(c, cc);
            }
        }

    }

}
fn letters_possible(p: &Constraint) -> CharCounter {
    let mut cc = CharCounter::default();
    letters_possible_recurse(p, &mut cc);
    cc
}
fn length_range(
    p: &Constraint,
    bindings: &VariableMap<Option<String>>,
    spec: &VariableMap<(usize, usize)>,
) -> (usize, usize) {
    match p {
        Literal(_) | LiteralFrom(_) => (1, 1),
        Anagram(use_all_tags, use_all_places, v) => v
            .iter()
            .map(|i| length_range(i, bindings, spec))
            .fold((0, if *use_all_tags { 0 } else { 255 }), |acc, v| {
                (acc.0 + if *use_all_places {v.0} else {0}, acc.1 + v.1)
            }),
        &Variable(v) => {
            if let Some(b) = &bindings[v] {
                (b.len(), b.len())
            } else {
                (spec[v].0, spec[v].1)
            }
        }
        Subpattern(v) | Reverse(v) => v
            .iter()
            .map(|i| length_range(i, bindings, spec))
            .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1)),

        Disjunction((a, b)) | Conjunction((a, b)) => {
            let arange = a
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            let brange = b
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            match p {
                Disjunction(_) => (arange.0.min(brange.0), arange.1.max(brange.1)),
                _ => (arange.0.max(brange.0), arange.1.min(brange.1)),
            }
        }
        Word(_dir) => (2, 255),
        Star | Negate(_) => (0, 255),
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
    length_range: (usize, usize),
}

impl Default for PatternIndex {
    fn default() -> Self {
        Self {
            layer: Default::default(),
            path: Default::default(),
            bound: Default::default(),
            length_range: (0, 255),
        }
    }
}

impl PatternIndex {
    fn index(&self, index: Range<usize>) -> PatternIndex {
        PatternIndex {
            layer: self.layer,
            path: self.path.clone(),
            bound: self.bound.start + index.start
                ..(self.bound.start + index.end).min(self.bound.end),
            length_range: (0, 255),
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
            length_range: (0, 255),
        }
    }
}

pub struct ExecutionContext<'a, 'b> {
    candidate_stack: Vec<&'a str>,
    candidate_stack_goal: usize,
    bindings: VariableMap<Option<String>>,
    patterns: Vec<ConstrainedPattern>,
    spec_var_length: VariableMap<(usize, usize)>,
    spec_var_inequality: Vec<Vec<char>>,
    spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    active: bool,
    count: usize,
    subexpr_pattern_stack: Vec<(String, PatternIndex, Option<usize>)>, // replace with a simple int index
    config_no_binding_in_subexpressions: bool,
    callback: Option<&'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<String>>) -> bool>,
}

impl<'a, 'b> Index<&PatternIndex> for ExecutionContext<'a, 'b> {
    type Output = [Constraint];
    fn index(&self, index: &PatternIndex) -> &Self::Output {
        &index.path.iter().fold(
            &self.patterns[index.layer].1[..],
            |acc, (i, direction)| match acc[*i] {
                Conjunction((ref l, ref r)) | Disjunction((ref l, ref r)) => match direction {
                    Branch::Left => &l[..],
                    Branch::Right => &r[..],
                    _ => unreachable!("Invalid branch direction"),
                },
                Subpattern(ref l) | Reverse(ref l) | Negate(ref l) => &l[..],
                Anagram(_, _, ref v) => match direction {
                    Branch::AnagramIndex(idx) => &v[*idx..*idx + 1],
                    _ => unreachable!("Invalid branch direction"),
                },
                _ => unreachable!("Invalid path {:?} in PatternIndex", acc[*i]),
            },
        )[index.bound.clone()]
    }
}
type ConstrainedPattern = ((usize, usize), Vec<Constraint>);
impl<'a, 'b, 'c> ExecutionContext<'a, 'b> {
    fn new(
        patterns: Vec<ConstrainedPattern>,
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

    fn solve_equation(&mut self) {
        if self.subexpr_pattern_stack.len() > 0 {
            return;
        }
        // println!("Solve {:?}", self.candidate_stack);
        let failed: FxHashSet<usize> = FxHashSet::default();
        let next_pattern = self.patterns.get(self.candidate_stack.len()).unwrap();
        let next_candidates: Box<dyn Iterator<Item = &&str>>;
        let mut started = false;
        let mut in_streak = true;
        let mut probe = "".to_string();
        let mut probes: Vec<String> = vec![];
        for c in &next_pattern.1 {
            let now_in_streak = match c {
                Literal(_) | Variable(_) | Reverse(_) => true,
                _ => false,
            } && {
                //TODO move length_range into the executino context?
                let range = length_range(&c, &self.bindings, &self.spec_var_length);
                range.0 == range.1
            };

            if !now_in_streak && in_streak {
                if probe != "" && probe != "^" {
                    probes.push(probe.clone());
                }
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
                    // TODO recursive function where Reverse calls on sub-logic
                    // instead of paritally/poorly recapitulating the logic for Variable
                    Reverse(vs) if vs.len() == 1 => match &vs[0] {
                        Variable(v) => match &self.bindings[*v] {
                            Some(bound) => probe += &bound[..].chars().rev().collect::<String>(),
                            None => {
                                probe.clear();
                                in_streak = false;
                            }
                        },
                        _ => panic!(),
                    },
                    Variable(v) => match &self.bindings[*v] {
                        Some(bound) => {
                            probe += &bound;
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

    fn nested_constraints_execute(&'c mut self, candidate: &str, pattern_idx: PatternIndex) {
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
            let (next_candidate, next_pattern, mut capped) =
                self.subexpr_pattern_stack.pop().unwrap();
            if capped.is_none() {
                self.nested_constraints_execute(&next_candidate, next_pattern.clone());
            } else {
                capped = capped.map(|c| c + 1);
            }
            self.subexpr_pattern_stack
                .push((next_candidate.clone(), next_pattern, capped));
            return;
        }

        if pattern_len == 0 {
            if candidate.len() == 0 {
                self.solve_equation();
            }
            return;
        }

        let pattern_range_constraint = pattern_idx.length_range;
        if candidate.len() < pattern_range_constraint.0
            || candidate.len() > pattern_range_constraint.1
        {
            return;
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
        // println!("SLB {:?}", streak_length_bound);

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

        if pattern_len == 1 && streak_length_bound.0 < candidate.len() {
            streak_length_bound.0 = candidate.len();
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

            match &self[&pattern_idx][constraint_index] {
                Subpattern(v) => {
                    let vlen = v.len();

                    self.subexpr_pattern_stack.push((
                        candidate[remainder_start..remainder_end].to_string(),
                        pattern_idx.index(if anchored_left {
                            1..pattern_len
                        } else {
                            0..pattern_len - 1
                        }),
                        None,
                    ));
                    let sub_candidate = &candidate[streak_start..streak_end];
                    // println!("{}  {:?} Get ready to subp {} recurse {:?}", candidate, self.patterns, constraint_index, pattern_idx);
                    self.nested_constraints_execute(
                        sub_candidate,
                        pattern_idx.step_in(constraint_index, Branch::Straight, vlen),
                    );
                    self.subexpr_pattern_stack.pop();
                }
                Reverse(v) => {
                    let vlen = v.len();
                    self.subexpr_pattern_stack.push((
                        candidate[remainder_start..remainder_end].to_string(),
                        pattern_idx.index(if anchored_left {
                            1..pattern_len
                        } else {
                            0..pattern_len - 1
                        }),
                        None,
                    ));

                    let reversed_sub_candidate = candidate[streak_start..streak_end]
                        .chars()
                        .rev()
                        .collect::<String>();

                    self.nested_constraints_execute(
                        &reversed_sub_candidate,
                        pattern_idx.step_in(constraint_index, Branch::Straight, vlen),
                    );

                    self.subexpr_pattern_stack.pop();
                }
                Negate(vs) => {
                    let sub_candidate = &candidate[streak_start..streak_end];
                    let next_pattern_idx =
                        pattern_idx.step_in(constraint_index, Branch::Straight, vs.len());
                    self.subexpr_pattern_stack.push((
                        "".to_string(),
                        PatternIndex::default(),
                        Some(0),
                    ));
                    self.nested_constraints_execute(sub_candidate, next_pattern_idx);

                    if let (_, _, Some(found)) = self.subexpr_pattern_stack.pop().unwrap() {
                        if found > 0 {
                            continue 'streaks;
                        } else {
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
                Word(dir) => {
                    let mut probe = "^".to_string();
                    match dir {
                        &WordDirection::Backwards => {
                            probe += candidate[streak_start..streak_end]
                                .chars()
                                .rev()
                                .collect::<String>()
                                .as_str()
                        }
                        &WordDirection::Forwards => probe += &candidate[streak_start..streak_end],
                    };
                    probe += "$";

                    if SUBSTRINGS.contains_key(&probe) {
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

                Disjunction((b, a)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];
                    // for i in 0..self.subexpr_pattern_stack.len() {
                    //     print!("  ");
                    // }
                    // println!("DISJ {} onm {} P{:?} curpat\n  {:?}\n  {:?}", streak_len, candidate, self.subexpr_pattern_stack, a,b);
                    let count_pre = self.count;
                    let a = a.len();
                    let b = b.len();

                    let pattern_continues_beyond = streak_len > 0 || pattern_len > 1;
                    if pattern_continues_beyond {
                        self.subexpr_pattern_stack.push((
                            candidate[remainder_start..remainder_end].to_string(),
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                            None,
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

                    if pattern_continues_beyond {
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
                            candidate[remainder_start..remainder_end].to_string(),
                            pattern_idx.index(if anchored_left {
                                1..pattern_len
                            } else {
                                0..pattern_len - 1
                            }),
                            None,
                        ));
                    }
                    let a_pattern = pattern_idx.step_in(constraint_index, Branch::Right, a);
                    let b_pattern = pattern_idx.step_in(constraint_index, Branch::Left, b);
                    self.subexpr_pattern_stack
                        .push((sub_candidate.to_string(), b_pattern, None));
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
                    let reset_var = match &self.bindings[v] {
                        Some(val) if val == &candidate[streak_start..streak_end] => Some(val),
                        None => {
                            self.bindings[v] =
                                Some(candidate[streak_start..streak_end].to_string());
                            None
                        }
                        _ => continue 'streaks,
                    }
                    .cloned();

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
                            .map(|v| &self.bindings[*v])
                            .filter_map(|b| b.as_ref().map(|v| v.len()))
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
                Anagram(use_all_places, use_all_tags, fodder) => {
                    let fodder = fodder.clone();
                    let use_all_places = *use_all_places;
                    let use_all_tags = *use_all_tags;

                    let mut letters = letters_possible(&self[&pattern_idx][constraint_index]);
                    let sub_candidate = &candidate[streak_start..streak_end];

                    for n in sub_candidate.chars(){
                        if letters[n] > 0 {
                            letters[n] -= 1;
                        } else {
                            continue 'streaks
                        }
                    }

                    if !fodder.iter().all(|f| match f {
                        Literal(_) => true,
                        _ => false,
                    }) {
                        let mut covers: Vec<(Tag, (usize, usize))> =
                            Vec::with_capacity(fodder.len());
                        for (i, f) in fodder.into_iter().enumerate() {
                            let mut f_covers = vec![];
                            let (a, b) = length_range(&f, &self.bindings, &self.spec_var_length);
                            for c_start in 0..=(sub_candidate.len().saturating_sub(a)) {
                                for c_len in a..=b.min(sub_candidate.len().saturating_sub(c_start))
                                {
                                    let next_pattern_idx = pattern_idx.step_in(
                                        constraint_index,
                                        Branch::AnagramIndex(i),
                                        1,
                                    );
                                    self.subexpr_pattern_stack.push((
                                        String::default(),
                                        PatternIndex::default(),
                                        Some(0),
                                    ));
                                    self.nested_constraints_execute(
                                        &sub_candidate[c_start..c_start + c_len],
                                        next_pattern_idx,
                                    );
                                    if let (_, _, Some(found)) =
                                        self.subexpr_pattern_stack.pop().unwrap()
                                    {

                                        if found > 0 {
                                            f_covers.push((c_start, c_start + c_len))
                                        }
                                    }
                                }
                            }

                            if use_all_tags && f_covers.len() == 0 {
                                continue 'streaks;
                            }

                            covers.extend(f_covers.into_iter().map(|r| (Tag(i, match f{
                                Variable(v) => Some(v),
                                _ => None
                            }), r)));
                        }
                        // println!("Solve c {} {:?}. {}", sub_candidate, covers, sub_candidate.len());
                        let works = constraint_solver::evaluate_covers(sub_candidate, &covers, sub_candidate.len(), use_all_tags, use_all_places);
                        if !works {
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
                    bound: 0..self.patterns[pattern_depth].1.len(),
                    length_range: self.patterns[pattern_depth].0,
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
        callback: &'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<String>>) -> bool,
    ) where
        T: std::ops::Deref<Target = &'a str>,
    {
        self.callback = Some(callback);
        self.execute_pattern(candidates);
        self.callback = None;
    }
}

use serde::Serialize;

#[wasm_bindgen]
#[derive(Eq, PartialEq, Clone, Debug, Serialize)]
struct JsResult {
    words: Vec<String>,
    bindings: HashMap<char, String>,
}

#[wasm_bindgen]
pub fn q(query: &str, f: &js_sys::Function) -> usize {
    let _wc = SUBSTRINGS.len(); // trigger lazy static
    let mut count = 0;
    let mut last_val = JsResult {
        words: vec![],
        bindings: HashMap::default(),
    };
    let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<String>>| {
        let words = a.iter().map(|v| v.to_string()).collect_vec();
        let bindings = ('A'..'Z')
            .filter_map(|v| b[v].as_ref().map(|binding| (v, binding.to_string())))
            .collect();
        let result = JsResult { words, bindings };

        if result != last_val {
            last_val = result.clone();
            count += 1;
            let this = JsValue::null();
            let x = JsValue::from_serde(&result).unwrap();
            let result = f.call1(&this, &x);
            return match result {
                Err(_) => false,
                Ok(v) => match v.as_bool() {
                    None => true,
                    Some(true) => true,
                    Some(false) => false,
                },
            };
        }
        true
    };

    parser::parser_exec(&query).execute(WORDS.iter(), &mut cb);

    count
}

pub fn q_internal(query: &str, max_results: usize) -> usize {
    let mut count = 0;
    let mut last_val = JsResult {
        words: vec![],
        bindings: HashMap::default(),
    };
    let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<String>>| {
        let words = a.iter().map(|v| v.to_string()).collect_vec();
        let bindings = ('A'..'Z')
            .filter_map(|v| b[v].as_ref().map(|binding| (v, binding.to_string())))
            .collect();
        let result = JsResult { words, bindings };
        if result != last_val {
            last_val = result.clone();
            count += 1;
        }
        count < max_results
    };

    parser::parser_exec(&query).execute(WORDS.iter(), &mut cb);

    count
}
