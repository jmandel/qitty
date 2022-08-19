use dict::{SUBSTRINGS, WORDS};
use wasm_bindgen::prelude::*;

pub mod dict;
pub mod examples;
pub mod parser;

#[macro_use]
extern crate lazy_static;

use itertools::Itertools;

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

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Pattern {
    items: Vec<Constraint>,
    ranges: Option<Vec<(usize, usize)>>,
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

pub struct ExecutionContext<'a, 'b> {
    candidate_stack: Vec<&'a str>,
    bindings: VariableMap<Option<&'a str>>,
    patterns: Vec<Vec<Constraint>>,
    spec_var_length: VariableMap<(usize, usize)>,
    spec_var_inequality: Vec<Vec<char>>,
    spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    active: bool,
    // count: usize,
    // sum: usize,
    subexpr_binding_stack: Vec<FxHashSet<VariableMap<Option<&'a str>>>>,
    callback: Option<&'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<&'a str>>) -> bool>,
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
            bindings: VariableMap::default(),
            patterns,
            spec_var_length,
            spec_var_inequality,
            spec_var_sets_length,
            active: true,
            subexpr_binding_stack: vec![],
            callback: None, // count: 0,
                            // sum: 0,
        }
    }

    fn nested_constraints_execute(&mut self, candidate: &'a str, pattern: &[Constraint]) {
        // println!("Patt {:?} cand {} in {:?}", pattern, candidate, self.candidate_stack);
        if candidate.len() == 0
            && (pattern.len() == 0 || (pattern.len() == 1 && pattern[0] == Star))
            && self.patterns.len() == self.candidate_stack.len()
            && self.subexpr_binding_stack.len() == 0
        {
            self.active = (self.callback.as_mut().unwrap())(&self.candidate_stack, &self.bindings);
            return;
        }

        if candidate.len() == 0 && 
             (pattern.len() == 0 || (pattern.len() == 1 && pattern[0] == Star)) &&
         self.subexpr_binding_stack.len() > 0 {
            self.subexpr_binding_stack
                .last_mut()
                .unwrap()
                .insert(self.bindings.clone());
            return;
        }

        if pattern.len() == 0 {
            let failed: FxHashSet<usize> = FxHashSet::default();
            if candidate.len() > 0 {
                return;
            }

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
                        Variable(v) => {
                            match self.bindings[*v] {
                                Some(bound) => {probe += bound;}
                                None => {
                                    probe.clear();
                                    in_streak = false;
                                }
                                
                            }
                        }
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

        if candidate.len() == 0 {
            return;
        }

        let mut constraint_index = 0usize;
        let mut anchored_left = true;
        let mut streak_start = 0;
        let mut streak_length_bound = length_range(
            pattern.first().unwrap(),
            &self.bindings,
            &self.spec_var_length,
        );

        if streak_length_bound.0 != streak_length_bound.1 {
            let right_anchor = length_range(
                &pattern.last().unwrap(),
                &self.bindings,
                &self.spec_var_length,
            );
            if right_anchor.0 == right_anchor.1 {
                streak_length_bound = right_anchor;
                anchored_left = false;
                constraint_index = pattern.len().saturating_sub(1);
                streak_start = candidate.len().saturating_sub(streak_length_bound.0);
            }
        }
        // println!(
        //     "{:?}; {}@{:?}->{:?}",
        //     &self.candidate_stack, candidate, pattern, streak_length_bound
        // );

        // let remaining_length_bound = pattern
        //     .iter()
        //     .skip(1)
        //     .map(|p| length_range(p, &self.bindings, &self.spec))
        //     .fold((0, 0), |acc, (a, b)| (acc.0 + a, acc.1 + b));
        // streak_length_bound.0 = streak_length_bound.0.max(candidate.len().saturating_sub(remaining_length_bound.1));
        // streak_length_bound.1 = streak_length_bound.1.min(candidate.len().saturating_sub(remaining_length_bound.0));
        // self.count+= 1;

        if candidate.len() < streak_length_bound.0 {
            return;
        }

        streak_length_bound.1 = streak_length_bound.1.min(candidate.len());
        'streaks: for streak_len in streak_length_bound.0..=streak_length_bound.1 {
            if !self.active {
                return;
            }
            let streak_end = (streak_start + streak_len).min(candidate.len());
            let remainder_len = candidate.len().saturating_sub(streak_len);
            let remainder_start = if anchored_left { streak_end } else { 0 };
            let remainder_end = remainder_start + remainder_len;

            match &pattern[constraint_index] {
                Subpattern(v) => {
                    let mut stitched: Vec<Constraint> = vec![];
                    if anchored_left {
                        stitched.extend(v.clone());
                    }
                    
                    stitched.extend(
                        pattern[if anchored_left {
                            1..pattern.len()
                        } else {
                            0..pattern.len() - 1
                        }]
                        .into_iter()
                        .cloned().collect_vec()
                    );
                    if !anchored_left {
                        stitched.extend(v.clone());
                    }
 
                    self.nested_constraints_execute(candidate, &stitched[..]);
                }
                Star => {
                    self.nested_constraints_execute(
                        &candidate[remainder_start..remainder_end],
                        &pattern[if anchored_left {
                            1..pattern.len()
                        } else {
                            0..pattern.len() - 1
                        }],
                    );
                }

                Disjunction((a, b)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];
                    let bindings_before_disjunction: VariableMap<Option<&str>> =
                        self.bindings.clone();

                    let a_pattern = &a[..];
                    self.subexpr_binding_stack.push(FxHashSet::default());
                    self.nested_constraints_execute(sub_candidate, a_pattern);
                    let result_a = self.subexpr_binding_stack.pop().unwrap();

                    for a_binding in result_a {
                        self.bindings = a_binding;
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            &pattern[if anchored_left {
                                1..pattern.len()
                            } else {
                                0..pattern.len() - 1
                            }],
                        );
                    }

                    self.bindings = bindings_before_disjunction.clone();
                    let b_pattern = &b[..];
                    self.subexpr_binding_stack.push(FxHashSet::default());
                    self.nested_constraints_execute(sub_candidate, b_pattern);
                    let result_b = self.subexpr_binding_stack.pop().unwrap();

                    for b_binding in result_b {
                        self.bindings = b_binding;
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            &pattern[if anchored_left {
                                1..pattern.len()
                            } else {
                                0..pattern.len() - 1
                            }],
                        );
                    }

                    self.bindings = bindings_before_disjunction;
                }

                Conjunction((a, b)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];

                    let bindings_before_conjunction: VariableMap<Option<&str>> =
                        self.bindings.clone();

                    let a_pattern = &a[..];
                    self.subexpr_binding_stack.push(FxHashSet::default());
                    self.nested_constraints_execute(sub_candidate, a_pattern);
                    let result_a = self.subexpr_binding_stack.pop().unwrap();

                    let b_pattern = &b[..];
                    self.subexpr_binding_stack.push(FxHashSet::default());
                    for a_binding in result_a {
                        // println!("RESA {:?} -- {:?}", b_pattern, a_binding);
                        self.bindings = a_binding;
                        self.nested_constraints_execute(sub_candidate, b_pattern);
                    }

                    let result_b = self.subexpr_binding_stack.pop().unwrap();
                    for b_binding in result_b {
                    //  println!("RESB  {:?} {}", b_binding,self.subexpr_binding_stack.len() );
                        self.bindings = b_binding;
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            &pattern[if anchored_left {
                                1..pattern.len()
                            } else {
                                0..pattern.len() - 1
                            }],
                        );
                    }

                    self.bindings = bindings_before_conjunction;
                }
                Literal(v) => {
                    if candidate[streak_start..streak_end].starts_with(*v) {
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            &pattern[if anchored_left {
                                1..pattern.len()
                            } else {
                                0..pattern.len() - 1
                            }],
                        );
                    }
                }
                LiteralFrom(v) => {
                    if v.contains(&candidate[streak_start..streak_end].chars().next().unwrap()) {
                        self.nested_constraints_execute(
                            &candidate[remainder_start..remainder_end],
                            &pattern[if anchored_left {
                                1..pattern.len()
                            } else {
                                0..pattern.len() - 1
                            }],
                        );
                    }
                }
                Variable(v) => {
                    // println!("Check {:?} var {} {:?} {:?} seg {:?}", pattern, v, self.bindings, candidate, &candidate[streak_start..streak_end]);
                    let reset_var = match self.bindings[*v] {
                        Some(val) if val == &candidate[streak_start..streak_end] => Some(val),
                        None => {
                            self.bindings[*v] = Some(&candidate[streak_start..streak_end]);
                            None
                        }
                        _ => continue 'streaks,
                    };

                    /* !=AB */
                    for var_distinct_constraint in &self.spec_var_inequality {
                        for (&a, &b) in var_distinct_constraint.into_iter().tuple_combinations() {
                            if self.bindings[a].is_some() && self.bindings[a] == self.bindings[b] {
                                self.bindings[*v] = reset_var;
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
                            self.bindings[*v] = reset_var;
                            continue 'streaks;
                        }
                    }
                    // let vars_constrained = ['A', 'B', 'C'];
                    // let vars_constrained_to=4;
                    // let (bound_count, bound_sum): (usize, usize) =
                    //     vars_constrained.iter().map(|v|self.bindings[*v])
                    //         .filter_map(|b| b.map(|v| v.len()))
                    //         .fold((0, 0), |acc, v| (acc.0 + 1, acc.1 + v));
                    // if bound_sum > vars_constrained_to || (bound_count == 3 && bound_sum != vars_constrained_to) {
                    //     self.bindings[*v] = reset_var;
                    //     continue 'streaks;
                    // }

                    // println!("Submatchv {:?}; {:?} p={:?},{}, {:?} for candidate {:?} [{}-{}]", &streak_length_bound, &anchored_left, pattern, v, self.bindings[*v], self.candidate_stack,  streak_start, streak_end);
                    // println!("Var {:?} {:?} {:?} {:?}", self.candidate_stack, candidate, v, self.bindings);
                    self.nested_constraints_execute(
                        &candidate[remainder_start..remainder_end],
                        &pattern[if anchored_left {
                            1..pattern.len()
                        } else {
                            0..pattern.len() - 1
                        }],
                    );

                    self.bindings[*v] = reset_var;
                }
                Anagram(open, fodder) => {
                    let bindings_before_anagram: VariableMap<Option<&str>> = self.bindings.clone();
                    let sub_candidate = &candidate[streak_start..streak_end];
                    // println!("Consider anagra {:?}", sub_candidate);

                    let (fixed_len, _variable_len): (Vec<_>, Vec<_>) = fodder
                        .iter()
                        .map(|c| (c, length_range(c, &self.bindings, &self.spec_var_length)))
                        .partition(|(c, flen)| flen.0 == flen.1);

                    let mut last_c = &Star;
                    let mut last_c_index = 255;
                    'flc: for (flc, (a, _b)) in fixed_len.into_iter().sorted() {
                        for flc_start in 0..sub_candidate.len() {
                            if last_c == flc && flc_start <= last_c_index {
                                continue;
                            }
                            if flc_start + a > sub_candidate.len() {
                                continue;
                            }
                            let sub_pattern = &[flc.clone()];
                            self.subexpr_binding_stack.push(FxHashSet::default());
                            self.nested_constraints_execute(
                                &sub_candidate[flc_start..flc_start + a],
                                &sub_pattern[..],
                            );
                            let results = self.subexpr_binding_stack.pop().unwrap();
                            self.bindings = bindings_before_anagram.clone(); // TODO implement a binding stack on the Context
                            if results.len() > 0 {
                                last_c = flc;
                                last_c_index = flc_start;
                                continue 'flc;
                            }
                        }
                        // println!("Failed ana");
                        continue 'streaks;
                    }

                    for fodder_order in fodder.iter().permutations(fodder.len()) {
                        let mut sub_pattern = fodder_order
                            .into_iter()
                            .cloned()
                            .flat_map(|c| {
                                if *open {
                                    vec![Star, c].into_iter()
                                } else {
                                    vec![c].into_iter()
                                }
                            })
                            .collect_vec();
                        if (*open) {
                            sub_pattern.push(Star);
                        }

                        // println!("Anagram order {:?} for {} in {} -{:?}", sub_pattern, sub_candidate, candidate, streak_length_bound);
                        self.subexpr_binding_stack.push(FxHashSet::default());
                        self.nested_constraints_execute(sub_candidate, &sub_pattern[..]);
                        let fodder_order_result = self.subexpr_binding_stack.pop().unwrap();
                        for b in fodder_order_result {
                            self.bindings = b;
                            self.nested_constraints_execute(
                                &candidate[remainder_start..remainder_end],
                                &pattern[if anchored_left {
                                    1..pattern.len()
                                } else {
                                    0..pattern.len() - 1
                                }],
                            );
                            self.bindings = bindings_before_anagram.clone();
                            // continue 'streaks;
                        }
                    }

                    self.bindings = bindings_before_anagram.clone();
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
            let this_pattern = self.patterns.get(pattern_depth).unwrap().clone();
            self.nested_constraints_execute(*c, &this_pattern);
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
