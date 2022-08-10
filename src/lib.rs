use dict::DICTIONARY;
use wasm_bindgen::prelude::*;

pub mod dict;
pub mod examples;
pub mod parser;

#[macro_use]
extern crate lazy_static;

type VariableName = char;
use itertools::Itertools;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as FmtWrite;

use std::iter::FromIterator;

use std::collections::{HashMap, HashSet};

use Constraint::*;

#[derive(Clone, Debug)]
pub struct VariableSpec {
    len_min: Option<usize>,
    len_max: Option<usize>,
}

pub type VariableValue<'a> = &'a str;
pub type VariableBindings<'a> = BTreeMap<VariableName, VariableValue<'a>>;

#[derive(Clone, Debug)]
pub struct Production<'a> {
    streak: &'a str,
    bindings: VariableBindings<'a>,
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Literal(char),
    LiteralFrom(Vec<char>),
    Anagram(Vec<char>, Vec<Vec<char>>, bool),
    Variable(char),
    Disjunction((Pattern, Pattern)),
    Conjunction((Pattern, Pattern)),
}

#[derive(Clone, Debug)]
pub struct Pattern {
    items: Vec<Constraint>,
}

#[derive(Clone)]
pub struct BindingsManager<'a> {
    bindings: HashSet<VariableBindings<'a>>,
    variables: HashSet<BTreeSet<VariableName>>,
}

impl<'a> Default for BindingsManager<'a> {
    fn default() -> Self {
        BindingsManager {
            bindings: HashSet::new(),
            variables: HashSet::new(),
        }
    }
}

impl<'a> BindingsManager<'a> {
    fn add(&mut self, bindings: &VariableBindings<'a>) {
        for subset in bindings.keys().powerset() {
            self.bindings.insert(
                subset
                    .iter()
                    .map(|&&c| (c, bindings.get(&c).unwrap().clone()))
                    .collect(),
            );
            self.variables.insert(subset.into_iter().cloned().collect());
        }
    }
    fn allows(
        &self,
        bindings: &VariableBindings,
        (plus_var, plus_binding): (char, &[&str]),
    ) -> Vec<bool> {
        let subsets: Vec<_> = bindings
            .keys()
            .powerset()
            .filter(|subset| {
                self.variables.contains(&BTreeSet::from_iter(
                    subset.iter().map(|v| **v).chain([plus_var]),
                ))
            })
            .collect();

        plus_binding
            .iter()
            .map(|b| {
                subsets.iter().all(|subset| {
                    self.bindings.contains(
                        &subset
                            .into_iter()
                            .chain([&&plus_var])
                            .map(|v| (**v, *(bindings.get(v).unwrap_or(b))))
                            .collect(),
                    )
                })
            })
            .collect()
    }
}

impl Pattern {
    fn len_range_starting_at(&self, start: usize, bindings: &VariableBindings) -> (usize, usize) {
        let res = self
            .items
            .iter()
            .skip(start)
            .fold((0usize, 0usize), |acc, c| {
                // println!("Folding {:?}, {:?} {:?}", acc, c, bindings);
                (
                    acc.0
                        + match c {
                            Variable(v) => bindings.get(v).map(|val| val.len()).unwrap_or(0),
                            Disjunction(ps) => {
                                std::cmp::min(ps.0.len_range().0, ps.1.len_range().0)
                            }
                            Conjunction(ps) => {
                                std::cmp::max(ps.0.len_range().0, ps.1.len_range().0)
                            }
                            Anagram(v, u, open) => v.len() + u.len(),
                            Literal(_) | LiteralFrom(_) => 1,
                        },
                    acc.1
                        + match c {
                            Variable(v) => bindings.get(v).map(|bound| bound.len()).unwrap_or(1000),
                            Disjunction(ps) => {
                                std::cmp::max(ps.0.len_range().1, ps.1.len_range().1)
                            }
                            Conjunction(ps) => {
                                std::cmp::min(ps.0.len_range().1, ps.1.len_range().1)
                            }
                            Anagram(v, u, open) => v.len() + u.len() + if *open { 1000 } else { 0 },
                            Literal(_) | LiteralFrom(_) => 1,
                        },
                )
            });
        // println!("Res {:?}", res);
        res
    }

    fn len_range(&self) -> (usize, usize) {
        self.len_range_starting_at(0, &BTreeMap::new())
    }

    fn vars(&self) -> Vec<char> {
        self.items
            .iter()
            .flat_map(|t| match t {
                Variable(c) => vec![*c],
                Disjunction(sp) | Conjunction(sp) => {
                    sp.0.vars()
                        .into_iter()
                        .chain(sp.1.vars().into_iter())
                        .collect()
                }
                _ => vec![],
            })
            .collect()
    }
    fn evaluate<'a>(
        &self,
        p: &Production<'a>,
        variable_spec: &HashMap<char, VariableSpec>,
        binding_limits: &BindingsManager,
        len_range: (usize, usize),
    ) -> Vec<Production<'a>> {
        // println!("begin with {} with range{:?}", p.streak, len_range);
        self.evaluate_helper(
            &p.streak,
            BTreeMap::new(),
            variable_spec,
            0,
            binding_limits,
            len_range,
        )
        .into_iter()
        .map(|survived_p| Production {
            streak: p.streak.clone(),
            bindings: survived_p,
        })
        .collect()
    }

    fn evaluate_helper<'a>(
        &self,
        p: &'a str,
        bindings: VariableBindings<'a>,
        variable_spec: &HashMap<char, VariableSpec>,
        at: usize,
        binding_limits: &BindingsManager,
        len_range: (usize, usize),
    ) -> Vec<VariableBindings<'a>> {
        if len_range.0 > p.len() {
            return vec![];
        }

        if at == self.items.len() {
            return if p.len() == 0 { vec![bindings] } else { vec![] };
        }
        match self.items.get(at).unwrap() {
            Literal(c) => {
                if p.starts_with(&[*c]) {
                    self.evaluate_helper(
                        &p[1..],
                        bindings,
                        variable_spec,
                        at + 1,
                        binding_limits,
                        (len_range.0.saturating_sub(1), len_range.1.saturating_sub(1)),
                    )
                } else {
                    vec![]
                }
            }
            LiteralFrom(cs) => {
                if p.len() > 0 && cs.contains(&p.chars().nth(0).unwrap()) {
                    self.evaluate_helper(
                        &p[1..],
                        bindings,
                        variable_spec,
                        at + 1,
                        binding_limits,
                        (len_range.0.saturating_sub(1), len_range.1.saturating_sub(1)),
                    )
                } else {
                    vec![]
                }
            }
            Anagram(fodder, extras, allow_more) => {
                let target_len_min = fodder.len() + extras.len();
                let target_len_max = if *allow_more { p.len() } else { target_len_min };

                (target_len_min..=target_len_max)
                    .flat_map(|target_len| {
                        if p.len() < target_len {
                            return vec![];
                        }

                        let mut start: Vec<char> = p[0..target_len].chars().collect();
                        for &f in fodder {
                            match start.iter().position(|v| *v == f) {
                                None => return vec![],
                                Some(pos) => {
                                    start.remove(pos);
                                }
                            }
                        }

                        let matching_extras =
                            start.iter().permutations(extras.len()).find(|remainders| {
                                remainders
                                    .iter()
                                    .zip(extras)
                                    .all(|(r, allowed_chars)| allowed_chars.contains(r))
                            });

                        if matching_extras.is_none() {
                            return vec![];
                        }

                        return self.evaluate_helper(
                            &p[target_len..],
                            bindings.clone(),
                            variable_spec,
                            at + 1,
                            binding_limits,
                            (
                                len_range.0.saturating_sub(target_len),
                                len_range.1.saturating_sub(target_len),
                            ),
                        );
                    })
                    .collect()
            }
            Variable(varname) => {
                let bound = bindings.get(varname);
                if bound.is_some() && p.starts_with(bound.unwrap()) {
                    let bound_len = bound.unwrap().len();
                    self.evaluate_helper(
                        &p[bound.unwrap().len()..],
                        bindings,
                        variable_spec,
                        at + 1,
                        binding_limits,
                        (
                            len_range.0.saturating_sub(bound_len),
                            len_range.1.saturating_sub(bound_len),
                        ),
                    )
                } else if bound.is_some() {
                    vec![]
                } else {
                    let range_to_scan = (
                        variable_spec
                            .get(varname)
                            .unwrap()
                            .len_min
                            .unwrap_or_default(),
                        variable_spec
                            .get(varname)
                            .unwrap()
                            .len_max
                            .unwrap_or(p.len()),
                    );

                    let remaining_range = self.len_range_starting_at(at + 1, &bindings);
                    let streak_range: Vec<_> = (range_to_scan.0..=range_to_scan.1)
                        .filter(|&i| {
                            i <= p.len()
                                && p.len() - i >= remaining_range.0
                                && p.len() - i <= remaining_range.1
                        })
                        .collect();

                    let bindings_for_streak = binding_limits.allows(
                        &bindings,
                        (
                            *varname,
                            &streak_range
                                .iter()
                                .map(|i| &p[0..*i])
                                .into_iter()
                                .collect::<Vec<_>>(),
                        ),
                    );

                    let min_additional_matches = self.items[at+1..].iter().filter(|i| if let Variable(otherv) =i {
                        otherv == varname
                    } else {
                        false
                    }).count();

                    streak_range
                        .iter()
                        .zip(bindings_for_streak)
                        .flat_map(|(&i, bindings_fit)| {
                            if !bindings_fit {
                                return vec![];
                            }
                            let bind_as: &str = &p[0..i];

                            let mut num_matches_found = 0;
                            let mut min_match_spot = i;
                            while num_matches_found < min_additional_matches {
                                match p[min_match_spot..].find(bind_as) {
                                    None => return vec![],
                                    Some(idx) => {
                                        min_match_spot = idx + bind_as.len();
                                        num_matches_found += 1;
                                    }
                                }
                            }
                            
                            let mut shadowed = bindings.clone();
                            shadowed.insert(*varname, bind_as);
                            self.evaluate_helper(
                                &p[i..],
                                shadowed,
                                variable_spec,
                                at + 1,
                                binding_limits,
                                remaining_range,
                            )
                        })
                        .collect()
                }
            }
            Disjunction((t1, t2)) => {
                let result1 = t1.evaluate_helper(
                    &p,
                    bindings.clone(),
                    variable_spec,
                    0,
                    binding_limits,
                    len_range,
                ); // fix len_range
                let result2 =
                    t2.evaluate_helper(&p, bindings, variable_spec, 0, binding_limits, len_range);
                result1.into_iter().chain(result2.into_iter()).collect()
            }
            Conjunction((t1, t2)) => {
                let result1 = t1.evaluate_helper(
                    &p,
                    bindings.clone(),
                    variable_spec,
                    0,
                    binding_limits,
                    len_range,
                ); // fix len_range
                let result2 =
                    t2.evaluate_helper(&p, bindings, variable_spec, 0, binding_limits, len_range);
                result1
                    .into_iter()
                    .filter(|b1| {
                        result2.iter().any(|b2| {
                            b1.keys().all(|b1k| match b2.get(b1k) {
                                None => true,
                                Some(v) => b1.get(b1k) == Some(v),
                            })
                        })
                    })
                    .collect()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Query {
    parts: Vec<Pattern>,
    variables: HashMap<VariableName, VariableSpec>,
}

impl Query {
    fn execute<'a>(&self, dict: &Vec<Production<'a>>) -> Vec<Vec<Production<'a>>> {
        let mut steps: Vec<Vec<Production>> = vec![];
        let mut binding_limits: BindingsManager = BindingsManager::default();

        for part in &self.parts {
            let range = part.len_range();
            steps.push(
                dict.iter()
                    // .filter(|p| p.streak == "adenohypophysis")
                    .filter(|p| p.streak.len() >= range.0 && p.streak.len() <= range.1)
                    .flat_map(|production| {
                        part.evaluate(production, &self.variables, &binding_limits, range)
                    })
                    .collect(),
            );
            for b in steps.last().unwrap().iter().map(|r| &r.bindings) {
                binding_limits.add(&b);
            }
        }

        self.expand(&steps, BTreeMap::new())
    }

    fn expand<'a>(
        &self,
        plist: &[Vec<Production<'a>>],
        b: VariableBindings<'a>,
    ) -> Vec<Vec<Production<'a>>> {
        if plist.len() == 0 {
            return vec![vec![]];
        }

        let mut ret: Vec<Vec<Production>> = vec![];
        for p in &plist[0] {
            if p.bindings
                .iter()
                .all(|(k, v)| b.get(k).and_then(|bk| Some(bk == v)).unwrap_or(true))
            {
                let mut next_b = b.clone();
                next_b.extend(p.bindings.clone());
                for matching in self.expand(&plist[1..], next_b) {
                    if matching.len() == plist.len() - 1 {
                        ret.push(vec![p.clone()].into_iter().chain(matching).collect());
                    }
                }
            }
        }
        ret
    }
}

pub fn q_for_productions(query: &str) -> Vec<Vec<Production>> {
    let mut s = String::new();

    write!(&mut s, "[").unwrap();
    parser::parse(query).execute(&DICTIONARY)
}

#[wasm_bindgen]
pub fn q(query: &str) -> String {
    let mut s = String::new();

    write!(&mut s, "[").unwrap();
    let result = q_for_productions(query);
    for (i, r) in result.iter().enumerate() {
        let rep = r.iter().map(|s| s.streak).join(";");
        write!(&mut s, "{:?}", rep).unwrap();
        if i < result.len() - 1 {
            writeln!(&mut s, ",").unwrap();
        }
    }
    write!(&mut s, "]").unwrap();
    return s;
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn it_works() {
        let result = q("");
        println!("Result count: {:?}", result);
    }
}
