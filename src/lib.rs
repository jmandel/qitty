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
use std::result;

use Constraint::*;

#[derive(Clone, Debug)]
pub struct VariableSpec {
    len_min: Option<usize>,
    len_max: Option<usize>,
}

pub type VariableValue = Vec<char>;
pub type VariableBindings = BTreeMap<VariableName, VariableValue>;

#[derive(Clone, Debug)]
pub struct Production {
    string: Vec<char>,
    bindings: VariableBindings,
}

#[derive(Clone, Debug)]
pub enum Constraint {
    Literal(char),
    SingleChar,
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
pub struct BindingsManager {
    bindings: HashSet<VariableBindings>,
    variables: HashSet<BTreeSet<VariableName>>,
}

impl Default for BindingsManager {
    fn default() -> Self {
        BindingsManager {
            bindings: HashSet::new(),
            variables: HashSet::new(),
        }
    }
}

impl BindingsManager {
    fn add(&mut self, bindings: &VariableBindings) {
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
    fn allows(&self, bindings: VariableBindings) -> bool {
        for subset in bindings
            .keys()
            .powerset()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            if self
                .variables
                .contains(&subset.clone().into_iter().copied().collect())
            {
                if !self.bindings.contains(
                    &subset
                        .into_iter()
                        .map(|v| (*v, bindings.get(v).unwrap().clone()))
                        .collect(),
                ) {
                    return false;
                }
            }
        }
        return true;
    }
}

impl Pattern {
    fn len_range(&self) -> (usize, usize) {
        self.items.iter().fold((0usize, 0usize), |acc, c| {
            (
                acc.0
                    + match c {
                        Variable(v) => 0, // TODO read from spc
                        Disjunction(ps) => std::cmp::min(ps.0.len_range().0, ps.1.len_range().0),
                        Conjunction(ps) => std::cmp::max(ps.0.len_range().0, ps.1.len_range().0),
                        Anagram(v, u, open) => v.len() + u.len(),
                        Literal(_) | LiteralFrom(_) | SingleChar => 1,
                    },
                acc.1
                    + match c {
                        Variable(v) => 1000, // TODO read from spc
                        Disjunction(ps) => std::cmp::max(ps.0.len_range().1, ps.1.len_range().1),
                        Conjunction(ps) => std::cmp::min(ps.0.len_range().1, ps.1.len_range().1),
                        Anagram(v, u, open) => v.len() + u.len() + if *open { 1000 } else { 0 },
                        Literal(_) | LiteralFrom(_) | SingleChar => 1,
                    },
            )
        })
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
    fn evaluate(
        &self,
        p: &Production,
        variable_spec: &HashMap<char, VariableSpec>,
        binding_limits: &BindingsManager,
    ) -> Vec<Production> {
        self.evaluate_helper(&p.string, BTreeMap::new(), variable_spec, 0, binding_limits)
            .into_iter()
            .map(|survived_p| Production {
                string: p.string.clone(),
                bindings: survived_p,
            })
            .collect()
    }

    fn evaluate_helper(
        &self,
        p: &[char],
        bindings: VariableBindings,
        variable_spec: &HashMap<char, VariableSpec>,
        at: usize,
        binding_limits: &BindingsManager,
    ) -> Vec<VariableBindings> {
        if at == self.items.len() {
            return if p.len() == 0 { vec![bindings] } else { vec![] };
        }
        match self.items.get(at).unwrap() {
            SingleChar => {
                if p.len() > 0 {
                    self.evaluate_helper(&p[1..], bindings, variable_spec, at + 1, binding_limits)
                } else {
                    vec![]
                }
            }
            Literal(c) => {
                if p.starts_with(&[*c]) {
                    self.evaluate_helper(&p[1..], bindings, variable_spec, at + 1, binding_limits)
                } else {
                    vec![]
                }
            }
            LiteralFrom(cs) => {
                if p.len() > 0 && cs.contains(&p[0]) {
                    self.evaluate_helper(&p[1..], bindings, variable_spec, at + 1, binding_limits)
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

                        let mut start: Vec<&char> = p[0..target_len].iter().collect();
                        for &f in fodder {
                            match start.iter().position(|v| **v == f) {
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
                        );
                    })
                    .collect()
            }
            Variable(varname) => {
                let bound = bindings.get(varname);
                if bound.is_some() && p.starts_with(bound.unwrap()) {
                    self.evaluate_helper(
                        &p[bound.unwrap().len()..],
                        bindings,
                        variable_spec,
                        at + 1,
                        binding_limits,
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

                    (range_to_scan.0..=range_to_scan.1)
                        .filter(|&i| i <= p.len())
                        .flat_map(|i| {
                            let bind_as: Vec<char> = p[0..i].iter().copied().collect();
                            let proposed_binding: VariableBindings = bindings
                                .clone()
                                .into_iter()
                                .chain(vec![(*varname, bind_as.clone())])
                                .collect();
                            if !binding_limits.allows(proposed_binding) {
                                return vec![];
                            }
                            let mut shadowed = bindings.clone();
                            shadowed.insert(*varname, bind_as);
                            self.evaluate_helper(
                                &p[i..],
                                shadowed,
                                variable_spec,
                                at + 1,
                                binding_limits,
                            )
                        })
                        .collect()
                }
            }
            Disjunction((t1, t2)) => {
                let result1 =
                    t1.evaluate_helper(&p, bindings.clone(), variable_spec, 0, binding_limits);
                let result2 = t2.evaluate_helper(&p, bindings, variable_spec, 0, binding_limits);
                result1.into_iter().chain(result2.into_iter()).collect()
            }
            Conjunction((t1, t2)) => {
                let result1 =
                    t1.evaluate_helper(&p, bindings.clone(), variable_spec, 0, binding_limits);
                let result2 = t2.evaluate_helper(&p, bindings, variable_spec, 0, binding_limits);
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
    fn execute(&self, dict: &Vec<Production>) -> Vec<Vec<Production>> {
        let mut steps: Vec<Vec<Production>> = vec![];
        let mut binding_limits: BindingsManager = BindingsManager::default();

        for part in &self.parts {
            let range = part.len_range();
            steps.push(
                dict.iter()
                    .filter(|p| p.string.len() >= range.0 && p.string.len() <= range.1)
                    .flat_map(|production| {
                        part.evaluate(production, &self.variables, &binding_limits)
                    })
                    .collect(),
            );
            for b in steps.last().unwrap().iter().map(|r| &r.bindings) {
                binding_limits.add(&b);
            }
        }

        self.expand(&steps, BTreeMap::new())
    }

    fn expand(&self, plist: &[Vec<Production>], b: VariableBindings) -> Vec<Vec<Production>> {
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
        let rep = r.iter().map(|p| p.string.iter().join("")).join(";");
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
