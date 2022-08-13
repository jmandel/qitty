use crate::dict::DICTIONARY;
use dict::{WORDS, SUBSTRINGS};
use wasm_bindgen::prelude::*;

pub mod dict;
pub mod examples;
pub mod parser;

#[macro_use]
extern crate lazy_static;

type VariableName = char;
use itertools::Itertools;
use std::collections::BTreeMap;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as FmtWrite;
use std::time::SystemTime;

use Constraint::*;

#[derive(Clone, Debug)]
pub struct VariableSpec {
    len_min: Option<usize>,
    len_max: Option<usize>,
}

pub type VariableValue<'a> = &'a str;
pub type VariableBindings<'a> = BTreeMap<VariableName, VariableValue<'a>>;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
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
    ranges: Option<Vec<(usize, usize)>>,
}

#[derive(Clone)]
pub struct BindingsManager<'a> {
    bindings: HashSet<VariableBindings<'a>>,
    variables: Vec<String>,
}

impl<'a> Default for BindingsManager<'a> {
    fn default() -> Self {
        BindingsManager {
            bindings: HashSet::new(),
            variables: vec![],
        }
    }
}

impl<'a> BindingsManager<'a> {
    fn add(&mut self, bindings: &VariableBindings<'a>) {
        for subset in bindings.keys().powerset() {
            if subset.len() == 0 {
                continue;
            }
            self.bindings.insert(
                subset
                    .iter()
                    .map(|&&c| (c, bindings.get(&c).unwrap().clone()))
                    .collect(),
            );
            let subset_vars = subset.into_iter().sorted().join("");
            if !self.variables.contains(&subset_vars) {
                self.variables.push(subset_vars);
            }
        }
    }

    fn subsets_for_allows(&self, bindings: &VariableBindings, plus_var: char) -> Vec<&String> {
        let ret = self
            .variables
            .iter()
            .filter(|vs| {
                vs.chars().contains(&plus_var)
                    && vs
                        .chars()
                        .all(|v| plus_var == v || bindings.contains_key(&v))
            })
            .sorted_by_key(|vs| -1 * vs.len() as isize)
            .fold(vec![], |mut acc: Vec<&String>, vs| {
                if acc
                    .iter()
                    .any(|existing_vs| vs.chars().all(|c| existing_vs.contains(c)))
                {
                    acc
                } else {
                    acc.push(vs);
                    acc
                }
            });

        // if self.variables.len() > 0{
        //     println!("BM {} {:?} @{:?} +{} --> {:?}", self.variables.len(), self.variables, bindings, plus_var, ret);
        // }

        ret
    }
    fn allows(
        &self,
        subsets: &Vec<&String>,
        bindings: &VariableBindings,
        (plus_var, plus_binding): (char, &str),
    ) -> bool {
        // if subsets.len() > 0 {
        //     println!("SSL {}", subsets.len());
        // }
        subsets.iter().all(|subset| {
            self.bindings.contains(
                &subset
                    .chars()
                    .chain([plus_var])
                    .map(|v| (v, *(bindings.get(&v).unwrap_or(&plus_binding))))
                    .collect(),
            )
        })
    }
}

impl Pattern {
    fn new(items: Vec<Constraint>) -> Pattern {
        Pattern {
            items,
            ranges: None,
        }
    }
    // TODO add Var Spec to this
    // variable_spec: &HashMap<char, VariableSpec>,
    fn len_range_starting_at(&self, start: usize, bindings: &VariableBindings) -> (usize, usize) {
        let res = self
            .items
            .iter()
            .skip(start)
            .fold((0usize, 0usize), |acc, c| {
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
                            Anagram(v, u, _open) => v.len() + u.len(),
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
    ) -> Vec<Production<'a>> {
        // println!("begin with {} with range{:?}", p.streak, len_range);
        self.evaluate_helper(&p.streak, BTreeMap::new(), variable_spec, 0, binding_limits)
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
    ) -> Vec<VariableBindings<'a>> {
        if at == self.items.len() {
            return if p.len() == 0 { vec![bindings] } else { vec![] };
        }
        match self.items.get(at).unwrap() {
            Literal(c) => {
                if p.starts_with(&[*c]) {
                    self.evaluate_helper(&p[1..], bindings, variable_spec, at + 1, binding_limits)
                } else {
                    vec![]
                }
            }
            LiteralFrom(cs) => {
                if p.len() > 0 && cs.contains(&p.chars().nth(0).unwrap()) {
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
                        );
                    })
                    .collect()
            }
            Variable(varname) => {
                let bound = bindings.get(varname);
                if bound.is_some() && p.starts_with(bound.unwrap()) {
                    let bound_len = bound.unwrap().len();
                    self.evaluate_helper(
                        &p[bound_len..],
                        bindings,
                        variable_spec,
                        at + 1,
                        binding_limits,
                    )
                } else if bound.is_some() {
                    vec![]
                } else {
                    let min_additional_matches = self.items[at + 1..]
                        .iter()
                        .filter(|i| {
                            if let Variable(otherv) = i {
                                otherv == varname
                            } else {
                                false
                            }
                        })
                        .count();

                    let subsets = binding_limits.subsets_for_allows(&bindings, *varname);

                    let mut range_to_scan = (
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

                    let (suffix_a, suffix_b) = self.len_range_starting_at(at + 1, &bindings);
                    range_to_scan.0 = range_to_scan.0.max(p.len().saturating_sub(suffix_b));
                    range_to_scan.1 = range_to_scan.1.min(p.len().saturating_sub(suffix_a));

                    (range_to_scan.0..=range_to_scan.1)
                        .filter(|&i| i <= p.len())
                        .filter(|&i| {
                            binding_limits.allows(&subsets, &bindings, (*varname, &p[..i]))
                        })
                        .flat_map(|i| {
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
                            )
                        })
                        .collect()
                }
            }
            Disjunction((t1, t2)) => {
                let (a1, b1) = t1.len_range();
                let (a2, b2) = t2.len_range();
                let (suffix_a, suffix_b) = self.len_range_starting_at(at + 1, &bindings);
                let a = a1.min(a2).max(p.len().saturating_sub(suffix_b));
                let b = b1.max(b2).min(p.len().saturating_sub(suffix_a));

                (a..=b)
                    .filter(|i| *i <= p.len())
                    .flat_map(|i| {
                        let result1 = t1.evaluate_helper(
                            &p[..i],
                            bindings.clone(),
                            variable_spec,
                            0,
                            binding_limits,
                        ); // fix len_range
                        let result2 = t2.evaluate_helper(
                            &p[..i],
                            bindings.clone(),
                            variable_spec,
                            0,
                            binding_limits,
                        );
                        result1
                            .into_iter()
                            .chain(result2.into_iter())
                            .unique()
                            .flat_map(move |discovered_bindings| {
                                self.evaluate_helper(
                                    &p[i..],
                                    discovered_bindings,
                                    variable_spec,
                                    at + 1,
                                    binding_limits,
                                )
                            })
                    })
                    .collect()
            }
            Conjunction((t1, t2)) => {
                let (a1, b1) = t1.len_range();
                let (a2, b2) = t2.len_range();
                let (suffix_a, suffix_b) = self.len_range_starting_at(at + 1, &bindings);
                let a = a1.max(a2).max(p.len().saturating_sub(suffix_b));
                let b = b1.min(b2).min(p.len().saturating_sub(suffix_a));

                (a..=b)
                    .filter(|i| *i <= p.len())
                    .flat_map(|i| {
                        t1.evaluate_helper(
                            &p[..i],
                            bindings.clone(),
                            variable_spec,
                            0,
                            binding_limits,
                        )
                        .into_iter()
                        .flat_map(|bindings| {
                            t2.evaluate_helper(&p[..i], bindings, variable_spec, 0, binding_limits)
                        })
                        .flat_map(move |discovered_bindings| {
                            self.evaluate_helper(
                                &p[i..],
                                discovered_bindings,
                                variable_spec,
                                at + 1,
                                binding_limits,
                            )
                        })
                        .collect::<Vec<_>>()
                    })
                    .collect()
            }
        }
    }

    fn cache_len_ranges(&mut self, _varspec: &HashMap<char, VariableSpec>) {
        let nobindings: BTreeMap<_, _> = BTreeMap::new();
        self.ranges = Some(
            (0..self.items.len())
                .map(|i| self.len_range_starting_at(i, &nobindings))
                .collect(),
        );
    }
}

#[derive(Clone, Debug)]
pub struct Query {
    parts: Vec<Pattern>,
    variables: HashMap<VariableName, VariableSpec>,
}

impl Query {
    fn execute<'a>(&mut self, dict: &Vec<Production<'a>>) -> Vec<Vec<Production<'a>>> {
        let mut steps: Vec<Vec<Production>> = vec![];
        let mut binding_limits: BindingsManager = BindingsManager::default();

        for part in &mut self.parts {
            part.cache_len_ranges(&self.variables);

            let range = part.len_range();
            steps.push(
                dict.iter()
                    .filter(|p| p.streak.len() >= range.0 && p.streak.len() <= range.1)
                    .flat_map(|production| {
                        part.evaluate(production, &self.variables, &binding_limits)
                    })
                    .collect(),
            );
            for b in steps.last().unwrap().iter().map(|r| &r.bindings) {
                binding_limits.add(&b);
            }
            println!(
                "Done step {} ({})",
                steps.len(),
                steps.last().unwrap().len()
            );
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

use parser::VariableMap;
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

    let mut ctx = parser::parser_exec(&query).build(&mut cb);

    ctx.execute(WORDS.iter());

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
