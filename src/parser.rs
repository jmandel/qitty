use std::{mem::swap, str::FromStr};

#[allow(unused_imports)]
use crate::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of},
    combinator::{map, opt, value},
    multi::{fold_many0, many1, many_m_n, separated_list1},
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};

#[derive(Clone, Debug)]
enum QueryTerm {
    QueryTermPattern(Pattern),
    QueryTermVariableLength(char, usize, Option<usize>),
    QueryTermVariablesInequality(Vec<char>),
    QueryTermVariableSetsLength(Vec<char>, usize, Option<usize>),
}

fn production_pattern_item(input: &str) -> IResult<&str, Vec<Constraint>> {
    let (input, m) = alt((
        map(
            tuple((
                tag("/"),
                many1(one_of("abcdefghijklmnopqrstuvwxyz.*ABCDEFG")),
            )),
            |(_, fodder)| {
                vec![Anagram(
                    fodder
                        .clone()
                        .into_iter()
                        .filter(|c| !['*', '.'].contains(c))
                        .collect(),
                    fodder
                        .clone()
                        .into_iter()
                        .filter(|&c| c == '.')
                        .map(|_| "abcdefghijklmnopqrstuvwxyz".chars().collect())
                        .collect(),
                    fodder.contains(&'*'),
                )]
            },
        ),
        value(
            vec![LiteralFrom("abcdefghijklmnopqrstuvwxyz".chars().collect())],
            tag("."),
        ),
        value(vec![LiteralFrom("aeiou".chars().collect())], tag("@")),
        value(vec![Anagram(vec![], vec![], true)], tag("*")),
        value(
            vec![LiteralFrom("bcdfghjklmnpqrstvwxyz".chars().collect())],
            tag("#"),
        ),
        map(one_of("abcdefghijklmnopqrstuvwxyz"), |c| vec![Literal(c)]),
        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |c| vec![Variable(c)]),
        map(
            delimited(tag("("), production_patttern_term, tag(")")),
            |o| match o {
                QueryTerm::QueryTermPattern(p) => p.items,
                _ => panic!("Parenthetical terms can only be patterns"),
            },
        ),
    ))(input)?;

    Ok((input, m))
}
fn production_varsingle_length_constraint(input: &str) -> IResult<&str, QueryTerm> {
    let (input, result) =
        delimited(tag("|"), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag("|"))(input)?;

    let (input, len_min): (&str, usize) = map(tuple((tag("="), digit1)), |v| {
        FromStr::from_str(v.1).unwrap()
    })(input)?;

    let (input, len_max): (&str, Option<Option<usize>>) =
        opt(map(tuple((tag("-"), opt(digit1))), |v| {
            v.1.map(|v| FromStr::from_str(v).ok().unwrap())
        }))(input)?;

    Ok((
        input,
        QueryTerm::QueryTermVariableLength(
            result,
            len_min,
            match len_max {
                None => Some(len_min),
                Some(v) => v,
            },
        ),
    ))
}

fn production_variables_length_constraint(input: &str) -> IResult<&str, QueryTerm> {
    let (input, result) = delimited(
        tag("|"),
        many_m_n(2, 26, one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
        tag("|"),
    )(input)?;

    let (input, len_min): (&str, usize) = map(tuple((tag("="), digit1)), |v| {
        FromStr::from_str(v.1).unwrap()
    })(input)?;

    let (input, len_max): (&str, Option<Option<usize>>) =
        opt(map(tuple((tag("-"), opt(digit1))), |v| {
            v.1.map(|v| FromStr::from_str(v).ok().unwrap())
        }))(input)?;

    Ok((
        input,
        QueryTerm::QueryTermVariableSetsLength(
            result,
            len_min,
            match len_max {
                None => Some(len_min),
                Some(v) => v,
            },
        ),
    ))
}

fn production_vardifferent_constraint(input: &str) -> IResult<&str, QueryTerm> {
    let (input, varnames) =
        preceded(tag("!="), many1(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ")))(input)?;
    Ok((input, QueryTerm::QueryTermVariablesInequality(varnames)))
}

fn production_patttern_term(input: &str) -> IResult<&str, QueryTerm> {
    let (input, t1) = many1(production_pattern_item)(input)?;
    let t1: Vec<_> = t1.into_iter().flatten().collect();

    let (input, items) = fold_many0(
        pair(alt((tag("&"), tag("|"))), many1(production_pattern_item)),
        || t1.clone(),
        |acc, b| {
            let b_items: Vec<_> = b.1.into_iter().flatten().collect();
            if b.0 == "&" {
                vec![Conjunction((Pattern::new(acc), Pattern::new(b_items)))]
            } else {
                vec![Disjunction((Pattern::new(acc), Pattern::new(b_items)))]
            }
        },
    )(input)?;

    Ok((input, QueryTerm::QueryTermPattern(Pattern::new(items))))
}

fn parse_query_terms(input: &str) -> Vec<QueryTerm> {
    separated_list1(
        tag(";"),
        alt((
            production_patttern_term,
            production_varsingle_length_constraint,
            production_variables_length_constraint,
            production_vardifferent_constraint,
        )),
    )(&input)
    .unwrap()
    .1
}
pub fn parse(input: &str) -> Query {
    // let patternm = parser::production_patttern_term(&input).unwrap();
    let terms = parse_query_terms(input);

    let patterns: Vec<Pattern> = terms
        .iter()
        .filter_map(|p| {
            if let QueryTerm::QueryTermPattern(pattern) = p {
                Some(pattern)
            } else {
                None
            }
        })
        .cloned()
        .collect();

    Query {
        parts: patterns.clone(),
        variables: terms
            .into_iter()
            .filter_map(|p| {
                if let QueryTerm::QueryTermVariableLength(var, lenmin, lenmax) = p {
                    Some((
                        var,
                        VariableSpec {
                            len_min: Some(lenmin),
                            len_max: lenmax,
                        },
                    ))
                } else {
                    None
                }
            })
            .chain(patterns.iter().flat_map(|p| -> Vec<_> {
                p.vars()
                    .iter()
                    .map(|&v| {
                        (
                            v,
                            VariableSpec {
                                len_min: Some(1),
                                len_max: None,
                            },
                        )
                    })
                    .collect()
            }))
            .fold(HashMap::new(), |mut acc, b| {
                if !acc.iter().any(|c| c.0 == &b.0) {
                    acc.insert(b.0, b.1);
                }
                acc
            }),
    }
}

use std::{
    env,
    ops::{Deref, Index, IndexMut},
    time::SystemTime,
};

use crate::dict::{SUBSTRINGS, WORDS};
use rustc_hash::FxHashSet;

use std::str;

#[derive(Clone, Debug, PartialEq, Eq)]
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
        Anagram(fixed, extras, false) => {
            let f = fixed.len() + extras.len();
            (f, f)
        }

        &Variable(v) => {
            if let Some(b) = bindings[v] {
                (b.len(), b.len())
            } else {
                (spec[v].0, spec[v].1)
            }
        }
        Disjunction((a, b)) => {
            let arange = a
                .items
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            let brange = b
                .items
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            (arange.0.min(brange.0), arange.1.max(brange.1))
        }
        Conjunction((a, b)) => {
            let arange = a
                .items
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            let brange = b
                .items
                .iter()
                .map(|i| length_range(i, bindings, spec))
                .fold((0, 0), |acc, v| (acc.0 + v.0, acc.1 + v.1));
            (arange.0.max(brange.0), arange.1.min(brange.1))
        }
        Anagram(_, _, true) => (0, 200),
    }
}

pub struct ExecutionContextBuilder<'a> {
    candidate_stack: Vec<&'a str>,
    bindings: VariableMap<Option<&'a str>>,
    patterns: Vec<Vec<Constraint>>,
    spec_var_length: VariableMap<(usize, usize)>,
    spec_var_inequality: Vec<Vec<char>>,
    spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    active: bool,
    count: usize,
    sum: usize,
}
impl<'a> ExecutionContextBuilder<'a> {
    fn new(
        patterns: Vec<Vec<Constraint>>,
        spec_var_length: VariableMap<(usize, usize)>,
        spec_var_inequality: Vec<Vec<char>>,
        spec_var_sets_length: Vec<(Vec<char>, usize, usize)>,
    ) -> Self {
        ExecutionContextBuilder {
            candidate_stack: vec![],
            bindings: VariableMap::default(),
            patterns,
            spec_var_length,
            spec_var_inequality,
            spec_var_sets_length,
            active: true,
            count: 0,
            sum: 0,
        }
    }

    pub fn build<'b>(
        self,
        callback: &'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<&'a str>>) -> bool,
    ) -> ExecutionContext<'a, 'b> {
        ExecutionContext {
            active: self.active,
            bindings: self.bindings,
            candidate_stack: self.candidate_stack,
            count: self.count,
            patterns: self.patterns,
            spec_var_inequality: self.spec_var_inequality,
            spec_var_length: self.spec_var_length,
            spec_var_sets_length: self.spec_var_sets_length,
            sum: self.sum,
            subexpr_binding_stack: vec![],
            callback,
        }
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
    count: usize,
    sum: usize,
    subexpr_binding_stack: Vec<Vec<VariableMap<Option<&'a str>>>>,
    callback: &'b mut dyn FnMut(&Vec<&'a str>, &VariableMap<Option<&'a str>>) -> bool,
}

impl<'a, 'b, 'c> ExecutionContext<'a, 'b> {
    fn nested_constraints_execute(&mut self, candidate: &'a str, pattern: &[Constraint]) {
        if candidate.len() == 0
            && pattern.len() == 0
            && self.patterns.len() == self.candidate_stack.len()
            && self.subexpr_binding_stack.len() == 0
        {
            self.active = (*self.callback)(&self.candidate_stack, &self.bindings);
            return;
        }

        // if self.subexpr_binding_stack.len() > 0 {
        // println!("Subs {:?} {:?}", candidate, pattern);
        // }
        if candidate.len() == 0 && pattern.len() == 0 && self.subexpr_binding_stack.len() > 0 {
            self.subexpr_binding_stack
                .last_mut()
                .unwrap()
                .push(self.bindings.clone());
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
                            probe += self.bindings[*v].unwrap();
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

            return self.execute(next_candidates);
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
            // println!("{:?}; {}@{:?}->{:?}", &self.candidate_stack, candidate, pattern, streak_length_bound);
            if !self.active {
                return;
            }
            let streak_end = (streak_start + streak_len).min(candidate.len());
            let remainder_len = candidate.len().saturating_sub(streak_len);
            let remainder_start = if anchored_left { streak_end } else { 0 };
            let remainder_end = remainder_start + remainder_len;

            match &pattern[constraint_index] {
                Disjunction((a, b)) => {
                    let sub_candidate = &candidate[streak_start..streak_end];
                    let bindings_before_disjunction: VariableMap<Option<&str>> = self.bindings.clone();

                    let a_pattern = &a.items[..];
                    self.subexpr_binding_stack.push(vec![]);
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
                    let b_pattern = &b.items[..];
                    self.subexpr_binding_stack.push(vec![]);
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

                    let a_pattern = &a.items[..];
                    self.subexpr_binding_stack.push(vec![]);
                    self.nested_constraints_execute(sub_candidate, a_pattern);
                    let result_a = self.subexpr_binding_stack.pop().unwrap();

                    let b_pattern = &b.items[..];
                    for a_binding in result_a {
                        self.subexpr_binding_stack.push(vec![]);
                        self.bindings = a_binding;
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
                    // // println!("Var {:?} {:?} {:?} {:?}", candidate_stack, candidate, v, bindings);
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
                Anagram(fixed, extras, _open) => {
                    let mut streak_chars =
                        candidate[streak_start..streak_end].chars().collect_vec();

                    for f in fixed {
                        if let Some(pos) = streak_chars.iter().position(|c| c == f) {
                            streak_chars.remove(pos);
                        } else {
                            continue 'streaks;
                        }
                    }

                    let matching_extras = streak_chars
                        .iter()
                        .permutations(extras.len())
                        .find(|remainders| {
                            remainders
                                .iter()
                                .zip(extras)
                                .all(|(r, allowed_chars)| allowed_chars.contains(r))
                        })
                        .is_some();

                    if !matching_extras {
                        continue 'streaks;
                    }

                    self.nested_constraints_execute(
                        &candidate[remainder_start..remainder_end],
                        &pattern[if anchored_left {
                            1..pattern.len()
                        } else {
                            0..pattern.len() - 1
                        }],
                    );
                }
                _ => todo!(),
            }
        }
    }

    pub fn execute<T>(&'c mut self, candidates: impl Iterator<Item = T>)
    where
        T: Deref<Target = &'a str>,
    {
        let pattern_depth = self.candidate_stack.len();
        for c in candidates {
            self.candidate_stack.push(&*c);
            // println!("Recurign into next {}", patterns.len());
            let this_pattern = self.patterns.get(pattern_depth).unwrap().clone();
            self.nested_constraints_execute(*c, &this_pattern);
            self.candidate_stack.pop();
            if !self.active {
                return;
            }
        }
    }
}
pub fn parser_exec(q: &str) -> ExecutionContextBuilder<'static> {
    let query_terms = parse_query_terms(&q);

    let patterns = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermPattern(p) => Some(p.items.clone()),
            _ => None,
        })
        .collect_vec();

    let variable_length = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermVariableLength(c, a, b) => Some((c, a, b.unwrap_or(255))),
            _ => None,
        })
        .collect_vec();


    fn mention(c: &Constraint) -> Vec<char> {
        match c {
            Variable(c) => vec![*c],
            Disjunction((a,b)) => [&a.items, &b.items].iter().flat_map(|cs|cs.iter().flat_map(|c| mention(c).into_iter())).collect(),
            _=> vec![]
        }
    }
    let variables_mentioned = patterns
        .iter()
        .flat_map(|i| {
            i.iter().flat_map(|c| mention(c))
        })
        .fold(VariableMap::<(usize, usize)>::default(), |mut vm, v| {
            vm[v] = (1, 255);
            vm
        });

    let variables_constrained =
        variable_length
            .iter()
            .fold(variables_mentioned, |mut acc, &(c, a, b)| {
                acc[*c] = (*a, b);
                acc
            });

    let variable_sets_length = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermVariableSetsLength(vs, a, b) => {
                Some((vs.clone(), *a, b.unwrap_or(255)))
            }
            _ => None,
        })
        .collect_vec();

    let variable_inquality = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermVariablesInequality(vs) => Some(vs.clone()),
            _ => None,
        })
        .collect_vec();

    ExecutionContextBuilder::new(
        patterns,
        variables_constrained,
        variable_inquality,
        variable_sets_length,
    )
}

pub mod tests {
    use super::*;
    #[test]
    fn parser_test() {
        let parg: String = env::args().last().unwrap();
        q("start");
        println!("{:?}", parse(&parg));
        println!("lenrange {:?}", parse(&parg).parts[0].len_range());
        let now = SystemTime::now();
        let result = q(&parg);
        let elapsed = now.elapsed();
        println!("Result: {}", result);
        println!("Processing: {:?} -- #{}", elapsed, result.lines().count());
    }

    #[test]
    fn executort() {
        let parg: String = env::args().last().unwrap();
        println!("{:?}", parse_query_terms(&parg));
        let word_count = SUBSTRINGS.len();
        println!("Total words: {}", word_count);

        let mut results = vec![];

        let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<&str>>| {
            println!(
                "Bound sol'n {:?} @{:?}",
                a,
                b.0.iter().cloned().filter_map(|v| v).collect_vec()
            );
            results.push(a.iter().map(|v| v.to_string()).collect_vec());
            results.len() < 1000
        };

        let t0 = SystemTime::now();
        let mut ctx = parser_exec(&parg).build(&mut cb);

        ctx.execute(WORDS.iter()/*.filter(|w|w == &&"tripping")*/);
        println!("Found {} results in {:?}", results.len(), t0.elapsed());
    }
}
