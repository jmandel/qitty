use std::{ops::Sub, str::FromStr};

use crate::dict::ALPHABET;
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
    QueryTermConstraints(Vec<Constraint>),
    QueryTermVariableLength(char, usize, Option<usize>),
    QueryTermVariablesInequality(Vec<char>),
    QueryTermVariableSetsLength(Vec<char>, usize, Option<usize>),
}

fn production_pattern_item(input: &str) -> IResult<&str, Vec<Constraint>> {
    let (input, m) = alt((
        map(
            tuple((tag("/*"), many1(production_pattern_item))),
            |(_, avec)| {
                vec![Anagram(
                    true,
                    avec.into_iter().flat_map(|v| v.into_iter()).collect(),
                )]
            },
        ),
        map(
            tuple((tag("/"), many1(production_pattern_item))),
            |(_, avec)| {
                vec![Anagram(
                    false,
                    avec.into_iter().flat_map(|v| v.into_iter()).collect(),
                )]
            },
        ),
        map(
            delimited(
                tag("["),
                pair(
                    opt(tag("!")),
                    many1(alt((
                        map(
                            tuple((
                                one_of("abcdefghijklmnopqrstuvwxyz"),
                                tag("-"),
                                one_of("abcdefghijklmnopqrstuvwxyz"),
                            )),
                            |(c1, _, c2)| {
                                ALPHABET
                                    .iter()
                                    .filter(|l| l >= &&c1 && l <= &&c2)
                                    .copied()
                                    .collect()
                            },
                        ),
                        map(one_of("abcdefghijklmnopqrstuvwxyz"), |v| vec![v]),
                    ))),
                ),
                tag("]"),
            ),
            |(negated, charsets)| {
                let chars = charsets.into_iter().flatten().collect_vec();
                vec![LiteralFrom(match negated {
                    Some(_) => ALPHABET
                        .iter()
                        .filter(|l| !chars.contains(l))
                        .copied()
                        .collect(),
                    None => chars,
                })]
            },
        ),
        value(
            vec![LiteralFrom("abcdefghijklmnopqrstuvwxyz".chars().collect())],
            tag("."),
        ),
        value(vec![LiteralFrom("aeiou".chars().collect())], tag("@")),
        // TODO add back support for >1 `*` across patterns, with a dedicated NonceVariable or `_` indicator or something
        value(vec![Star], tag("*")),
        value(vec![Word(WordDirection::Forwards)], tag(">")),
        value(vec![Word(WordDirection::Backwards)], tag("<")),
        value(
            vec![LiteralFrom("bcdfghjklmnpqrstvwxyz".chars().collect())],
            tag("#"),
        ),
        map(one_of("abcdefghijklmnopqrstuvwxyz"), |c| vec![Literal(c)]),
        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |c| vec![Variable(c)]),
        map(
            delimited(tag("("), production_patttern_term, tag(")")),
            |o| match o {
                QueryTerm::QueryTermConstraints(p) => vec![Subpattern(p)],
                _ => panic!("Parenthetical terms can only be patterns"),
            },
        ),
    ))(input)?;

    Ok((input, m))
}

#[derive(Copy, Clone)]
enum BongeSetting {
    Misprint,
    OptionalMisprint,
}

fn bonge(constraint: Constraint, optional: BongeSetting) -> Constraint {
    match constraint {
        Literal(c) => LiteralFrom(ALPHABET.iter().filter(|l| **l != c).copied().collect()),
        LiteralFrom(cs) => LiteralFrom(
            ALPHABET
                .iter()
                .filter(|l| !cs.contains(l))
                .copied()
                .collect(),
        ),
        Disjunction((a, b)) => {
            let mut abonged = bonge(Subpattern(a), optional);
            abonged = match abonged {
                Subpattern(ab) if ab.len() == 1 => ab[0].clone(),
                _ => abonged,
            };
            let mut bbonged = bonge(Subpattern(b), optional);
            bbonged = match bbonged {
                Subpattern(bb) if bb.len() == 1 => bb[0].clone(),
                _ => bbonged,
            };

            Disjunction((vec![abonged], vec![bbonged]))
        }
        Conjunction((a, b)) => {
            let a = bonge(Subpattern(a), optional);
            let b = bonge(Subpattern(b), optional);
            Conjunction((vec![a], vec![b]))
        }
        Subpattern(vs) => {
            let mut init = Subpattern(if let BongeSetting::OptionalMisprint = optional {
                vs.clone()
            } else {
                vec![]
            });
            init = match init {
                Subpattern(vs) if vs.len() == 1 => vs[0].clone(),
                _ => init,
            };
            vs.iter()
                .enumerate()
                .fold(init, |acc: Constraint, (i, c): (usize, &Constraint)| {
                    let mut bonged_clause = vs.clone();
                    bonged_clause[i] = bonge(c.clone(), optional);
                    Disjunction((vec![acc], bonged_clause))
                })
        }
        Anagram(_, _) | Star | Word(_) | Variable(_) => constraint,
    }
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
    let (input, qualifier) = opt(alt((tag("`"), tag("?`"))))(input)?;

    let (input, t1) = many1(production_pattern_item)(input)?;
    let t1: Vec<_> = t1.into_iter().flatten().collect();

    let (input, mut items) = fold_many0(
        pair(alt((tag("&"), tag("|"))), many1(production_pattern_item)),
        || t1.clone(),
        |acc, b| {
            let b_items: Vec<_> = b.1.into_iter().flatten().collect();
            if b.0 == "&" {
                vec![Conjunction((acc, b_items))]
            } else {
                vec![Disjunction((acc, b_items))]
            }
        },
    )(input)?;

    items = match qualifier {
        None => items,
        Some(q) => match bonge(
            Subpattern(items),
            match q {
                "?`" => BongeSetting::OptionalMisprint,
                "`" => BongeSetting::Misprint,
                _ => panic!(),
            },
        ) {
            Subpattern(vs) => vs,
            c => vec![c],
        },
    };

    Ok((input, QueryTerm::QueryTermConstraints(items)))
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
pub fn parser_exec<'ctx>(q: &str) -> ExecutionContext<'static, 'ctx> {
    let query_terms = parse_query_terms(&q);

    let patterns = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermConstraints(p) => Some(p.clone()),
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
            Disjunction((a, b)) | Conjunction((a, b)) => [&a, &b]
                .iter()
                .flat_map(|cs| cs.iter().flat_map(|c| mention(c).into_iter()))
                .collect(),
            Subpattern(v) | Anagram(_, v) => v.iter().flat_map(|c| mention(c)).collect(),
            Star | Word(_) | Literal(_) | LiteralFrom(_) => vec![],
        }
    }
    let variables_mentioned = patterns
        .iter()
        .flat_map(|i| i.iter().flat_map(|c| mention(c)))
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

    ExecutionContext::new(
        patterns,
        variables_constrained,
        variable_inquality,
        variable_sets_length,
    )
}

pub mod tests {

    use crate::dict::DICTIONARY;
    // rust analyzer seems confused about these being "unused" (or I am ;-))
    #[allow(unused_imports)]
    use crate::{
        dict::{SUBSTRINGS, WORDS},
        parser::{parse_query_terms, parser_exec},
        VariableMap,
    };
    #[allow(unused_imports)]
    use itertools::Itertools;
    #[allow(unused_imports)]
    use std::{env, time::SystemTime};

    #[test]
    fn executort() {
        let parg: String = env::args().last().unwrap();
        println!("{:?}", parse_query_terms(&parg));
        let word_count = SUBSTRINGS.len();
        println!("{} word with {} substrings", DICTIONARY.len(), word_count);

        let mut results = vec![];
        let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<&str>>| {
            let result = a.iter().map(|v| v.to_string()).collect_vec();
            if Some(&result) != results.last() {
                println!(
                    "Bound sol'n {:?} @{:?}",
                    a,
                    b.0.iter().cloned().filter_map(|v| v).collect_vec()
                );

                results.push(result);
            }
            results.len() < 1000
        };

        let t0 = SystemTime::now();
        parser_exec(&parg).execute(WORDS.iter(), &mut cb);
        println!("Found {} results in {:?}", results.len(), t0.elapsed());
    }
}
