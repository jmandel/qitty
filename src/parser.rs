use std::{ops::Sub, str::FromStr};

use crate::dict::ALPHABET;
#[allow(unused_imports)]
use crate::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of},
    combinator::{map, map_res, opt, value},
    multi::{fold_many0, many1, many_m_n, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Clone, Debug)]
enum QueryTerm {
    QueryTermConstraints((usize, usize), Constraint),
    QueryTermVariableLength(char, usize, Option<usize>),
    QueryTermVariablesInequality(Vec<char>),
    QueryTermVariableSetsLength(Vec<char>, usize, Option<usize>),
}

fn parse_qat_element(input: &str) -> IResult<&str, Constraint> {
    let (input, reversed) = opt(tag("~"))(input)?;

    let (input, mut item) = alt((
        map(one_of("abcdefghijklmnopqrstuvwxyz"), |c| Literal(c)),
        value(
            LiteralFrom("abcdefghijklmnopqrstuvwxyz".chars().collect()),
            tag("."),
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
                LiteralFrom(match negated {
                    Some(_) => ALPHABET
                        .iter()
                        .filter(|l| !chars.contains(l))
                        .copied()
                        .collect(),
                    None => chars,
                })
            },
        ),
        value(LiteralFrom("aeiou".chars().collect()), tag("@")),
        value(
            LiteralFrom("bcdfghjklmnpqrstvwxyz".chars().collect()),
            tag("#"),
        ),
        value(Star, tag("*")),
        value(Word(WordDirection::Forwards), tag(">")),
        value(Word(WordDirection::Backwards), tag("<")),
        // A digit from 0 to 9 matches any letter, the same one throughout the pattern. Different digits match different letters.
        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |c| Variable(c)),
        delimited(tag("("), parse_qat_compound_pattern, tag(")")),
    ))(input)?;

    item = match reversed {
        Some(_) => Reverse(vec![item]),
        None => item,
    };
    Ok((input, item))
}

fn parse_qat_simple_pattern(input: &str) -> IResult<&str, Constraint> {
    let (input, result) = alt((
        map(
            tuple((
                many1(parse_qat_element),
                preceded(tag("/"), many1(parse_qat_element)),
            )),
            |(seq, ana)| Conjunction((vec![DrawnFromAnagram(ana)], seq)), // TODO add anagram letterbank flag
        ),
        map(many1(parse_qat_element), |seq| {
            if seq.len() == 1 {
                seq[0].clone()
            } else {
                Subpattern(seq)
            }
        }),
        map(preceded(tag("/*"), many1(parse_qat_element)), |ana| {
            Anagram(true, ana)
        }),
        map(preceded(tag("/"), many1(parse_qat_element)), |ana| {
            Anagram(false, ana)
        }),
    ))(input)?;
    Ok((input, result))
}

fn parse_qat_compound_pattern_precedence_2(input: &str) -> IResult<&str, Constraint> {
    let (input, item) = map(
        tuple((opt(tag("!")), parse_qat_simple_pattern)),
        |(negated, item)| {
            if negated.is_some() {
                Negate(match item {
                    Subpattern(vs) => vs,
                    _ => vec![item],
                })
            } else {
                item
            }
        },
    )(input)?;
    Ok((input, item))
}

fn parse_qat_compound_pattern_precedence_1(input: &str) -> IResult<&str, Constraint> {
    let (input, t1) = parse_qat_compound_pattern_precedence_2(input)?;
    let (input, items) = fold_many0(
        preceded(tag("&"), parse_qat_compound_pattern_precedence_2),
        || t1.clone(),
        |acc, vs| {
            Conjunction((
                match acc {
                    Subpattern(items) => items,
                    _ => vec![acc],
                },
                match vs {
                    Subpattern(items) => items,
                    _ => vec![vs],
                },
            ))
        },
    )(input)?;

    Ok((input, items))
}

fn parse_qat_length_qualifier(input: &str) -> IResult<&str, (usize, usize)> {
    let (input, length_qualifier) = opt(alt((
        map(
            terminated(map_res(digit1, |s: &str| s.parse::<usize>()), tag(":")),
            |v| (v, v),
        ),
        map(
            delimited(
                tag("-"),
                map_res(digit1, |s: &str| s.parse::<usize>()),
                tag(":"),
            ),
            |v| (0, v),
        ),
        map(
            terminated(map_res(digit1, |s: &str| s.parse::<usize>()), tag("-:")),
            |v| (v, 255),
        ),
        map(
            terminated(
                separated_pair(
                    map_res(digit1, |s: &str| s.parse::<usize>()),
                    tag("-"),
                    map_res(digit1, |s: &str| s.parse::<usize>()),
                ),
                tag(":"),
            ),
            |(a, b)| (a, b),
        ),
    )))(input)?;

    Ok((input, length_qualifier.unwrap_or((0, 255))))
}

fn parse_qat_compound_pattern(input: &str) -> IResult<&str, Constraint> {
    let (input, misprint_qualifier) = opt(alt((tag("`"), tag("?`"))))(input)?;

    let (input, mut t1) = parse_qat_compound_pattern_precedence_1(input)?;
    let (input, mut item) = fold_many0(
        preceded(tag("|"), parse_qat_compound_pattern_precedence_1),
        || t1.clone(),
        |acc, vs| {
            Disjunction((
                match acc {
                    Subpattern(items) => items,
                    _ => vec![acc],
                },
                match vs {
                    Subpattern(items) => items,
                    _ => vec![vs],
                },
            ))
        },
    )(input)?;

    item = match misprint_qualifier {
        None => item,
        Some(q) => misprint(
            item,
            match q {
                "?`" => MisprintSetting::OptionalMisprint,
                "`" => MisprintSetting::Misprint,
                _ => panic!(),
            },
        ),
    };

    Ok((input, item))
}

use std::{env, time::SystemTime};
#[test]
fn qatcp() {
    let parg: String = env::args().last().unwrap();
    let r = parse_qat_compound_pattern(&parg);
    println!("Parsed to {:?}", r);
}

fn production_pattern_item(input: &str) -> IResult<&str, Vec<Constraint>> {
    let (input, reversal) = opt(tag("~"))(input)?;
    let (input, mut items) = alt((
        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |c| vec![Variable(c)]),
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
    ))(input)?;

    items = match reversal {
        None => items,
        Some(_) => vec![Reverse(items)],
    };

    Ok((input, items))
}

#[derive(Copy, Clone)]
enum MisprintSetting {
    Misprint,
    OptionalMisprint,
}

fn misprint(constraint: Constraint, optional: MisprintSetting) -> Constraint {
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
            let mut abonged = misprint(Subpattern(a), optional);
            abonged = match abonged {
                Subpattern(ab) if ab.len() == 1 => ab[0].clone(),
                _ => abonged,
            };
            let mut bbonged = misprint(Subpattern(b), optional);
            bbonged = match bbonged {
                Subpattern(bb) if bb.len() == 1 => bb[0].clone(),
                _ => bbonged,
            };

            Disjunction((vec![abonged], vec![bbonged]))
        }
        Conjunction((a, b)) => {
            let a = misprint(Subpattern(a), optional);
            let b = misprint(Subpattern(b), optional);
            Conjunction((vec![a], vec![b]))
        }
        Negate(vs) => Negate(vec![misprint(Subpattern(vs), optional)]),
        Reverse(vs) => Reverse(vec![misprint(Subpattern(vs), optional)]),
        Subpattern(vs) => {
            let mut init = Subpattern(if let MisprintSetting::OptionalMisprint = optional {
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
                    bonged_clause[i] = misprint(c.clone(), optional);
                    Disjunction((vec![acc], bonged_clause))
                })
        }
        DrawnFromAnagram(_) | Anagram(_, _) | Star | Word(_) | Variable(_) => constraint,
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

fn parse_query_terms(input: &str) -> Vec<QueryTerm> {
    separated_list1(
        tag(";"),
        alt((
            map(
                tuple((parse_qat_length_qualifier, parse_qat_compound_pattern)),
                |(l, p)| QueryTerm::QueryTermConstraints(l, p),
            ),
            production_varsingle_length_constraint,
            production_variables_length_constraint,
            production_vardifferent_constraint,
        )),
    )(&input)
    .unwrap()
    .1
}

pub fn parser_exec<'a, 'ctx>(q: &str) -> ExecutionContext<'a, 'ctx> {
    let query_terms = parse_query_terms(&q);

    let mut patterns = query_terms
        .iter()
        .filter_map(|t| match t {
            QueryTerm::QueryTermConstraints(l, p) => Some((l.clone(), p.clone())),
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
            Subpattern(v) | Anagram(_, v) | DrawnFromAnagram(v) | Negate(v) | Reverse(v) => {
                v.iter().flat_map(|c| mention(c)).collect()
            }
            Star | Word(_) | Literal(_) | LiteralFrom(_) => vec![],
        }
    }

    patterns = patterns
        .into_iter()
        .sorted_by_key(|p| -(mention(&p.1).len() as isize))
        .collect();
    let variables_mentioned = patterns.iter().flat_map(|i| mention(&i.1)).fold(
        VariableMap::<(usize, usize)>::default(),
        |mut vm, v| {
            vm[v] = (1, 255);
            vm
        },
    );

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
        patterns
            .into_iter()
            .map(|(l, p)| match p {
                Subpattern(vs) => (l, vs),
                v => (l, vec![v]),
            })
            .collect(),
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
        let mut cb = |a: &Vec<&str>, b: &VariableMap<Option<String>>| {
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
