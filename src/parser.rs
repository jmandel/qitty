use core::num;
use std::{ops::Sub, str::FromStr};

#[allow(unused_imports)]
use crate::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of},
    combinator::{map, opt, value},
    multi::{fold_many0, many1, separated_list1},
    number,
    sequence::{delimited, pair, tuple},
    IResult,
};

#[derive(Clone, Debug)]
enum QueryTerm {
    QueryTermPattern(Pattern),
    QueryTermVariableLength(char, usize, Option<usize>),
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
fn production_varconstraint(input: &str) -> IResult<&str, QueryTerm> {
    let (input, result) =
        delimited(tag("|"), one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), tag("|"))(input)?;

    let (input, len_min): (&str, usize) = map(tuple((tag("="), digit1)), |v| {
        FromStr::from_str(v.1).unwrap()
    })(input)?;

    let (input, len_max): (&str, Option<Option<usize>>) = opt(map(tuple((tag("-"), opt(digit1))), |v| {
        v.1.map(|v|FromStr::from_str(v).ok().unwrap())
    }))(input)?;

    Ok((
        input,
        QueryTerm::QueryTermVariableLength(result, len_min, match len_max {
            None => Some(len_min),
            Some(v) => v
        })
    ))
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
                vec![Conjunction((
                    Pattern { items: acc },
                    Pattern { items: b_items },
                ))]
            } else {
                vec![Disjunction((
                    Pattern { items: acc },
                    Pattern { items: b_items },
                ))]
            }
        },
    )(input)?;

    Ok((input, QueryTerm::QueryTermPattern(Pattern { items })))
}

pub fn parse(input: &str) -> Query {
    // let patternm = parser::production_patttern_term(&input).unwrap();
    let patterns = separated_list1(
        tag(";"),
        alt((production_patttern_term, production_varconstraint)),
    )(&input)
    .unwrap()
    .1;
    let parts: Vec<Pattern> = patterns
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
        parts: parts.clone(),
        variables: patterns
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
            .chain(parts.iter().flat_map(|p| -> Vec<_> {
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

#[cfg(test)]
mod tests {
    use std::{env, time::SystemTime};

    use super::*;

    #[test]
    fn parser_test() {
        let parg: String = env::args().last().unwrap();
        q("start");
        println!("{:?}", parse(&parg));
        let now = SystemTime::now();
        let result = q(&parg);
        let elapsed = now.elapsed();
        println!("Result: {}", result);
        println!("Processing: {:?} -- #{}", elapsed, result.lines().count());
    }
}
