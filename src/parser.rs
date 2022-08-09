use std::ops::Sub;

#[allow(unused_imports)]
use crate::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    combinator::{map, value},
    multi::{fold_many0, many1, separated_list1},
    sequence::{delimited, pair, tuple},
    IResult,
};

fn production_pattern_item(input: &str) -> IResult<&str, Constraint> {
    let (input, m) = alt((
        map(
            tuple((
                tag("/"),
                many1(one_of("abcdefghijklmnopqrstuvwxyz.*ABCDEFG")),
            )),
            |(_, fodder)| {
                Anagram(
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
                )
            },
        ),
        value(SingleChar, tag(".")),
        value(LiteralFrom("aeiou".chars().collect()), tag("@")),
        value(
            LiteralFrom("bcdfghjklmnpqrstvwxyz".chars().collect()),
            tag("#"),
        ),
        map(one_of("abcdefghijklmnopqrstuvwxyz"), |c| Literal(c)),
        map(one_of("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), |c| Variable(c)),
        map(
            delimited(tag("("), production_patttern_term, tag(")")),
            |o| SubPattern(o),
        ),
    ))(input)?;

    Ok((input, m))
}

pub fn production_patttern_term(input: &str) -> IResult<&str, Pattern> {
    let (input, t1) = many1(production_pattern_item)(input)?;

    let (input, p) = fold_many0(
        pair(alt((tag("&"), tag("|"))), many1(production_pattern_item)),
        || t1.clone(),
        |acc, b| {
            if b.0 == "&" {
                vec![Conjunction((
                    Pattern { items: acc },
                    Pattern { items: b.1 },
                ))]
            } else {
                vec![Disjunction((
                    Pattern { items: acc },
                    Pattern { items: b.1 },
                ))]
            }
        },
    )(input)?;

    Ok((input, Pattern { items: p }))
}

pub fn parse(input: &str) -> Query {
    // let patternm = parser::production_patttern_term(&input).unwrap();
    let patterns = separated_list1(tag(";"), production_patttern_term)(&input)
        .unwrap()
        .1;

    Query {
        parts: patterns.clone(),
        variables: patterns
            .into_iter()
            .flat_map(|p| p.vars())
            .unique()
            .map(|v| {
                (
                    v,
                    VariableSpec {
                        len_min: Some(1),
                        len_max: None,
                    },
                )
            })
            .collect(),
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
        println!("Processing: {:?}", elapsed);
    }
}
