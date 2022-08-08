use std::ops::Sub;

#[allow(unused_imports)]
use crate::*;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::one_of,
    combinator::{map, map_res, value},
    multi::{fold_many0, many0, many1, separated_list0, separated_list1},
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
        ), // map(separated_list0(tag("|"), production_pattern_item), |l| {
           //     Disjunction(l)
           // })
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

#[cfg(test)]
mod tests {
    use std::time::SystemTime;

    use super::*;

    #[test]
    fn parser_test() {
        let dictionary: Vec<String> = dict::UKACD17.lines().map(|f| f.to_string()).collect();

        let dictionary: Vec<Production> = dictionary
            .iter()
            .map(|l| l.to_lowercase().replace(" ", "").replace("'", ""))
            .filter(|l| l.len() <= 30)
            .map(|l| Production {
                string: l.chars().collect(),
                bindings: BTreeMap::new(),
            })
            .collect();

        let mut s = String::new();

        let patternm = parser::production_patttern_term("ca(/et*)ring").unwrap();
        let pattern = patternm.1.clone();
        println!("{:?}", patternm);
        let q = Query {
            parts: vec![pattern.clone()],
            variables: pattern
                .vars()
                .iter()
                .map(|v| {
                    (
                        *v,
                        VariableSpec {
                            len_min: None,
                            len_max: None,
                        },
                    )
                })
                .collect(),
        };
        let now = SystemTime::now();
        let result = q.execute(&dictionary);
        let elapsed = now.elapsed();
        for (i, r) in result.iter().enumerate() {
            let rep = r.iter().map(|p| p.string.iter().join("")).join(";");
            writeln!(&mut s, "{:?}", rep).unwrap();
        }
        println!("Result: {}", s);
        println!("Result: {}", result.len());
        println!("Processing: {:?}", elapsed);
    }
}
