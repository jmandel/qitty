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
    QueryTermConstraints(Vec<Constraint>),
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
                QueryTerm::QueryTermConstraints(p) => p,
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
                vec![Conjunction((acc, b_items))]
            } else {
                vec![Disjunction((acc, b_items))]
            }
        },
    )(input)?;

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
pub fn parser_exec(q: &str) -> ExecutionContextBuilder<'static> {
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
            Disjunction((a,b)) => [&a, &b].iter().flat_map(|cs|cs.iter().flat_map(|c| mention(c).into_iter())).collect(),
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
