use crate::*;

lazy_static! {
    pub(crate) static ref Q_BLUEORANGE: Query = Query {
        parts: vec![
            Pattern {
                items: vec![
                    Variable('A'),
                    Anagram(vec!['o', 'r', 'a', 'n', 'g', 'e'], vec![], true),
                    Variable('B'),
                ],
            },
            Pattern {
                items: vec![
                    Variable('A'),
                    Anagram(vec!['b', 'l', 'u', 'e'], vec![], false),
                    Variable('B'),
                ],
            },
        ],
        variables: HashMap::from_iter([
            (
                'A',
                VariableSpec {
                    len_min: Some(2),
                    len_max: None,
                },
            ),
            (
                'B',
                VariableSpec {
                    len_min: Some(2),
                    len_max: None,
                },
            ),
        ]),
    };
    
    pub(crate) static ref Q_POY: Query = Query {
        parts: vec![
            Pattern {
                items: vec![Literal('p'), Literal('o'), Variable('A'), Literal('y'),],
            },
            Pattern {
                items: vec![Literal('d'), Literal('r'), Literal('a'), Variable('A'),],
            },
        ],
        variables: HashMap::from_iter([(
            'A',
            VariableSpec {
                len_min: Some(1),
                len_max: None,
            },
        ),]),
    };
    
    pub(crate) static ref Q_FEATURES: Query = Query {
        parts: vec![Pattern {
            items: vec![SingleChar, LiteralFrom(vec!['a', 'b', 'c'])],
        },],
        variables: HashMap::from_iter([]),
    };
}

