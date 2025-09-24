use logos::{Lexer, Logos};

use crate::query::QueryFragment;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Query {
    Field {
        name: String,
        quoted: bool,
        optional: bool,
    },
    Index {
        value: usize,
        optional: bool,
    },
    CollectArray {
        optional: bool,
    },
}

impl Query {
    fn set_optional(&mut self) {
        match self {
            Self::Field { optional, .. } => {
                *optional = true;
            }
            Self::Index { optional, .. } => {
                *optional = true;
            }
            Self::CollectArray { optional } => {
                *optional = true;
            }
        }
    }
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip r"[ \t\n\f]+")]
enum Token {
    #[token(r#"."#)]
    Dot,
    #[token(r#"["#)]
    OpenBracket,
    #[token(r#"]"#)]
    CloseBracket,
    #[token(r#"?"#)]
    QuestionMark,
    #[regex(r#"[a-zA-Z_][0-9a-zA-Z_]*"#)]
    Field,
    // https://github.com/maciejhirsz/logos/issues/133#issuecomment-619444615
    #[regex(r#""(?:[^"]|\\")*""#)]
    QuotedField,
    #[regex(r#"[0-9]+"#)]
    Index,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    fn expected_token(start: usize, end: usize, expected: Token, got: Option<Token>) -> Self {
        let message = got.map_or_else(
            || format!("{start}..{end}: expected {expected:?}, got Error"),
            |token| format!("{start}..{end}: expected {expected:?}, got {token:?}"),
        );

        Self { message }
    }

    fn expected_other(start: usize, end: usize, expected: &str, got: &str) -> Self {
        Self {
            message: format!("{start}..{end}: expected {expected}, got {got}"),
        }
    }
}

// very simple handling of escape sequences:
// \<c> is treated as <c>
fn from_quoted(quoted: &str) -> String {
    let mut ret = String::with_capacity(quoted.len());
    let mut escape = false;

    for c in quoted.chars() {
        if c == '\\' && !escape {
            escape = true;
            continue;
        }
        escape = false;
        ret.push(c);
    }

    ret
}

fn read_dotted_queries(lexer: &mut Lexer<Token>) -> (Vec<Query>, Vec<ParseError>) {
    let mut queries = vec![];
    let mut errors = vec![];

    if let Some(token) = lexer.next() {
        match token {
            Ok(Token::Dot) => {
                read_queries(lexer, &mut queries, &mut errors);
            }
            other => {
                errors.push(ParseError::expected_token(
                    lexer.span().start,
                    lexer.span().end,
                    Token::Dot,
                    other.ok(),
                ));

                return read_dotted_queries(lexer);
            }
        }
    }

    (queries, errors)
}

fn read_queries(lexer: &mut Lexer<Token>, queries: &mut Vec<Query>, errors: &mut Vec<ParseError>) {
    if let Some(mut query) = read_query(lexer, errors) {
        if let Some(token) = lexer.next() {
            match token {
                Ok(Token::Dot) => {
                    queries.push(query);
                    read_queries(lexer, queries, errors);
                }
                Ok(Token::QuestionMark) => {
                    query.set_optional();
                    queries.push(query);

                    if let Some(token) = lexer.next() {
                        match token {
                            Ok(Token::Dot) => read_queries(lexer, queries, errors),
                            other => {
                                errors.push(ParseError::expected_token(
                                    lexer.span().start,
                                    lexer.span().end,
                                    Token::Dot,
                                    other.ok(),
                                ));
                            }
                        }
                    }
                }
                other => {
                    errors.push(ParseError::expected_token(
                        lexer.span().start,
                        lexer.span().end,
                        Token::Dot,
                        other.ok(),
                    ));
                }
            }
        } else {
            queries.push(query);
        }
    }
}

fn read_query(lexer: &mut Lexer<Token>, errors: &mut Vec<ParseError>) -> Option<Query> {
    match lexer.next() {
        None => {
            errors.push(ParseError::expected_other(
                lexer.span().start,
                lexer.span().end,
                "'[' or an identifier",
                "EOF",
            ));

            None
        }
        Some(Err(())) => {
            errors.push(ParseError::expected_other(
                lexer.span().start,
                lexer.span().end,
                "'[' or an identifier",
                "Error",
            ));

            None
        }
        Some(Ok(Token::Field)) => Some(Query::Field {
            name: lexer.slice().into(),
            quoted: false,
            optional: false,
        }),
        Some(Ok(Token::OpenBracket)) => read_bracketed(lexer, errors),
        Some(Ok(other)) => {
            errors.push(ParseError::expected_other(
                lexer.span().start,
                lexer.span().end,
                "'[' or an identifier",
                &format!("{other:?}"),
            ));

            None
        }
    }
}

fn read_bracketed(lexer: &mut Lexer<Token>, errors: &mut Vec<ParseError>) -> Option<Query> {
    let start = lexer.span().start;
    let inner = {
        let mut inner = vec![];
        let mut closed = false;
        while let Some(token) = lexer.next() {
            match token {
                Ok(Token::CloseBracket) => {
                    closed = true;
                    break;
                }
                Ok(token) => {
                    inner.push((token, lexer.slice()));
                }
                Err(()) => {
                    errors.push(ParseError::expected_other(
                        start,
                        lexer.span().end,
                        "an ']'",
                        "Error",
                    ));
                    break;
                }
            }
        }
        if !closed {
            errors.push(ParseError::expected_other(
                start,
                lexer.span().end,
                "an ']'",
                "Error",
            ));
            return None;
        }
        inner
    };

    let end = lexer.span().end;

    match inner.as_slice() {
        [(Token::Index, slice)] => Some(Query::Index {
            value: slice.parse().unwrap(),
            optional: false,
        }),
        [(Token::QuotedField, slice)] => {
            let len = slice.len();
            assert_eq!(&slice[0..1], "\"");
            assert_eq!(&slice[len - 1..], "\"");
            Some(Query::Field {
                name: from_quoted(&slice[1..len - 1]),
                quoted: true,
                optional: false,
            })
        }
        [] => Some(Query::CollectArray { optional: false }),
        [(token, _), ..] => {
            errors.push(ParseError::expected_other(
                start,
                end,
                "an index or a quoted field inside indexing",
                &format!("{token:?}"),
            ));
            None
        }
    }
}

pub fn parse(input: &str) -> (QueryFragment, Vec<ParseError>) {
    let mut lexer = Token::lexer(input);

    let (queries, errors) = read_dotted_queries(&mut lexer);

    let fragment =
        queries
            .into_iter()
            .rev()
            .fold(QueryFragment::accept(), |rest, query| match query {
                Query::Field {
                    name,
                    quoted,
                    optional,
                } => QueryFragment::field(name, quoted, optional, rest),
                Query::Index { value, optional } => {
                    QueryFragment::index_array(value, optional, rest)
                }
                Query::CollectArray { optional } => QueryFragment::collect_array(optional, rest),
            });
    (fragment, errors)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lexer() {
        use Token::*;

        let lexer = Token::lexer(r#". [ "kubernetes_clusters" ].id.[0 ]"#);
        let tokens: Vec<Token> = lexer.collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(
            tokens,
            [
                Dot,
                OpenBracket,
                QuotedField,
                CloseBracket,
                Dot,
                Field,
                Dot,
                OpenBracket,
                Index,
                CloseBracket
            ]
        );
    }

    #[test]
    fn parser() {
        let (query, errors) = parse(r#".["field name with spaces"]"#);
        assert_eq!(
            query,
            QueryFragment::field(
                "field name with spaces".into(),
                true,
                false,
                QueryFragment::accept()
            )
        );
        assert!(errors.is_empty());

        let (query, errors) = parse(r#".[1]"#);
        assert_eq!(
            query,
            QueryFragment::index_array(1, false, QueryFragment::accept())
        );
        assert!(errors.is_empty());

        let (query, errors) = parse(r#".[1]?.abc"#);
        assert_eq!(
            query,
            QueryFragment::index_array(
                1,
                true,
                QueryFragment::field("abc".to_string(), false, false, QueryFragment::accept())
            )
        );
        assert!(errors.is_empty());
    }
}
