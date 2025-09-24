use logos::Logos;

use crate::query::QueryFragment;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Query {
    Field { name: String, quoted: bool },
    Index(usize),
    CollectArray,
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
        let message = match got {
            Some(token) => format!("{start}..{end}: expected {expected:?}, got {token:?}"),
            None => format!("{start}..{end}: expected {expected:?}, got Error"),
        };

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

pub fn parse(input: &str) -> (QueryFragment, Vec<ParseError>) {
    let mut tokens = Token::lexer(input);
    let mut queries = vec![];
    let mut errors = vec![];

    loop {
        match tokens.next() {
            Some(Ok(Token::Dot)) => {}
            None => break,
            Some(token) => {
                errors.push(ParseError::expected_token(
                    tokens.span().start,
                    tokens.span().end,
                    Token::Dot,
                    token.ok(),
                ));
                continue;
            }
        }

        match tokens.next() {
            Some(Ok(Token::OpenBracket)) => {
                let start = tokens.span().start;
                let inner = {
                    let mut inner = vec![];
                    let mut closed = false;
                    while let Some(token) = tokens.next() {
                        match token {
                            Ok(Token::CloseBracket) => {
                                closed = true;
                                break;
                            }
                            Ok(token) => {
                                inner.push((token, tokens.slice()));
                            }
                            Err(()) => {
                                errors.push(ParseError::expected_other(
                                    start,
                                    tokens.span().end,
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
                            tokens.span().end,
                            "an ']'",
                            "Error",
                        ));
                        break;
                    }
                    inner
                };
                let end = tokens.span().end;

                match inner.as_slice() {
                    [(Token::Index, slice)] => queries.push(Query::Index(slice.parse().unwrap())),
                    [(Token::QuotedField, slice)] => {
                        let len = slice.len();
                        assert_eq!(&slice[0..1], "\"");
                        assert_eq!(&slice[len - 1..], "\"");
                        queries.push(Query::Field {
                            name: from_quoted(&slice[1..len - 1]),
                            quoted: true,
                        });
                    }
                    [] => queries.push(Query::CollectArray),
                    [(token, _), ..] => {
                        errors.push(ParseError::expected_other(
                            start,
                            end,
                            "an index or a quoted field inside indexing",
                            &format!("{token:?}"),
                        ));
                    }
                }
            }
            Some(Ok(Token::Field)) => queries.push(Query::Field {
                name: tokens.slice().into(),
                quoted: false,
            }),
            None => {
                errors.push(ParseError::expected_other(
                    tokens.span().start,
                    tokens.span().end,
                    "'[' or an identifier",
                    "EOF",
                ));
                break;
            }
            Some(Ok(token)) => {
                errors.push(ParseError::expected_other(
                    tokens.span().start,
                    tokens.span().end,
                    "'[' or an identifier",
                    &format!("{token:?}"),
                ));
                break;
            }
            Some(Err(())) => {
                errors.push(ParseError::expected_other(
                    tokens.span().start,
                    tokens.span().end,
                    "'[' or an identifier",
                    "Error",
                ));
                break;
            }
        }
    }

    let fragment =
        queries
            .into_iter()
            .rev()
            .fold(QueryFragment::accept(), |rest, query| match query {
                Query::Field { name, quoted } => QueryFragment::field(name, quoted, rest),
                Query::Index(index) => QueryFragment::index_array(index, rest),
                Query::CollectArray => QueryFragment::collect_array(rest),
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
                QueryFragment::accept()
            )
        );
        assert!(errors.is_empty());

        let (query, errors) = parse(r#".[1]"#);
        assert_eq!(
            query,
            QueryFragment::index_array(1, QueryFragment::accept())
        );
        assert!(errors.is_empty());
    }
}
