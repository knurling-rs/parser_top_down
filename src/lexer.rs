//! This lexer is intended to be used with Ferrous Systems flip-link
#![allow(warnings)]
use std::iter::{Enumerate, Peekable};
use std::ops::Range;
use std::str::Chars;
pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_kind: TokenKind,
    pub span: Span,
    pub line_number: usize,
}
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Plus,
    Minus,
    Colon,
    SemiColon,
    CurlyClose,
    CurlyOpen,
    Equal,
    Number(u64),
    Word(String),
    Comma,
    Dot,
    ParClose,
    ParOpen,
}

impl Token {
    pub fn test_new(k: TokenKind) -> Self {
        Token {
            token_kind: k,
            span: 0..0,
            line_number: 0,
        }
    }
}
fn is_delimiter(c: Option<char>) -> bool {
    if let Some(c) = c {
        match c {
            ' ' | ',' | '\n' | '}' | '{' | ':' | '(' | ')' | ';' => true,
            _ => false,
        }
    } else {
        // None case
        true
    }
}
pub fn lexer(script: &str) -> Vec<Token> {
    let mut tokens = Vec::new();

    // ATM flip-link is using a line number to write
    // back to the original linker script.
    let mut line_number = 0;

    let mut it = script.chars().enumerate().peekable();
    while let Some((index, ch)) = it.next() {
        let mut push_token = |token_kind, span| {
            tokens.push(Token {
                token_kind,
                span,
                line_number,
            })
        };
        let mut push_char = |token_kind| push_token(token_kind, index..index + 1);

        match ch {
            ':' => push_char(TokenKind::Colon),
            ',' => push_char(TokenKind::Comma),
            '{' => push_char(TokenKind::CurlyOpen),
            '}' => push_char(TokenKind::CurlyClose),
            '=' => push_char(TokenKind::Equal),
            '(' => push_char(TokenKind::ParOpen),
            ')' => push_char(TokenKind::ParClose),
            '.' => push_char(TokenKind::Dot),
            ';' => push_char(TokenKind::SemiColon),
            ' ' | '\t' | '\r' => {
                // nothing to do with whitespaces atm
                // if we implement start and stop positions
                // we will need to increment  in that block
            }
            '\n' => {
                line_number += 1;
            }
            '/' => {
                // Multiblock comment
                if let Some((_, '*')) = it.peek() {
                    // eating the '*' character
                    let _ = advance_while(&mut it, |c| *c == '*');
                    let stop = advance_while(&mut it, |c| *c == '/').unwrap_or(script.len());
                    // Findex all new lines in the multiline comment
                    line_number += script[index..stop].chars().filter(|c| *c == '\n').count();
                }
                // One line comment
                if let Some((_, '/')) = it.peek() {
                    // must iterate until /n (end of line)
                    let _ = advance_while(&mut it, |ch| *ch == '\n');
                }
            }
            '+' => push_char(TokenKind::Plus),
            '-' => push_char(TokenKind::Minus),
            // Assuming that hex number always start with a 0, and not an "x"!
            '0'..='9' => {
                let mut from = index;
                let radix = if let Some((_, 'x')) = it.peek() {
                    // Consume the "x"
                    it.next();
                    from += 2;
                    16
                } else {
                    10
                };
                // to should depend on the value of the radix
                let to = if radix == 16 {
                    advance_while(it.by_ref(), |c: &char| !c.is_ascii_hexdigit())
                        .unwrap_or(script.len())
                } else {
                    advance_while(it.by_ref(), |c: &char| !c.is_digit(10)).unwrap_or(script.len())
                };

                // Tighten up error management at this stage, or by the parser? What about negative numbers etc.
                // map returns None without panicking
                let next_char = it.peek().map(|(u, c)| *c);
                if dbg!(is_delimiter(next_char)) {
                    let number = u64::from_str_radix(&script[from..to], radix).unwrap();
                    push_token(TokenKind::Number(number), index..to);
                } else {
                    let mut index_u: Option<usize> = None;
                    // keeps a state of the iteration
                    // peek --> reference into that state
                    // end that borrow into current state before next
                    // next updates the state
                    while let Some((u, c)) = it.peek() {
                        //256K_
                        if is_delimiter(Some(*c)) {
                            // need to end the u borrow before it.next()
                            index_u = Some(*u);
                            // Unquote for infinite loop
                            break;
                        }
                        it.next();
                        //125something\n
                        //125_
                    }
                    if let Some(index_u) = index_u {
                        push_token(
                            TokenKind::Word(script[from..index_u].to_string()),
                            (from..index_u),
                        );
                    } else {
                        push_token(
                            TokenKind::Word(script[from..script.len()].to_string()),
                            (from..script.len()),
                        );
                    }
                }
            }
            'a'..='z' | 'A'..='Z' => {
                let from = index;

                //a0a a 0 a
                while let Some((u, c)) = it.peek() {
                    // c is borrowing the iterator
                    if is_delimiter(Some(*c)) {
                        break;
                    }
                    // c relases the iterator
                    it.next();
                }
                let to = it.peek().map(|(u, c)| *u);

                if to.is_some() {
                    push_token(
                        TokenKind::Word(script[from..to.unwrap()].to_string()),
                        index..to.unwrap(),
                    )
                } else {
                    push_token(
                        TokenKind::Word(script[from..script.len()].to_string()),
                        index..script.len(),
                    )
                }
            }
            // to be decided: substraction? division? multiplication ..?
            _ => {
                continue;
            }
        }
    }

    tokens
}

/// This function advances in the script until it hits a
/// predicate, and returns the index.
fn advance_while(
    it: &mut Peekable<Enumerate<Chars<'_>>>,
    pred: fn(&char) -> bool,
) -> Option<usize> {
    let stop = loop {
        let &(idx, ch) = match it.peek() {
            Some(it) => it,
            None => return None,
        };
        if pred(&ch) {
            break idx;
        }
        it.next();
    };
    Some(stop)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn empty_string() {
        let empty: Vec<Token> = Vec::new();
        assert_eq!(lexer(""), empty);
    }

    #[test]
    fn one_space() {
        let empty: Vec<Token> = Vec::new();
        assert_eq!(lexer(" "), empty);
    }

    #[test]
    fn multiple_spaces() {
        let empty: Vec<Token> = Vec::new();
        assert_eq!(lexer("     "), empty);
    }

    #[test]
    fn multiple_spaces_and_tab() {
        let empty: Vec<Token> = Vec::new();
        assert_eq!(lexer("    \t "), empty);
    }
    #[test]
    fn one_zero() {
        assert_eq!(lexer("0"), vec![create_token_number(0, 0, 1, 0)]);
    }

    #[test]
    fn lexer_number_and_unit_K() {
        assert_eq!(vec![create_token_word("0K", 0, 2, 0)], lexer("0K"));
    }

    #[test]
    fn lexer_word_starting_with_zero() {
        assert_eq!(vec![create_token_word("0abc", 0, 4, 0)], lexer("0abc"));
    }

    // fix that, it starts with a letter and then quit the branch
    #[test]
    fn word_with_letters_and_numbers() {
        assert_eq!(vec![create_token_word("a0a", 0, 3, 0)], lexer("a0a"));
    }
    #[test]
    fn word_with_letters_and_numbers_ends_comma() {
        assert_eq!(
            vec![
                create_token_word("a0a", 0, 3, 0),
                Token {
                    token_kind: TokenKind::Comma,
                    span: 3..4,
                    line_number: 0
                }
            ],
            lexer("a0a,")
        );
    }

    #[test]
    fn semicolon() {
        assert_eq!(
            vec![Token {
                token_kind: TokenKind::SemiColon,
                span: 0..1,
                line_number: 0
            }],
            lexer(";")
        );
    }

    #[test]
    fn semicolon_and_origin() {
        assert_eq!(
            vec![
                Token {
                    token_kind: TokenKind::Word("ORIGIN".to_string()),
                    span: 0..6,
                    line_number: 0
                },
                Token {
                    token_kind: TokenKind::SemiColon,
                    span: 6..7,
                    line_number: 0
                }
            ],
            lexer("ORIGIN;")
        );
    }

    #[test]
    fn semicolon_and_number() {
        assert_eq!(
            vec![
                Token {
                    token_kind: TokenKind::Number(0),
                    span: 0..1,
                    line_number: 0
                },
                Token {
                    token_kind: TokenKind::SemiColon,
                    span: 1..2,
                    line_number: 0
                }
            ],
            lexer("0;")
        );
    }

    #[test]
    fn word_with_ram_and_colon() {
        assert_eq!(
            vec![
                create_token_word("RAM", 0, 3, 0),
                Token {
                    token_kind: TokenKind::Colon,
                    span: 3..4,
                    line_number: 0
                }
            ],
            lexer("RAM:")
        );
    }
    #[test]
    fn zero_with_space_before() {
        assert_eq!(lexer(" 0"), vec![create_token_number(0, 1, 2, 0)]);
    }

    #[test]
    fn zero_with_space_after() {
        assert_eq!(lexer("0 "), vec![create_token_number(0, 0, 1, 0)]);
    }
    #[test]
    fn number_with_space_after() {
        assert_eq!(lexer("12 "), vec![create_token_number(12, 0, 2, 0)]);
    }

    #[test]
    fn zero_with_space_before_and_after() {
        assert_eq!(lexer(" 0 "), vec![create_token_number(0, 1, 2, 0)]);
    }

    #[test]
    fn tab_with_space_and_zero() {
        assert_eq!(lexer("  \t  0     "), vec![create_token_number(0, 5, 6, 0)]);
    }

    #[test]
    fn number_one() {
        assert_eq!(lexer("1"), vec![create_token_number(1, 0, 1, 0)]);
    }

    #[test]
    fn number_twelve() {
        assert_eq!(lexer("12"), vec![create_token_number(12, 0, 2, 0)]);
    }

    #[test]
    fn one_and_seven_zeroes() {
        assert_eq!(lexer("00000001"), vec![create_token_number(1, 0, 8, 0)]);
    }

    #[test]
    fn zero_with_hex() {
        assert_eq!(lexer("0x0"), vec![create_token_number(0, 0, 3, 0)]);
    }

    #[test]
    fn one_with_hex() {
        assert_eq!(lexer("0x1"), vec![create_token_number(1, 0, 3, 0)]);
    }

    #[test]
    fn hex_23() {
        assert_eq!(lexer("0x0017"), vec![create_token_number(0x0017, 0, 6, 0)]);
    }

    #[test]
    fn more_hex_0x0Af17() {
        assert_eq!(lexer("0x0Af17"), vec![create_token_number(0xaf17, 0, 7, 0)]);
    }
    #[test]
    fn words_and_hex_num() {
        let three_elems: Vec<Token> = vec![
            create_token_word("nx", 0, 2, 0),
            create_token_word("nx", 3, 5, 0),
            create_token_number(1, 6, 9, 0),
        ];

        assert_eq!(lexer("nx nx 0x1"), three_elems);
    }

    #[test]
    fn empty_comment_zero() {
        let test_new_lines = "
//
0";
        assert_eq!(lexer(test_new_lines), vec![create_token_number(0, 4, 5, 2)]);
    }

    #[test]
    fn more_comment_zero() {
        let test_new_lines = "
// some comment
0";
        assert_eq!(
            lexer(test_new_lines),
            vec![create_token_number(0, 17, 18, 2)]
        );
    }

    #[test]
    fn comments_before_and_after_zero() {
        let test_new_lines = "
// some comment
0
// some comment
// more comment";
        assert_eq!(
            lexer(test_new_lines),
            vec![create_token_number(0, 17, 18, 2)]
        );
    }

    #[test]
    fn very_unclear_comment() {
        let test_new_lines = "
/*
Very unclear comment
*/
0";
        assert_eq!(
            lexer(test_new_lines),
            vec![create_token_number(0, 28, 29, 4)]
        );
    }

    #[test]
    fn comment_with_star() {
        assert_eq!(lexer("/**/"), Vec::new());
    }
    #[test]
    fn comment_start() {
        assert_eq!(lexer("//"), Vec::new());
    }

    #[test]
    fn number_and_unit_K() {
        let number_and_unit = "256K";
        assert_eq!(
            lexer(number_and_unit),
            vec![create_token_word("256K", 0, 4, 0)]
        );
    }

    #[test]
    fn two_numbers() {
        let number_and_unit = "256, 368";
        assert_eq!(
            lexer(number_and_unit),
            vec![
                create_token_number(256, 0, 3, 0),
                Token {
                    token_kind: TokenKind::Comma,
                    span: (3..4),
                    line_number: 0,
                },
                create_token_number(368, 5, 8, 0)
            ]
        );
    }

    // make test as minus
    #[test]
    fn numbers_and_comma() {
        let number_and_unit = "
256
,
368";
        assert_eq!(
            lexer(number_and_unit),
            vec![
                create_token_number(256, 1, 4, 1),
                Token {
                    token_kind: TokenKind::Comma,
                    span: (5..6),
                    line_number: 2,
                },
                create_token_number(368, 7, 10, 3)
            ]
        );
    }

    #[test]
    fn phrase_on_3_lines() {
        const LINKER_SCRIPT: &str = "MEMORY
PROBLEMS
";

        let expected = vec![
            create_token_word("MEMORY", 0, 6, 0),
            create_token_word("PROBLEMS", 7, 15, 1),
        ];
        assert_eq!(lexer(LINKER_SCRIPT), expected);
    }

    #[test]
    fn test_openpar_r_closepar() {
        const LINKER_SCRIPT: &str = "(r)";
        let expected = vec![
            Token {
                token_kind: TokenKind::ParOpen,
                span: (0..1),
                line_number: 0,
            },
            Token {
                token_kind: TokenKind::Word("r".to_string()),
                span: (1..2),
                line_number: 0,
            },
            Token {
                token_kind: TokenKind::ParClose,
                span: (2..3),
                line_number: 0,
            },
        ];

        assert_eq!(expected, lexer(&LINKER_SCRIPT));
    }
    #[test]
    fn memory_text() {
        const LINKER_SCRIPT: &str = "MEMORY
{
    FLASH : ORIGIN = 0x00000000, LENGTH = 256K
}
";
        let expected = vec![
            create_token_word("MEMORY", 0, 6, 0),
            Token {
                token_kind: TokenKind::CurlyOpen,
                span: (7..8),
                line_number: 1,
            },
            create_token_word("FLASH", 13, 18, 2),
            Token {
                token_kind: TokenKind::Colon,
                span: (19..20),
                line_number: 2,
            },
            create_token_word("ORIGIN", 21, 27, 2),
            Token {
                token_kind: TokenKind::Equal,
                span: (28..29),
                line_number: 2,
            },
            create_token_number(0, 30, 40, 2),
            Token {
                token_kind: TokenKind::Comma,
                span: (40..41),
                line_number: 2,
            },
            create_token_word("LENGTH", 42, 48, 2),
            Token {
                token_kind: TokenKind::Equal,
                span: (49..50),
                line_number: 2,
            },
            create_token_word("256K", 51, 55, 2),
            Token {
                token_kind: TokenKind::CurlyClose,
                span: (56..57),
                line_number: 3,
            },
        ];

        assert_eq!(expected, lexer(LINKER_SCRIPT));
    }

    #[test]
    fn lmem_linker() {
        const LINKER_SCRIPT: &str = "MEMORY
LINKER.x
";

        let expected = vec![
            create_token_word("MEMORY", 0, 6, 0),
            create_token_word("LINKER.x", 7, 15, 1),
        ];

        assert_eq!(expected, lexer(LINKER_SCRIPT));
    }

    fn create_token_number(number: u64, from: usize, to: usize, line: usize) -> Token {
        Token {
            token_kind: TokenKind::Number(number),
            span: from..to,
            line_number: line,
        }
    }
    fn create_token_word(string: &str, from: usize, to: usize, line: usize) -> Token {
        Token {
            token_kind: TokenKind::Word(string.to_string()),
            span: from..to,
            line_number: line,
        }
    }
}
