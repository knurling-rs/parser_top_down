// TODO:
// 1. Change lexer to keep number and unit together DONE
// 2. Handle units in expression DONE
// 3. Parse attributes (rx..) DONE
// 4. Refactoring DONE [x]
// 5. Error handling
// 6. Alternative syntax origin ORG o and length...
// 7. Sections

//use std::assert_matches;

use std::{iter::Peekable, vec::IntoIter};

use lexer::Token;

use crate::lexer::TokenKind;

// heap allocated string to be matched

mod lexer;
#[derive(Debug, PartialEq)]
pub struct LinkerScript {
    // This is not recursive so this is not a problem
    // Child of Expr can be Expr and it makes it recursive
    commands: Vec<Command>,
}

#[derive(Debug, PartialEq)]
enum Command {
    Memory { regions: Vec<Region> },
    Sections,
}

#[derive(Debug, PartialEq)]
struct Region {
    id: String,
    attribute: Vec<Attribute>,
    origin: Expr,
    length: Expr,
}
#[derive(Debug, PartialEq)]
struct Expr {
    expr_kind: ExprKind,
}
#[derive(Debug, PartialEq)]
enum Attribute {
    Read,
    Write,
    Execute,
    Allocatable,
    Initialized,
    Invert,
}

#[derive(Debug, PartialEq)]
enum ExprKind {
    Number(u64),
    NumberUnit(u64, Unit),
    BinExpr(Box<Expr>, Op, Box<Expr>),
}
#[derive(Debug, PartialEq)]
enum Unit {
    K,
    M,
}
#[derive(Debug, PartialEq)]
pub enum Error {
    UnmatchingBrace,
    UnexpectedToken,
    UnexpectedEoF,
}
#[derive(Debug, PartialEq)]
enum Op {
    Plus,
    Minus,
}
// #[derive(Debug, PartialEq)]
// struct Memory;

// #[derive(Debug, PartialEq)]
// struct Sections;

// Type name is name of enum
// parse explicitely pub so all types need explicit pub
pub fn parse(str: &str) -> Result<LinkerScript, Error> {
    let tokens = lexer::lexer(str);
    // println!("Printing tokens {:#?}", tokens);

    // consumes the value
    // dbg!(tokens);
    // dbg consumes the value and returns it right away for evaluation
    let mut commands = vec![];

    // into_iter() consummes
    let mut it = tokens.into_iter().peekable();

    while let Some(t) = it.next() {
        match t.token_kind {
            TokenKind::Word(w) => {
                // instead of heap allocated string,
                // we match on a str slice
                // & is &String - &*w would be a &str (string slice)
                // &w[..] --> another way to get a string slice

                // let command = match w.as_str() {
                //     "MEMORY" => match parse_memory(&mut it) {
                //         Ok(mem) => mem,
                //         // early return
                //         Err(e) => return Err(e),
                //     },
                // ====>
                // changing control flow is ^^ not typed checked ^^

                let command = match w.as_str() {
                    "MEMORY" => parse_memory(&mut it)?,
                    "SECTIONS" => Command::Sections,
                    _ => unreachable!("Unexpected command"),
                };

                commands.push(command);
                // no need to explicit continue, as there are stuff after the match expression
            }
            _ => continue,
        }
    }
    Ok(LinkerScript { commands })
}

fn parse_memory(it: &mut Peekable<IntoIter<Token>>) -> Result<Command, Error> {
    assert!(it.next().unwrap().token_kind == TokenKind::CurlyOpen);
    let mut regions = Vec::new();

    match it.peek() {
        Some(token) => {
            if token.token_kind == TokenKind::CurlyClose {
                return Ok(Command::Memory { regions });
            }
        }
        None => return Err(Error::UnmatchingBrace),
    }

    // MEMORY {}
    while let Some(t) = it.next() {
        let region = parse_region(t, it)?;
        regions.push(region);

        // if let TokenKind::CurlyClose = it.peek().unwrap().token_kind {
        //     it.next();
        //     break;
        // }

        if matches!(it.peek().unwrap().token_kind, TokenKind::CurlyClose) {
            it.next();
            break;
        }

        // skip_while consummes and it is a problem at the next while let
        // same than while let Some(t2) = it.next() {
    }

    Ok(Command::Memory { regions })
}

fn parse_region(t: Token, it: &mut Peekable<IntoIter<Token>>) -> Result<Region, Error> {
    let id = match &t.token_kind {
        TokenKind::Word(w) => w,
        _ => return Err(Error::UnexpectedToken),
    };
    let vec_attribute = parse_attributes(it);
    match it.next() {
        Some(t) => {
            if t.token_kind != TokenKind::Colon {
                return Err(Error::UnexpectedToken);
                // when no other branch, evaluates to ()
            }
        }
        None => return Err(Error::UnexpectedEoF),
    }
    //assert_eq!(it.next().unwrap().token_kind, TokenKind::Colon);
    assert!(matches!(
        it.next().unwrap().token_kind,
        TokenKind::Word { .. }
    ));
    assert_eq!(it.next().unwrap().token_kind, TokenKind::Equal);
    let origin = parse_expr(it);
    assert_eq!(it.next().unwrap().token_kind, TokenKind::Comma);
    assert!(matches!(
        it.next().unwrap().token_kind,
        TokenKind::Word { .. }
    ));
    assert_eq!(it.next().unwrap().token_kind, TokenKind::Equal);
    let length = parse_expr(it);

    Ok(Region {
        id: id.to_string(),
        attribute: vec_attribute,
        origin,
        length,
    })
}

// USE replace qualified with use
fn parse_attributes(it: &mut Peekable<IntoIter<Token>>) -> Vec<Attribute> {
    let mut attributes: Vec<Attribute> = vec![];
    // option of a token kind, still an option
    // map applies a function to the some variant
    if it.peek().map(|t| &t.token_kind) == Some(&TokenKind::ParOpen) {
        // go to letters
        it.next();
        let t3 = dbg!(it.next().unwrap());
        assert!(matches!(dbg!(&t3.token_kind), TokenKind::Word { .. }));
        let a_chars = match &t3.token_kind {
            TokenKind::Word(w) => w.chars(),
            _ => unreachable!("should be one word"),
        };
        for c in a_chars {
            let attribute = match c {
                'r' | 'R' => Attribute::Read,
                'w' | 'W' => Attribute::Write,
                'x' | 'X' => Attribute::Execute,
                'a' | 'A' => Attribute::Allocatable,
                'i' | 'I' | 'l' | 'L' => Attribute::Initialized,
                '!' => Attribute::Invert,
                _ => unreachable!("unacceptable variant"),
            };
            attributes.push(attribute);

            // add the attribute vector
        }
        assert_eq!(TokenKind::ParClose, it.next().unwrap().token_kind);
    }
    attributes
}

fn parse_expr(it: &mut Peekable<IntoIter<Token>>) -> Expr {
    // assert!(matches!(
    //     it.peek().unwrap().token_kind,
    //     TokenKind::Number { .. }
    // ));
    let origin = match it.next().unwrap().token_kind {
        TokenKind::Number(o) => Expr {
            expr_kind: ExprKind::Number(o),
        },

        TokenKind::Word(s) => {
            dbg!(&s);
            let pos_n = s.chars().position(|c| !c.is_numeric()).unwrap();
            let n = (&s[..pos_n]).parse::<u64>().unwrap();
            let u = match &s[pos_n..] {
                "K" => Unit::K,
                "M" => Unit::M,
                _ => unreachable!("wrong unit"),
            };
            Expr {
                expr_kind: ExprKind::NumberUnit(n, u),
            }
        }
        _ => unreachable!("should be a number"),
    };

    if let Some(t) = it.peek() {
        match &t.token_kind {
            s @ (TokenKind::Plus | TokenKind::Minus) => {
                let s = match s {
                    TokenKind::Plus => Op::Plus,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("Plus or minus are expected"),
                };
                it.next();
                let exp2 = parse_expr(it);
                Expr {
                    expr_kind: ExprKind::BinExpr(Box::new(origin), s, Box::new(exp2)),
                }
            }
            _ => origin,
        }
    } else {
        origin
    }
}

//tmod expands!!
#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn empty_string() {
        let ls = parse("").unwrap();
        assert!(ls.commands.is_empty());
    }

    #[test]
    fn memory_string() {
        let ls = parse("MEMORY {}").unwrap();
        assert_eq!(ls.commands.len(), 1);
        assert!(matches!(ls.commands[0], Command::Memory { .. }));
    }

    #[test]
    fn memory_string_without_closing() {
        let ls_res = parse("MEMORY {");
        assert!(matches!(ls_res, Err(Error::UnmatchingBrace)));
    }

    #[test]
    fn memory_string_double_opening() {
        let ls_res = parse("MEMORY {{");
        assert!(matches!(ls_res, Err(Error::UnexpectedToken)));
    }

    #[test]
    fn two_memory_word_string() {
        let linker_script = parse("MEMORY {} MEMORY {}").unwrap();
        assert_eq!(linker_script.commands.len(), 2);
        // field of enum variant ==> ..
        assert!(matches!(linker_script.commands[0], Command::Memory { .. }));
        assert!(matches!(linker_script.commands[1], Command::Memory { .. }));
    }

    #[test]
    fn section_string() {
        let linker_script = parse("SECTIONS {}").unwrap();
        assert_eq!(linker_script.commands.len(), 1);
        assert!(matches!(
            linker_script.commands[0],
            Command::Sections { .. }
        ));
    }

    #[test]
    fn memory_ram() {
        let ls = parse("MEMORY { RAM: ORIGIN = 1, LENGTH = 2 }").unwrap();
        assert_eq!(ls.commands.len(), 1);

        match &ls.commands[0] {
            Command::Memory { regions } => assert_eq!(regions.len(), 1),
            Command::Sections => unreachable!("This arm should not be unreachable."),
        }
    }

    #[test]
    fn error_on_semi_colon() {
        let ls = parse("MEMORY { RAM; ORIGIN = 1, LENGTH = 2 }");

        assert!(matches!(ls, Err(Error::UnexpectedToken)));
    }

    #[test]
    fn error_on_unexpected_eof() {
        let ls = parse("MEMORY { RAM");

        assert!(matches!(ls, Err(Error::UnexpectedEoF)));
    }

    #[test]
    fn memory_ram_with_comma_and_white_space() {
        let ls = parse("MEMORY { RAM: ORIGIN = 1 , LENGTH = 2 }").unwrap();
        assert_eq!(ls.commands.len(), 1);

        match &ls.commands[0] {
            Command::Memory { regions } => assert_eq!(regions.len(), 1),
            Command::Sections => unreachable!("This arm should not be unreachable."),
        }
    }

    #[test]
    fn memory_ram_2() {
        let ls = parse(
            "
MEMORY 
{ 
    RAM:   ORIGIN = 1, LENGTH = 2 
    RAM:   ORIGIN = 3, LENGTH = 4 
}
",
        )
        .unwrap();
        assert_eq!(ls.commands.len(), 1);

        match ls.commands[0] {
            // Cannot move out an element of the vector
            // indexing with square brackets --> get ref
            Command::Memory { ref regions } => {
                assert_eq!(regions.len(), 2);
                assert_eq!(regions[0].id, "RAM");
                assert_eq!(regions[1].id, "RAM");
            }
            Command::Sections => todo!(),
        }
    }

    #[test]
    fn memory_ram_flash() {
        let ls = parse(
            "
MEMORY 
{ 
    RAM:   ORIGIN = 1K, LENGTH = 2 
    FLASH:   ORIGIN = 3, LENGTH = 4 
}
",
        )
        .unwrap();
        assert_eq!(1, ls.commands.len());

        match ls.commands[0] {
            // Cannot move out an element of the vector
            // indexing with square brackets --> get ref
            Command::Memory { ref regions } => {
                assert_eq!(regions.len(), 2);
                assert_eq!(regions[0].id, "RAM");
                assert_eq!(
                    regions[0].origin,
                    Expr {
                        expr_kind: ExprKind::NumberUnit(1, Unit::K)
                    }
                );
                assert_eq!(
                    regions[0].length,
                    Expr {
                        expr_kind: ExprKind::Number(2)
                    }
                );
                assert_eq!(regions[1].id, "FLASH");
                assert_eq!(
                    regions[1].origin,
                    Expr {
                        expr_kind: ExprKind::Number(3)
                    }
                );
                assert_eq!(
                    regions[1].length,
                    Expr {
                        expr_kind: ExprKind::Number(4)
                    }
                );
            }
            Command::Sections => todo!(),
        }
    }
    //tfn macro
    // ok
    #[test]
    fn parse_expr_number() {
        let tokens: Vec<Token> = vec![Token::test_new(TokenKind::Number(0))];
        let expr = parse_expr(&mut tokens.into_iter().peekable());
        assert_eq!(
            Expr {
                expr_kind: ExprKind::Number(0)
            },
            expr
        );
    }

    #[test]
    fn parse_expr_number_unit_1() {
        let tokens: Vec<Token> = vec![Token::test_new(TokenKind::Word("1K".to_string()))];
        let expr = parse_expr(&mut tokens.into_iter().peekable());
        assert_eq!(
            Expr {
                expr_kind: ExprKind::NumberUnit(1, Unit::K),
            },
            expr
        );
    }

    #[test]
    fn parse_expr_number_unit_2() {
        let tokens: Vec<Token> = vec![Token::test_new(TokenKind::Word("1M".to_string()))];
        let expr = parse_expr(&mut tokens.into_iter().peekable());
        assert_eq!(
            Expr {
                expr_kind: ExprKind::NumberUnit(1, Unit::M),
            },
            expr
        );
    }

    #[test]
    fn parse_expr_number_unit_3() {
        let expr = parse("MEMORY { RAM: ORIGIN = 0x1, LENGTH = 128K}").unwrap();
        match &expr.commands[0] {
            Command::Memory { regions } => match &regions[0].length.expr_kind {
                ExprKind::NumberUnit(n, u) => {
                    assert_eq!(128, *n);
                    assert_eq!(Unit::K, *u);
                }
                ExprKind::Number(_) => unreachable!("This test should produce number unit"),
                ExprKind::BinExpr(_, _, _) => unreachable!("This test should produce number unit"),
            },
            Command::Sections => unreachable!("This test should produce number unit"),
        }
    }

    #[test]
    fn parse_expr_two_numbers_with_plus() {
        let tokens: Vec<Token> = vec![
            Token::test_new(TokenKind::Number(0)),
            Token::test_new(TokenKind::Plus),
            Token::test_new(TokenKind::Number(1)),
        ];
        let expr = parse_expr(&mut tokens.into_iter().peekable());
        assert_eq!(
            Expr {
                expr_kind: ExprKind::BinExpr(
                    Box::new(Expr {
                        expr_kind: ExprKind::Number(0)
                    }),
                    Op::Plus,
                    Box::new(Expr {
                        expr_kind: ExprKind::Number(1)
                    }),
                )
            },
            expr
        )
    }

    // #[test]
    // fn parse_expr_two_numbers_with_minus() {
    //     let tokens: Vec<Token> = vec![
    //         Token::test_new(TokenKind::Number(0)),
    //         Token::test_new(TokenKind::Minus),
    //         Token::test_new(TokenKind::Number(1)),
    //     ];
    //     let expr = parse_expr(&mut tokens.into_assert_eq!(1, &regions[0].attribute.len());
    //                 Box::new(Expr {
    //                     expr_kind: ExprKind::Number(1)
    //                 }),
    //             )
    //         },
    //         expr
    //     )
    // }

    #[test]
    fn parse_expr_with_two_plus() {
        let tokens: Vec<Token> = vec![
            Token::test_new(TokenKind::Number(0)),
            Token::test_new(TokenKind::Plus),
            Token::test_new(TokenKind::Number(1)),
            Token::test_new(TokenKind::Plus),
            Token::test_new(TokenKind::Number(2)),
        ];
        let expr = parse_expr(&mut tokens.into_iter().peekable());
        let expected_exp = Expr {
            expr_kind: ExprKind::BinExpr(
                Box::new(Expr {
                    expr_kind: ExprKind::Number(0),
                }),
                Op::Plus,
                Box::new(Expr {
                    expr_kind: ExprKind::BinExpr(
                        Box::new(Expr {
                            expr_kind: ExprKind::Number(1),
                        }),
                        Op::Plus,
                        Box::new(Expr {
                            expr_kind: ExprKind::Number(2),
                        }),
                    ),
                }),
            ),
        };
        assert_eq!(expected_exp, expr)
    }

    #[test]
    fn memory_ram_r() {
        let expr = parse("MEMORY { RAM (r): ORIGIN = 0, LENGTH = 0}").unwrap();
        match &expr.commands[0] {
            Command::Memory { regions: r } => {
                assert_eq!(1, r[0].attribute.len());
                assert_eq!(Attribute::Read, r[0].attribute[0]);
            }
            _ => unreachable!(""),
        }
    }

    #[test]
    fn memory_ram_rwx() {
        let expr = parse("MEMORY { RAM (rwx): ORIGIN = 0, LENGTH = 0}").unwrap();
        match &expr.commands[0] {
            Command::Memory { regions: r } => {
                assert_eq!(3, r[0].attribute.len());
                assert_eq!(Attribute::Read, r[0].attribute[0]);
                assert_eq!(Attribute::Write, r[0].attribute[1]);
                assert_eq!(Attribute::Execute, r[0].attribute[2]);
            }
            _ => unreachable!(""),
        }
    }
}
