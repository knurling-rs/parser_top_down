use crate::lexer::TokenKind;

mod lexer;
#[derive(Debug, PartialEq)]
pub struct LinkerScript {
    // This is not recursive so this is not a problem
    // Child of Expr can be Expr and it makes it recursive
    commands: Vec<Command>,
}

#[derive(Debug, PartialEq)]
enum Command {
    Memory { regions: Option<Regions> },
    Sections,
}

#[derive(Debug, PartialEq)]
struct Regions;

// #[derive(Debug, PartialEq)]
// struct Memory;

// #[derive(Debug, PartialEq)]
// struct Sections;

pub fn parse(str: &str) -> LinkerScript {
    let tokens = lexer::lexer(str);
    // consumes the value
    // dbg!(tokens);
    // dbg consumes the value and returns it right away for evaluation
    let mut commands = vec![];
    if dbg!(tokens.is_empty()) {
        LinkerScript { commands: vec![] }
    } else {
        // into_iter() consummes
        let mut it = tokens.into_iter().peekable();

        while let Some(t) = it.next() {
            match t.token_kind {
                TokenKind::Word(w) => {
                    if w == "MEMORY" {
                        // Open curly
                        assert!(it.next().unwrap().token_kind == TokenKind::CurlyOpen);
                        if it.next().unwrap().token_kind != TokenKind::CurlyClose {
                            commands.push(Command::Memory {
                                regions: Some(Regions),
                            });
                            // skip_while consummes and it is a problem at the next while let
                            // same than while let Some(t2) = it.next() {
                            for t2 in it.by_ref() {
                                if t2.token_kind == TokenKind::CurlyClose {
                                    break;
                                }
                            }
                        } else {
                            commands.push(Command::Memory { regions: None });
                        }
                    } else if w == "SECTIONS" {
                        commands.push(Command::Sections)
                    }
                    // no need to explicit continue, as there are stuff after the match expression
                }

                _ => continue,
            }
        }
        LinkerScript { commands }
    }
}
//tmod expands!!
#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;

    #[test]
    fn empty_string() {
        assert_eq!(LinkerScript { commands: vec![] }, parse(""));
    }

    #[test]
    fn memory_string() {
        assert_eq!(
            LinkerScript {
                commands: vec![Command::Memory { regions: None }]
            },
            parse("MEMORY {}")
        );
    }

    #[test]
    fn two_memory_word_string() {
        assert_eq!(
            LinkerScript {
                commands: vec![
                    Command::Memory { regions: None },
                    Command::Memory { regions: None }
                ]
            },
            parse("MEMORY {} MEMORY {}")
        );
    }

    #[test]
    fn section_string() {
        assert_eq!(
            LinkerScript {
                commands: vec![Command::Sections]
            },
            parse("SECTIONS {}")
        );
    }

    #[test]
    fn memory_ram() {
        assert_eq!(
            LinkerScript {
                commands: vec![Command::Memory {
                    regions: Some(Regions)
                }]
            },
            parse("MEMORY { RAM: ORIGIN = 1, LENGTH = 2 }")
        );
    }
}

// type Span = Range<usize>;

// struct Token {
//     kind: TokenKind,
//     span: Span,
// }

// enum TokenKind {
//     Plus,
//     // ..
// }

// struct LinkerScript {
//     commands: Vec<Command>,
// }

// struct Command {
//     kind: CommandKind,
//     span: Span,
// }

// enum CommandKind {
//     Memory(Memory), // MEMORY { .. }
//     //                 ^           ^ span
//     Sections, // SECTIONS { .. }
// }

// /*
// SECTIONS {
//   _var = 0;

//   .text : var + 1 {

//   } > FLASH

// }
//  */
// struct Memory {
//     span: Span,
//     regions: Vec<Region>, // FLASH (r w x) ORIGIN = 0, LENGTH = 128
//                           // ^                                 ^ span
//                           // ..
// }

// struct Region {
//     name: String,
//     attributes: Attributes,
//     origin: Expr, // 1 + 1
//     length: Expr,
// }

// struct Attributes {
//     span: Span,
//     elements: Vec<Attribute>, // r x
// }

// // check `syn` crate as a reference
// // 1 + 2
// // ^   ^
// // 1 + 2 + 3 + 4 + 5
// // +++++
// // ---------
// // *************
// struct Expr {
//     span: Span,
//     kind: ExprKind,
// }

// enum ExprKind {
//     Symbol(String),
//     Number(u32),        // 128
//     Binary(BinaryExpr), // 1 + 1
//     Paren(ParenExpr),   // (1 + 2)
// }

// // 1 + 2 * 3 -> Bin(+, 1, Bin(*, 2, 3))
// // (1 + 2) * 3 -> Bin(+, Paren(Bin(*, 1, 2)), 3)
// // (2) + 3 -> Bin(+, Paren(2), 3)
// // 2 + 3 -> Bin(+, 2, 3)

// // 1 + 1
// struct BinaryExpr {
//     lhs: Box<Expr>,
//     op: Operator,
//     rhs: Box<Expr>,
// }

// struct ParenExpr {
//     inner: Box<Expr>,

// enum Operator {
//     Plus,
//     Minus,
// }

// pub fn lex(input: &str) -> Vec<Token> {
//     todo!()
// }

// pub fn parse(tokens: Vec<Token>) -> LinkerScript {
//     let tokens = tokens.into_iter();
//     while let Some(token) = tokens {
//         match token {
//             Number(num) => {
//                 let next = tokens.next();
//                 match next {
//                     Some(Token::Operator(op)) => {
//                         // binary expression -> BinaryExpr
//                     }
//                 }
//             }
//         }
//     }
// }
// // parser ^

// // flip-link
// fn compute_ram_region(linker_script: &LinkerScript) {
//     // iterate over regions
//     // look for name == "RAM"
//     // evaluate ORIGIN expr
//     // evaluate LENGTH expr
// }

// fn eval(expr: &Expr) -> u32 {
//     // ..
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn empty_memory() {
//         let input = "MEMORY {}";
//         let tokens = lex(input);
//         let linker_script = parse(tokens);
//         assert_eq!(1, linker_script.commands.len());
//         let command = linker_script.commands[0];
//         match command {
//             Command::Memory(memory) => {
//                 assert!(memory.regions.is_empty());
//             }
//             _ => unreachable!(), // === panic!("this is unreachable")
//         }
//     }

//     //
// }
