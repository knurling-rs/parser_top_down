use lexer::Token;

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
    Memory { regions: Vec<Region> },
    Sections,
}

#[derive(Debug, PartialEq)]
struct Region {
    id: String,
}

// #[derive(Debug, PartialEq)]
// struct Memory;

// #[derive(Debug, PartialEq)]
// struct Sections;
fn has_regions(it: &mut std::iter::Peekable<std::vec::IntoIter<Token>>) -> bool {
    it.peek().unwrap().token_kind != TokenKind::CurlyClose
}

pub fn parse(str: &str) -> LinkerScript {
    let tokens = lexer::lexer(str);
    // println!("Printing tokens {:#?}", tokens);

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
                        parse_memory(&mut it, &mut commands);
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

fn parse_memory(
    it: &mut std::iter::Peekable<std::vec::IntoIter<Token>>,
    commands: &mut Vec<Command>,
) {
    assert!(it.next().unwrap().token_kind == TokenKind::CurlyOpen);
    if has_regions(it) {
        let mut regions = Vec::new();
        while let Some(t) = it.next() {
            assert!(matches!(t.token_kind, TokenKind::Word { .. }));
            assert_eq!(it.next().unwrap().token_kind, TokenKind::Colon);
            assert!(matches!(
                it.next().unwrap().token_kind,
                TokenKind::Word { .. }
            ));
            assert_eq!(it.next().unwrap().token_kind, TokenKind::Equal);
            assert!(matches!(
                it.next().unwrap().token_kind,
                TokenKind::Number { .. }
            ));
            assert_eq!(it.next().unwrap().token_kind, TokenKind::Comma);
            assert!(matches!(
                it.next().unwrap().token_kind,
                TokenKind::Word { .. }
            ));
            assert_eq!(it.next().unwrap().token_kind, TokenKind::Equal);
            assert!(matches!(
                it.next().unwrap().token_kind,
                TokenKind::Number { .. }
            ));

            let region = Region {
                id: "RAM".to_string(),
            };

            regions.push(region);
            match it.peek().unwrap().token_kind {
                TokenKind::CurlyClose => {
                    it.next();
                    // no push in struct initialisation
                }
                _ => continue,
            }
        }

        commands.push(Command::Memory { regions });

        // skip_while consummes and it is a problem at the next while let
        // same than while let Some(t2) = it.next() {
    } else {
        commands.push(Command::Memory { regions: vec![] });
    }
}
//tmod expands!!
#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn empty_string() {
        let ls = parse("");
        assert!(ls.commands.is_empty());
    }

    #[test]
    fn memory_string() {
        let ls = parse("MEMORY {}");
        assert_eq!(ls.commands.len(), 1);
        assert!(matches!(ls.commands[0], Command::Memory { .. }));
    }

    #[test]
    fn two_memory_word_string() {
        let linker_script = parse("MEMORY {} MEMORY {}");
        assert_eq!(linker_script.commands.len(), 2);
        // field of enum variant ==> ..
        assert!(matches!(linker_script.commands[0], Command::Memory { .. }));
        assert!(matches!(linker_script.commands[1], Command::Memory { .. }));
    }

    #[test]
    fn section_string() {
        let linker_script = parse("SECTIONS {}");
        assert_eq!(linker_script.commands.len(), 1);
        assert!(matches!(
            linker_script.commands[0],
            Command::Sections { .. }
        ));
    }

    #[test]
    fn memory_ram() {
        let ls = parse("MEMORY { RAM: ORIGIN = 1, LENGTH = 2 }");
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
        );
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
}
