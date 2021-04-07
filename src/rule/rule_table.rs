use super::lexer::*;

pub type RuleTable = Vec<Rule>;

#[derive(Clone, Debug)]
pub struct Rule {
    pub name: String,
    pub reduce_name: String,
    pub items: Vec<RuleItem>,
}

#[derive(Clone, Debug)]
pub enum RuleItem {
    Terminal(String),
    NonTerminal(String),
    Option(Vec<RuleItem>),
    Repeat(Vec<RuleItem>),
}

pub fn parse_rule_table(mut lexer: Lexer) -> RuleTable {
    let mut rule_table = RuleTable::new();

    loop {
        match next(&mut lexer) {
            Some(rule) => rule_table.push(rule),
            None => break,
        }
    }

    rule_table
}

fn next(lexer: &mut Lexer) -> Option<Rule> {
    let name = lexer.next(true);

    if name.token_type == Type::EoF {
        return None;
    }

    if name.token_type != Type::Terminal {
        panic!("name expected, got [{:?}: {}]", name.token_type, name.token_content);
    }

    let equal = lexer.next(true);

    if equal.token_type != Type::Equal {
        panic!("equal expected, got [{:?}: {}]", equal.token_type, equal.token_content);
    }

    let reduce_name = lexer.next(true);

    if reduce_name.token_type != Type::Terminal {
        panic!("reduce name expected, got [{:?}: {}]", reduce_name.token_type, reduce_name.token_content);
    }

    let colon = lexer.next(true);

    if colon.token_type != Type::Colon {
        panic!("colon expected, got [{:?}: {}]", colon.token_type, colon.token_content);
    }

    let mut rule = Rule {
        name: name.token_content,
        reduce_name: reduce_name.token_content,
        items: Vec::new(),
    };

    let first_item = next_item(lexer);

    match first_item {
        Some(item) => {
            rule.items.push(item);
        }
        None => {
            let token = lexer.next(true);

            panic!("terminal or non-terminal or open-brace or open-bracket expected, got [{:?}: {}]", token.token_type, token.token_content)
        }
    }

    loop {
        match next_item(lexer) {
            Some(item) => rule.items.push(item),
            None => {
                let token = lexer.next(true);

                if token.token_type != Type::Semicolon {
                    panic!("semicolon expected, got [{:?}: {}]", token.token_type, token.token_content)
                }

                break;
            }
        }
    }

    Some(rule)
}

fn next_item(lexer: &mut Lexer) -> Option<RuleItem> {
    let token = lexer.next(false);

    match token.token_type {
        Type::Semicolon | Type::BraceR | Type::BracketR => None,
        Type::Terminal => Some(RuleItem::Terminal(lexer.next(true).token_content)),
        Type::NonTerminal => Some(RuleItem::NonTerminal(lexer.next(true).token_content)),
        Type::BraceL => {
            lexer.next(true);

            let mut items = Vec::new();

            loop {
                let item = next_item(lexer);

                match item {
                    Some(item) => items.push(item),
                    None => {
                        let token = lexer.next(true);

                        if token.token_type != Type::BraceR {
                            panic!("close-brace expected, got [{:?}: {}]", token.token_type, token.token_content)
                        }

                        break;
                    }
                }
            }

            Some(RuleItem::Repeat(items))
        }
        Type::BracketL => {
            lexer.next(true);

            let mut items = Vec::new();

            loop {
                let item = next_item(lexer);

                match item {
                    Some(item) => items.push(item),
                    None => {
                        let token = lexer.next(true);

                        if token.token_type != Type::BracketR {
                            panic!("close-bracket expected, got [{:?}: {}]", token.token_type, token.token_content)
                        }

                        break;
                    }
                }
            }

            Some(RuleItem::Option(items))
        }
        _ => panic!("terminal or non-terminal or open-brace or open-bracket expected, got [{:?}: {}]", token.token_type, token.token_content),
    }
}
