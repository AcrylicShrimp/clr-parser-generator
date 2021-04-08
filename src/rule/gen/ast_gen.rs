use super::super::rule_table::*;
use std::collections::HashMap;

fn resolve_type_rule_item(rule_name: &str, item: &RuleItem) -> String {
    match item {
        RuleItem::Terminal(..) => "String".to_owned(),
        RuleItem::NonTerminal(non_ternimal) => {
            format!("Box<AST{}>", {
                non_ternimal
                    .split("-")
                    .map(|token| {
                        let mut chars = token.chars();
                        match chars.next() {
                            Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                            None => String::new(),
                        }
                    })
                    .collect::<String>()
            })
        }
        RuleItem::Option(items) => format!("Option<{}>", {
            let types = items.iter().map(|item| resolve_type_rule_item(rule_name, item)).collect::<Vec<_>>();
            format!("({},)", types.join(", "))
        }),
        RuleItem::Repeat(items) => format!("Vec<{}>", {
            let types = items.iter().map(|item| resolve_type_rule_item(rule_name, item)).collect::<Vec<_>>();
            format!("({},)", types.join(", "))
        }),
    }
}

fn resolve_type_rule(rule: &Rule) -> String {
    let types = rule.items.iter().map(|item| resolve_type_rule_item(&rule.name, item)).collect::<Vec<_>>();
    format!("({},)", types.join(", "))
}

pub fn generate_ast(rule_table: &RuleTable) -> String {
    let mut root = "".to_owned();
    let mut enums = HashMap::<String, Vec<(String, String)>>::new();

    for rule in rule_table.iter() {
        if rule.name == "__root" {
            // The root non-terminal is alias of tuple.
            root = resolve_type_rule(rule);
        } else {
            // Internal rules are not generating enums(inlined).
            let rule_name = {
                rule.name
                    .split("-")
                    .map(|token| {
                        let mut chars = token.chars();
                        match chars.next() {
                            Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                            None => String::new(),
                        }
                    })
                    .collect::<String>()
            };

            let members = enums.entry(format!("AST{}", rule_name)).or_default();

            members.push((
                {
                    rule.reduce_name
                        .split("-")
                        .map(|token| {
                            let mut chars = token.chars();
                            match chars.next() {
                                Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                                None => String::new(),
                            }
                        })
                        .collect::<String>()
                },
                resolve_type_rule(rule),
            ));
        }
    }

    format!(
        r#"pub type AST = {};

{}"#,
        root,
        enums
            .into_iter()
            .map(|(name, members)| {
                format!(
                    r#"pub enum {} {{
	{}
}}"#,
                    name,
                    members.into_iter().map(|member| format!("{}{},", member.0, member.1)).collect::<Vec<_>>().join("\n\t")
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n"),
    )
}
