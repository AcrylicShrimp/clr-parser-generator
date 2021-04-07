use super::super::normalized_rule_table::*;
use std::collections::HashMap;

struct Member {
    pub name: String,
    pub ty: String,
    pub rule_name: String,
    pub rule_reduce_name: String,
    pub rule_items: Vec<NormalizedRuleItem>,
    pub rule_types: Vec<NormalizedRuleOriginType>,
}

pub fn resolve_type_rule(rule: &NormalizedRule) -> String {
    format!(
        "({},)",
        rule.items
            .iter()
            .map(|item| match item {
                NormalizedRuleItem::Terminal(..) => "String".to_owned(),
                NormalizedRuleItem::NonTerminal(non_terminal) => format!(
                    "Box<RawAST{}{}>",
                    if non_terminal.starts_with("_") { "_" } else { "" },
                    non_terminal
                        .split(|c| c == '-' || c == '_')
                        .map(|token| {
                            let mut chars = token.chars();
                            match chars.next() {
                                Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                                None => String::new(),
                            }
                        })
                        .collect::<String>()
                ),
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

pub fn generate_raw_ast(rule_table: &NormalizedRuleTable) -> String {
    let mut rule_names = HashMap::<_, Vec<_>>::new();

    for rule in &rule_table.rules {
        let rule_name = format!(
            "{}{}",
            if rule.name.starts_with("_") { "_" } else { "" },
            rule.name
                .split(|c| c == '-' || c == '_')
                .map(|token| {
                    let mut chars = token.chars();
                    match chars.next() {
                        Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                        None => String::new(),
                    }
                })
                .collect::<String>()
        );
        let rule_member = Member {
            name: rule
                .reduce_name
                .split(|c| c == '-' || c == '_')
                .map(|token| {
                    let mut chars = token.chars();
                    match chars.next() {
                        Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                        None => String::new(),
                    }
                })
                .collect::<String>(),
            ty: resolve_type_rule(rule),
            rule_name: rule.name.clone(),
            rule_reduce_name: rule.reduce_name.clone(),
            rule_items: rule.items.clone(),
            rule_types: rule.origin_type.clone(),
        };

        rule_names.entry(rule_name).or_default().push(rule_member);
    }

    // println!("{:#?}", rule_table);

    // 	println!(
    // 		r#"pub enum RawAST {{
    // 	__Token(String),
    // 	{}
    // }}

    // 	println!(
    // 		r#"pub fn build_ast_terminal(token: Token) -> RawAST {{
    // RawAST::__Token(token.token_content)
    // }}"#
    // 	);

    // 	println!(
    // 		r#"pub fn build_ast_nonterminal(name: &str, child_asts: Vec<RawAST>) {{
    // match name {{
    // 	{}
    // }}
    // }}"#, rule_names.iter().map(|(name, ..)| )
    // 	);

    format!(
        r#"#[derive(Debug)]
pub enum RawAST {{
    __Token(String),
	{}
}}

{}

fn build_ast_terminal(token: Token) -> RawAST {{
	RawAST::__Token(token.token_content)
}}

fn build_ast_nonterminal(name: &str, child_asts: Vec<RawAST>) -> RawAST {{
	match name {{
		{}
		_ => unreachable!(),
	}}
}}"#,
        rule_names.iter().map(|(name, ..)| format!("{}(RawAST{}),", name, name)).collect::<Vec<_>>().join("\n\t"),
        rule_names
            .iter()
            .map(|(name, members)| {
                format!(
                    r#"#[derive(Debug)]
pub enum RawAST{} {{
	{}
}}"#,
                    name,
                    members
                        .iter()
                        .map(|member| format!("{}{}, // {:?}", member.name, member.ty, member.rule_types))
                        .collect::<Vec<_>>()
                        .join("\n\t")
                )
            })
            .collect::<Vec<_>>()
            .join("\n\n"),
        rule_names
            .iter()
            .map(|(name, members)| {
                let name = name.clone();
                members.iter().map(move |member| {
                    format!(
                        "\"{}{}\" => {{ let mut it = child_asts.into_iter(); let ast = ({},); RawAST::{}(RawAST{}::{}({})) }}",
                        member.rule_name,
                        member.rule_reduce_name,
                        member.rule_items.iter().map(|_| format!("it.next().unwrap()")).collect::<Vec<_>>().join(", "),
                        name,
                        name,
                        member.name,
                        member
                            .rule_items
                            .iter()
                            .enumerate()
                            .map(|(index, item)| match item {
                                NormalizedRuleItem::Terminal(..) => {
                                    format!("match ast.{} {{ RawAST::__Token(token) => token, _ => unreachable!(), }}", index)
                                }
                                NormalizedRuleItem::NonTerminal(non_terminal) => {
                                    format!(
                                        "match ast.{} {{ RawAST::{}{}(ast) => Box::new(ast), _ => unreachable!(), }}",
                                        index,
                                        if non_terminal.starts_with("_") { "_" } else { "" },
                                        non_terminal
                                            .split(|c| c == '-' || c == '_')
                                            .map(|token| {
                                                let mut chars = token.chars();
                                                match chars.next() {
                                                    Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                                                    None => String::new(),
                                                }
                                            })
                                            .collect::<String>()
                                    )
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                })
            })
            .flatten()
            .collect::<Vec<_>>()
            .join("\n\t\t")
    )
}
