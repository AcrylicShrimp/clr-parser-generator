use super::super::normalized_rule_table::*;
use std::collections::HashMap;

struct Member {
    pub name: String,
    pub ty: String,
    pub rule_name: String,
    pub rule_reduce_name: String,
    pub rule_origin_reduce_name: String,
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
    let mut rule_names = HashMap::new();

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
            rule_origin_reduce_name: rule.origin_reduce_name.clone(),
            rule_items: rule.items.clone(),
            rule_types: rule.origin_type.clone(),
        };

        rule_names.entry(rule_name).or_insert((rule.is_internal, Vec::new())).1.push(rule_member);
    }

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
}}

fn raw_ast_to_ast(ast: RawAST) -> AST {{
    let root_ast = match ast {{
        RawAST::_Root(root_ast) => root_ast,
        _ => unreachable!(),
    }};

    {}
}}

{}"#,
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
                        .1
                        .iter()
                        .map(|member| format!("{}{}, // {} {:?}", member.name, member.ty, member.rule_origin_reduce_name, member.rule_types))
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
                members.1.iter().map(move |member| {
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
            .join("\n\t\t"),
        generate_raw_ast_to_ast_logic_root(&rule_names.get("_Root").unwrap().1),
        rule_names
            .iter()
            .filter(|&(name, _)| name != "_Root")
            .map(|(name, members)| if members.0 {
                generate_raw_ast_to_ast_logic_internal(name, &members.1)
            } else {
                generate_raw_ast_to_ast_logic(&rule_names, name, &members.1)
            })
            .collect::<Vec<_>>()
            .join("\n\n")
    )
}

fn generate_raw_ast_to_ast_logic_root(members: &Vec<Member>) -> String {
    format!(
        r#"match root_ast {{
    {}
}}"#,
        members
            .iter()
            .map(|member| {
                format!(
                    "RawAST_Root::{}(ast) => (Box::new({}),),",
                    member.name,
                    match &member.rule_items[0] {
                        NormalizedRuleItem::NonTerminal(non_terminal) => {
                            let name = format!(
                                "{}{}",
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
                            );
                            format!("raw_ast_to_ast_{}(*ast)", name)
                        }
                        _ => unreachable!(),
                    }
                )
            })
            .collect::<Vec<_>>()
            .join("\n\t")
    )
}

fn generate_raw_ast_to_ast_logic(rules: &HashMap<String, (bool, Vec<Member>)>, name: &str, members: &Vec<Member>) -> String {
    format!(
        r#"fn raw_ast_to_ast_{}(ast: RawAST{}) -> AST{} {{
    match ast {{
        {}
    }}
}}"#,
        name,
        name,
        name,
        members
            .iter()
            .map(|member| {
                format!(
                    "RawAST{}::{}({}) => AST{}::{}({}),",
                    name,
                    member.name,
                    (0..get_rule_type_length(&member.rule_types)).map(|index| format!("ast_{}", index)).collect::<Vec<_>>().join(", "),
                    name,
                    member
                        .rule_origin_reduce_name
                        .split(|c| c == '-' || c == '_')
                        .map(|token| {
                            let mut chars = token.chars();
                            match chars.next() {
                                Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                                None => String::new(),
                            }
                        })
                        .collect::<String>(),
                    {
                        fn resolve(mut index: usize, rule_types: &Vec<NormalizedRuleOriginType>, rules: &HashMap<String, (bool, Vec<Member>)>) -> (usize, String) {
                            let result = rule_types
                                .iter()
                                .map(|ty| match ty {
                                    NormalizedRuleOriginType::Terminal => {
                                        let result = format!("ast_{}", index);
                                        index += 1;
                                        result
                                    }
                                    NormalizedRuleOriginType::NonTerminal(non_terminal) => {
                                        let name = format!(
                                            "{}{}",
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
                                        );
                                        let result = format!("Box::new(raw_ast_to_ast_{}(*ast_{}))", name, index);
                                        index += 1;
                                        result
                                    }
                                    NormalizedRuleOriginType::OptionSome(option) => {
                                        let (new_index, result) = resolve(index, option, rules);
                                        index = new_index;
                                        format!("Some(({},))", result)
                                    }
                                    NormalizedRuleOriginType::OptionNone => "None".to_owned(),
                                    NormalizedRuleOriginType::Repeat(repeat) => {
                                        let name = format!(
                                            "{}{}",
                                            if repeat.starts_with("_") { "_" } else { "" },
                                            repeat
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
                                        let result = format!(
                                            r#"{{
    raw_ast_to_ast_{}(*ast_{}).into_iter().map(|item| {{
        ({},)
    }}).collect::<_>()
}}"#,
                                            name,
                                            index,
                                            rules
                                                .get(&name)
                                                .unwrap()
                                                .1
                                                .iter()
                                                .filter(|&member| match &member.rule_types[0] {
                                                    NormalizedRuleOriginType::Repeater(..) => true,
                                                    _ => false,
                                                })
                                                .next()
                                                .unwrap()
                                                .rule_items
                                                .iter()
                                                .enumerate()
                                                .map(|(index, rule_item)| match rule_item {
                                                    NormalizedRuleItem::Terminal(..) => format!("item.{}", index),
                                                    NormalizedRuleItem::NonTerminal(non_terminal) => {
                                                        let name = format!(
                                                            "{}{}",
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
                                                        );
                                                        format!("Box::new(raw_ast_to_ast_{}(*item.{}))", name, index)
                                                    }
                                                })
                                                .collect::<Vec<_>>()
                                                .join(", "),
                                        );
                                        index += 1;
                                        result
                                    }
                                    _ => unreachable!(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ");

                            (index, result)
                        }

                        let (_, result) = resolve(0, &member.rule_types, rules);
                        result
                    }
                )
            })
            .collect::<Vec<_>>()
            .join("\n\t")
    )
}

fn generate_raw_ast_to_ast_logic_internal(name: &str, members: &Vec<Member>) -> String {
    let return_ty = members
        .iter()
        .filter(|&member| match &member.rule_types[0] {
            NormalizedRuleOriginType::Repeater(..) => true,
            _ => false,
        })
        .map(|member| &member.ty)
        .next()
        .unwrap();

    format!(
        r#"fn raw_ast_to_ast_{}(ast: RawAST{}) -> Vec<{}> {{
    let mut result = Vec::new();
    let mut ast = ast;
    
    loop {{
        match ast {{
            {}
        }}
    }}
}}"#,
        name,
        name,
        return_ty,
        members
            .iter()
            .map(|member| {
                format!(
                    r#"RawAST{}::{}({}) => {{
    {}
}}"#,
                    name,
                    member.name,
                    {
                        match &member.rule_types[0] {
                            &NormalizedRuleOriginType::Repeater(length) => (0..length).map(|index| format!("ast_{}", index)).collect::<Vec<_>>().join(", "),
                            &NormalizedRuleOriginType::RepeaterContinue(length) => (0..length).map(|index| format!("ast_{}", index)).collect::<Vec<_>>().join(", "),
                            _ => unreachable!(),
                        }
                    },
                    {
                        match &member.rule_types[0] {
                            &NormalizedRuleOriginType::Repeater(length) => format!(
                                r#"result.push(({},));
                return result;"#,
                                (0..length).map(|index| format!("ast_{}", index)).collect::<Vec<_>>().join(", ")
                            ),
                            NormalizedRuleOriginType::RepeaterContinue(length) => format!(
                                r#"result.push(({},));
                ast = *ast_{};"#,
                                (0..length - 1).map(|index| format!("ast_{}", index)).collect::<Vec<_>>().join(", "),
                                length - 1
                            ),
                            _ => unreachable!(),
                        }
                    }
                )
            })
            .collect::<Vec<_>>()
            .join("\n\t")
    )
}

fn get_rule_type_length(rule_types: &Vec<NormalizedRuleOriginType>) -> usize {
    let mut length = 0;

    for rule_type in rule_types {
        match rule_type {
            NormalizedRuleOriginType::Terminal => {
                length += 1;
            }
            NormalizedRuleOriginType::NonTerminal(..) => {
                length += 1;
            }
            NormalizedRuleOriginType::OptionSome(option) => {
                length += get_rule_type_length(option);
            }
            NormalizedRuleOriginType::OptionNone => {}
            NormalizedRuleOriginType::Repeat(..) => {
                length += 1;
            }
            NormalizedRuleOriginType::Repeater(..) => {}
            NormalizedRuleOriginType::RepeaterContinue(..) => {}
        }
    }

    length
}
