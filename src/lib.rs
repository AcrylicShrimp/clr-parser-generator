mod rule;

use rule::gen::ast_gen::*;
use rule::gen::parser_gen::*;
use rule::gen::raw_ast_gen::*;
use rule::lexer::*;
use rule::normalized_rule_table::*;
use rule::rule_table::*;

// pub fn test_parser(rules: String) {
// 	let rule_table = parse_rule_table(Lexer::new(rules));
// 	println!("{}", generate_ast(&rule_table));
// 	let normalized_rule_table = normalize_rule_table(rule_table);
// }

pub fn generate_all(rules: String) -> String {
    let rule_table = parse_rule_table(Lexer::new(rules));

    let ast = generate_ast(&rule_table);

    let normalized_rule_table = normalize_rule_table(rule_table);

    let raw_ast = generate_raw_ast(&normalized_rule_table);

    let mut gen = Gen::new(normalized_rule_table);

    gen.generate_first();
    gen.generate_dag();
    gen.generate_action_table();

    let action_table = gen.action_table;
    let action_table_len = action_table.len();

    let stringified_action_table = action_table
        .into_iter()
        .map(|action| {
            let mut expected_tokens = action
                .literal
                .keys()
                .filter(|token| !token.is_empty())
                .map(|token| format!("\"{}\".to_owned()", token))
                .collect::<Vec<_>>();
            expected_tokens.sort();
            let expected_tokens = expected_tokens.join(", ");

            format!(
                "(vec![{}].into_iter().collect(), vec![{}].into_iter().collect(), vec![{}])",
                action
                    .literal
                    .into_iter()
                    .map(|(key, value)| format!(
                        "(TokenType::{}, Action::{})",
                        if key.is_empty() {
                            "Eof".to_owned()
                        } else {
                            key.split("-")
                                .map(|token| {
                                    let mut chars = token.chars();
                                    match chars.next() {
                                        Some(ch) => ch.to_uppercase().collect::<String>() + chars.as_str(),
                                        None => String::new(),
                                    }
                                })
                                .collect::<String>()
                        },
                        match value.action_type {
                            ActionType::Accept => "Accept".to_owned(),
                            ActionType::Goto => format!("Goto({})", value.next_state),
                            ActionType::Shift => format!("Shift({})", value.next_state),
                            ActionType::Reduce => {
                                format!("Reduce({}, \"{}\".to_owned(), \"{}\".to_owned())", value.reduce_count, value.rule_name, value.reduce_rule_name)
                            }
                        }
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                action
                    .id
                    .into_iter()
                    .map(|(key, value)| format!(
                        "(\"{}\".to_owned(), Action::{})",
                        key,
                        match value.action_type {
                            ActionType::Accept => "Accept".to_owned(),
                            ActionType::Goto => format!("Goto({})", value.next_state),
                            ActionType::Shift => format!("Shift({})", value.next_state),
                            // This case seems not possible currently;
                            ActionType::Reduce => {
                                format!("Reduce({}, \"{}\".to_owned(), \"{}\".to_owned())", value.reduce_count, value.rule_name, value.reduce_rule_name)
                            }
                        }
                    ))
                    .collect::<Vec<_>>()
                    .join(", "),
                expected_tokens
            )
        })
        .collect::<Vec<_>>()
        .join(",\n\t\t");

    format!(
        r#"#[allow(non_camel_case_types)]
#[allow(non_upper_case_globals)]

use super::lexer::*;
use super::token::*;
use lazy_static::lazy_static;
use std::collections::HashMap;

{}

{}

enum StackItem {{
	AST(RawAST),
	State(usize),
}}

enum Action {{
	Accept,
	Goto(usize),
	Shift(usize),
	Reduce(usize, String, String),
}}

lazy_static! {{
	static ref action_table: [(HashMap<TokenType, Action>, HashMap<String, Action>, Vec<String>); {}] = [
		{}
	];
}}

pub fn parse(mut lexer: Lexer) -> Result<AST, String> {{
	let mut state = 0;
	let mut stack: Vec<StackItem> = vec![StackItem::State(state)];

	let mut next_token = || loop {{
		let token = lexer.next();

		if token.token_type != TokenType::Comment {{
			return token;
		}}
	}};

	let mut token = next_token();

	loop {{
		state = match stack.last().unwrap() {{
			&StackItem::State(state) => state,
			_ => unreachable!(),
		}};

		let action = match action_table[state].0.get(&token.token_type) {{
			Some(action) => action,
			None => {{
				return Err(format!(
					"unexpected token; got {{:#?}}, expected one of {{:#?}}",
					token,
					action_table[state].2
				));
			}}
		}};

		match action {{
			Action::Accept => {{
				stack.pop();
				return Ok(match stack.pop().unwrap() {{
					StackItem::AST(ast) => raw_ast_to_ast(ast),
					_ => unreachable!(),
				}});
			}}
			Action::Goto(..) => unreachable!(),
			&Action::Shift(next) => {{
				stack.push(StackItem::AST(build_ast_terminal(token)));
				stack.push(StackItem::State(next));
				token = next_token();
			}}
			Action::Reduce(count, name, reduce_name) => {{
				let child_asts = stack
					.split_off(stack.len() - count * 2)
					.into_iter()
					.filter(|item| match item {{
						StackItem::AST(..) => true,
						_ => false,
					}})
					.map(|item| match item {{
						StackItem::AST(ast) => ast,
						_ => unreachable!(),
					}}).collect::<Vec<_>>();
				let ast = build_ast_nonterminal(reduce_name, child_asts);

				state = match stack.last().unwrap() {{
					&StackItem::State(state) => state,
					_ => unreachable!(),
				}};

				stack.push(StackItem::AST(ast));
				stack.push(StackItem::State(
					match action_table[state].1.get(name).unwrap() {{
						&Action::Goto(next) => next,
						_ => unreachable!(),
					}}
				));
			}}
		}}
	}}
}}
"#,
        ast, raw_ast, action_table_len, stringified_action_table
    )
}
