use super::lexer::{Lexer, Token, Type};
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Debug, Clone)]
pub enum RuleItem {
	Terminal(Token),
	NonTerminal(Token),
	Option(Vec<RuleItem>),
	Repeat(Vec<RuleItem>),
}

struct PartialRule {
	pub name: Token,
	pub sub_name: Option<Token>,
	pub items: Vec<RuleItem>,
	pub flatten: bool,
}

#[derive(Debug)]
pub struct Rule {
	pub index: usize,
	pub name: String,
	pub items: Vec<RuleItem>,
}

impl Rule {
	pub fn first_item(&self) -> Option<&RuleItem> {
		self.items.first()
	}
}

#[derive(Debug)]
pub struct RuleTable {
	pub rule_vec: Vec<Rule>,
	pub rule_map: HashMap<String, Vec<usize>>,
}

impl RuleTable {
	pub fn new() -> RuleTable {
		RuleTable {
			rule_vec: Vec::new(),
			rule_map: HashMap::new(),
		}
	}

	fn add_rule(&mut self, partial_rule: PartialRule) {
		// Resolve options.
		let mut items = vec![partial_rule.items];
		let mut index = 0;

		while index < items.len() {
			let mut extra_items = Vec::new();

			for items in items.iter_mut() {
				let mut index = 0;

				while index < items.len() {
					if let RuleItem::Option(option_items) = &items[index] {
						let mut extra_item = items.clone();
						let mut rest = extra_item.split_off(index + 1);
						extra_item.pop();
						extra_item.extend_from_slice(option_items);
						extra_item.append(&mut rest);
						extra_items.push(extra_item);
						items.remove(index);
					} else {
						index += 1;
					}
				}
			}

			items.extend(extra_items);
			index += 1;
		}

		for items in items {
			let index = self.rule_vec.len();
			self.rule_map
				.entry(partial_rule.name.token_content.clone())
				.or_default()
				.push(index);
			self.rule_vec.push(Rule {
				index,
				name: partial_rule.name.token_content.clone(),
				items,
			});
		}
	}
}

#[derive(Debug)]
pub struct Parser {
	lexer: Lexer,
}

impl Parser {
	pub fn new(content: String) -> Parser {
		Parser {
			lexer: Lexer::new(content),
		}
	}

	pub fn parse(&mut self) -> RuleTable {
		let mut rule_table = RuleTable::new();

		loop {
			match self.next() {
				Some(partial_rule) => {
					rule_table.add_rule(partial_rule);
				}
				None => {
					break;
				}
			};
		}

		rule_table
	}

	fn next(&mut self) -> Option<PartialRule> {
		let name = self.lexer.next(true);

		if name.token_type == Type::EoF {
			return None;
		}

		if name.token_type != Type::Terminal {
			panic!(
				"name expected, got [{:?}: {}]",
				name.token_type, name.token_content
			);
		}

		let colon_or_equal = self.lexer.next(true);
		let mut sub_name = None;

		if colon_or_equal.token_type != Type::Colon {
			if colon_or_equal.token_type == Type::Equal {
				let name = self.lexer.next(true);

				if name.token_type != Type::Terminal {
					panic!(
						"sub name expected, got [{:?}: {}]",
						name.token_type, name.token_content
					);
				}

				sub_name = Some(name);

				let colon = self.lexer.next(true);

				if colon.token_type != Type::Colon {
					panic!(
						"colon expected, got [{:?}: {}]",
						colon.token_type, colon.token_content
					);
				}
			} else {
				panic!(
					"colon or equal expected, got [{:?}: {}]",
					colon_or_equal.token_type, colon_or_equal.token_content
				);
			}
		}

		let mut partial_rule = PartialRule {
			name: name,
			sub_name,
			items: Vec::new(),
			flatten: false,
		};

		let first_item = self.next_item();

		match first_item {
			Some(item) => {
				partial_rule.items.push(item);
			}
			None => {
				let token = self.lexer.next(true);

				panic!(
					"terminal or non-terminal or open-brace or open-bracket expected, got [{:?}: {}]",
					token.token_type, token.token_content
				)
			}
		}
		loop {
			match self.next_item() {
				Some(item) => partial_rule.items.push(item),
				None => {
					let token = self.lexer.next(true);

					if token.token_type != Type::Semicolon {
						panic!(
							"semicolon expected, got [{:?}: {}]",
							token.token_type, token.token_content
						)
					}

					break;
				}
			}
		}

		Some(partial_rule)
	}

	fn next_item(&mut self) -> Option<RuleItem> {
		let token = self.lexer.next(false);

		match token.token_type {
			Type::Semicolon | Type::BraceR | Type::BracketR => None,
			Type::Terminal => Some(RuleItem::Terminal(self.lexer.next(true))),
			Type::NonTerminal => Some(RuleItem::NonTerminal(self.lexer.next(true))),
			Type::BraceL => {
				self.lexer.next(true);

				let mut items = Vec::new();

				loop {
					let item = self.next_item();

					match item {
						Some(item) => items.push(item),
						None => {
							let token = self.lexer.next(true);

							if token.token_type != Type::BraceR {
								panic!(
									"close-brace expected, got [{:?}: {}]",
									token.token_type, token.token_content
								)
							}

							break;
						}
					}
				}

				Some(RuleItem::Repeat(items))
			}
			Type::BracketL => {
				self.lexer.next(true);

				let mut items = Vec::new();

				loop {
					let item = self.next_item();

					match item {
						Some(item) => items.push(item),
						None => {
							let token = self.lexer.next(true);

							if token.token_type != Type::BracketR {
								panic!(
									"close-bracket expected, got [{:?}: {}]",
									token.token_type, token.token_content
								)
							}

							break;
						}
					}
				}

				Some(RuleItem::Option(items))
			}
			_ => panic!(
				"terminal or non-terminal or open-brace or open-bracket expected, got [{:?}: {}]",
				token.token_type, token.token_content
			),
		}
	}
}
