use super::lexer::{Lexer, Type};
use std::collections::HashMap;
use std::vec::Vec;

#[derive(Debug, Clone, PartialEq)]
pub enum RuleItem {
	Terminal(String),
	NonTerminal(String),
	FlattenNonTerminal(String),
	Option(Vec<RuleItem>),
	Repeat(Vec<RuleItem>),
}

struct PartialRule {
	pub name: String,
	pub sub_name: Option<String>,
	pub items: Vec<RuleItem>,
	pub flatten: bool,
}

#[derive(Debug)]
pub struct Rule {
	pub index: usize,
	pub name: String,
	pub sub_name: Option<String>,
	pub items: Vec<RuleItem>,
	pub flatten: bool,
}

impl Rule {
	pub fn first_item(&self) -> Option<&RuleItem> {
		self.items.first()
	}
}

impl PartialEq for Rule {
	fn eq(&self, other: &Self) -> bool {
		self.name == other.name && self.items == other.items
	}
}

#[derive(Debug)]
pub struct RuleTable {
	pub names: Vec<String>,
	pub rule_vec: Vec<Rule>,
	pub rule_map: HashMap<String, Vec<usize>>,
}

impl RuleTable {
	pub fn new() -> RuleTable {
		RuleTable {
			names: Vec::new(),
			rule_vec: Vec::new(),
			rule_map: HashMap::new(),
		}
	}

	fn add_rule(&mut self, partial_rule: PartialRule) {
		// Resolve options.
		// Convert RuleItem::Option into multiple sequences of RuleItem::Terminal or RuleItem::NonTerminal.
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

		// Resolve repeats.
		// Add a new partial rules for each RuleItem::Repeat.
		for items in items.iter_mut() {
			for item in items.iter_mut() {
				if let RuleItem::Repeat(repeat_items) = item {
					let new_partial_rule_name = format!("{}!{}", partial_rule.name, index);
					let mut rule_items = Vec::new();
					rule_items.append(repeat_items);
					rule_items.push(RuleItem::Option(vec![RuleItem::FlattenNonTerminal(
						new_partial_rule_name.clone(),
					)]));
					*item = RuleItem::FlattenNonTerminal(new_partial_rule_name.clone());
					self.add_rule(PartialRule {
						name: new_partial_rule_name,
						sub_name: None,
						items: rule_items,
						flatten: true,
					})
				}

				index += 1;
			}
		}

		for items in items {
			let index = self.rule_vec.len();
			if !self.names.contains(&partial_rule.name) {
				self.names.push(partial_rule.name.clone());
			}
			self.rule_map
				.entry(partial_rule.name.clone())
				.or_default()
				.push(index);
			self.rule_vec.push(Rule {
				index,
				name: partial_rule.name.clone(),
				sub_name: partial_rule.sub_name.clone(),
				items,
				flatten: partial_rule.flatten,
			});
		}
	}

	fn dedupe(&mut self) {
		self.names.dedup();
		self.rule_vec.dedup();
		self.rule_map.clear();

		let mut index = 0;

		for rule in self.rule_vec.iter_mut() {
			rule.index = index;
			self.rule_map
				.entry(rule.name.clone())
				.or_default()
				.push(index);
			index += 1;
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

		rule_table.dedupe();
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
			name: name.token_content,
			sub_name: sub_name.map(|token| token.token_content),
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
			Type::Terminal => Some(RuleItem::Terminal(self.lexer.next(true).token_content)),
			Type::NonTerminal => Some(RuleItem::NonTerminal(self.lexer.next(true).token_content)),
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
