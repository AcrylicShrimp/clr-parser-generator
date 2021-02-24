mod rule;

use rule::gen::*;
use rule::parser::*;

pub fn generate_crl_parser(rules: String) -> String {
	let mut rule_parser = Parser::new(rules);
	let mut gen = Gen::new(rule_parser.parse());

	gen.generate_first();
	gen.generate_dag();
	gen.generate_action_table();

	let action_table = gen.action_table;

	return "".to_owned();
}
