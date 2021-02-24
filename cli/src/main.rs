use clr_parser_generator::*;
use std::fs;

fn main() {
	match fs::read_to_string("test/test.rule") {
		Ok(rules) => {
			generate_crl_parser(rules);
		}
		Err(err) => println!("Error: {}", err),
	}
}
