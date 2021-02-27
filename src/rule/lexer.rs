use std::vec::Vec;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
	EoF,
	Unknown,
	Colon,
	Semicolon,
	Equal,
	BraceL,
	BraceR,
	BracketL,
	BracketR,
	Terminal,
	NonTerminal,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub token_type: Type,
	pub token_content: String,
	pub line_number: usize,
	pub line_offset: usize,
}

#[derive(Debug)]
pub struct Lexer {
	content: Vec<char>,
	index: usize,
	max_index: usize,
	line_number: usize,
	line_offset: usize,
}

impl Lexer {
	pub fn new(content: String) -> Lexer {
		let char_vec: Vec<char> = content.chars().collect();
		let length = char_vec.len();

		Lexer {
			content: char_vec,
			index: 0,
			max_index: length,
			line_number: 0,
			line_offset: 0,
		}
	}

	pub fn next(&mut self, advance: bool) -> Token {
		let current_index = self.index;
		let current_line_number = self.line_number;
		let current_line_offset = self.line_offset;

		let (content, line_number, line_offset) = self.next_blackspace();
		let mut content = content.to_string();

		let token_type = match content.as_str() {
			"\0" => Some(Type::EoF),
			":" => Some(Type::Colon),
			";" => Some(Type::Semicolon),
			"=" => Some(Type::Equal),
			"[" => Some(Type::BracketL),
			"]" => Some(Type::BracketR),
			"{" => Some(Type::BraceL),
			"}" => Some(Type::BraceR),
			_ => None,
		};

		if let Some(token_type) = token_type {
			if !advance {
				self.index = current_index;
				self.line_number = current_line_number;
				self.line_offset = current_line_offset;
			}

			return Token {
				token_type,
				token_content: content,
				line_number,
				line_offset,
			};
		}

		while !self.is_eof() && !self.content[self.index].is_whitespace() && !self.is_punctuation()
		{
			content.push(self.content[self.index]);
			self.index += 1;
			self.line_offset += 1;
		}

		let mut is_ternimal = true;

		if content.starts_with("@") {
			is_ternimal = false;
			content = content.chars().skip(1).collect();
		}

		if content.is_empty() {
			self.index += 1;
			self.line_offset += 1;

			let token = Token {
				token_type: Type::Unknown,
				token_content: self.content[self.index - 1].to_string(),
				line_number,
				line_offset,
			};

			if !advance {
				self.index = current_index;
				self.line_number = current_line_number;
				self.line_offset = current_line_offset;
			}

			return token;
		}

		if !advance {
			self.index = current_index;
			self.line_number = current_line_number;
			self.line_offset = current_line_offset;
		}

		Token {
			token_type: if is_ternimal {
				Type::Terminal
			} else {
				Type::NonTerminal
			},
			token_content: content,
			line_number,
			line_offset,
		}
	}

	fn next_blackspace(&mut self) -> (char, usize, usize) {
		while !self.is_eof() && self.content[self.index].is_whitespace() {
			if self.content[self.index] == '\n' {
				self.line_number += 1;
				self.line_offset = 1;
			} else {
				self.line_offset += 1;
			}

			self.index += 1;
		}

		if self.is_eof() {
			return ('\0', self.line_number, self.line_offset);
		}

		let index = self.index;
		let line_offset = self.line_offset;
		self.index += 1;
		self.line_offset += 1;
		(self.content[index], self.line_number, line_offset)
	}

	fn is_punctuation(&self) -> bool {
		match self.content[self.index] {
			':' | ';' | '@' | '=' | '[' | ']' | '{' | '}' => true,
			_ => false,
		}
	}

	fn is_eof(&self) -> bool {
		self.max_index <= self.index
	}
}
