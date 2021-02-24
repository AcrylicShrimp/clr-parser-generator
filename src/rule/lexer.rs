use std::vec::Vec;

#[derive(Debug, PartialEq)]
pub enum Type {
	EoF,
	Unknown,
	Colon,
	Semicolon,
	Id,
	Literal,
}

#[derive(Debug)]
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

	pub fn next(&mut self) -> Token {
		let (content, line_number, line_offset) = self.next_blackspace();
		let mut content = content.to_string();

		if content == "\0" {
			return Token {
				token_type: Type::EoF,
				token_content: "\0".to_string(),
				line_number,
				line_offset,
			};
		}
		if content == ":" {
			return Token {
				token_type: Type::Colon,
				token_content: ":".to_string(),
				line_number,
				line_offset,
			};
		}
		if content == ";" {
			return Token {
				token_type: Type::Semicolon,
				token_content: ";".to_string(),
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

		let mut id = false;

		if content.starts_with("@") {
			id = true;
			content = content.chars().skip(1).collect();
		}

		if content.is_empty() {
			self.index += 1;
			self.line_offset += 1;
			return Token {
				token_type: Type::Unknown,
				token_content: self.content[self.index - 1].to_string(),
				line_number,
				line_offset,
			};
		}

		Token {
			token_type: if id { Type::Id } else { Type::Literal },
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
			':' | ';' | '@' => true,
			_ => false,
		}
	}

	fn is_eof(&self) -> bool {
		self.max_index <= self.index
	}
}
