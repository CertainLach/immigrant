use std::{fs, path::Path};

use anyhow::anyhow;
use ass_stroke::{SnippetBuilder, Text};
use immigrant_schema::{parser, root::Schema};

pub fn parse_schema(schema: &str) -> anyhow::Result<Schema> {
	match parser::parse(schema) {
		Ok(s) => Ok(s),
		Err(e) => {
			let mut builder = SnippetBuilder::new(schema);
			for e in e {
				match e {
					parser::ParsingError::Peg(peg) => {
						builder
							.error(Text::single(
								format!("parsing error: {peg}").chars(),
								Default::default(),
							))
							.range(peg.location.offset..=peg.location.offset)
							.build();
					}
				}
			}
			eprintln!("schema parsing ended with failure:");
			eprint!("{}", ass_stroke::source_to_ansi(&builder.build()));
			Err(anyhow!("failed to parse schema"))
		}
	}
}

pub fn current_schema(dir: &Path) -> anyhow::Result<(String, Schema)> {
	let mut name = dir.to_path_buf();
	name.push("db.schema");
	let schema_str = fs::read_to_string(&name)?;
	let schema = parse_schema(&schema_str)?;
	Ok((schema_str, schema))
}
