use std::env::current_dir;

use clap::Parser;
use cli::current_schema;
use file_diffs::find_root;
use schema::{index::ConstraintTy, root::Item, table::Cardinality, SchemaTable};

/// Generate db structure in dmdl format
#[derive(Parser)]
#[clap(author, version)]
struct Opts {}

fn common_spaces_prefix<'a>(lines: impl Iterator<Item = &'a str>) -> usize {
	let mut spaces = usize::MAX;
	for doc in lines {
		spaces = spaces.min(doc.chars().take_while(|&arg| arg == ' ').count());
	}
	// assert_ne!(spaces, usize::MAX, "docs are not empty");
	if spaces == usize::MAX {
		0
	} else {
		spaces
	}
}

fn main() -> anyhow::Result<()> {
	let _opts = Opts::parse();

	let root = find_root(&current_dir()?)?;
	let (_, schema) = current_schema(&root)?;
	for item in &schema.0 {
		match item {
			Item::Table(t) => {
				let t = SchemaTable {
					schema: &schema,
					table: t,
				};
				println!("Table {} {{", t.name);
				for column in &t.columns {
					print!("\t{} {:?}", column.name, column.ty);
					let mut is_pk = false;
					for ele in t.constraints() {
						if let ConstraintTy::PrimaryKey(columns) = &ele.kind {
							if columns.contains(&column.name.id()) {
								is_pk = true;
								break;
							}
						}
					}
					if is_pk || !column.docs.is_empty() {
						print!(" [");
						let mut had = false;
						if is_pk {
							had = true;
							print!("primary key");
						}
						if column.docs.len() == 1 {
							if had {
								print!(", ");
							}
							print!("note: '{}'", column.docs[0].trim_start())
						} else if !column.docs.is_empty() {
							if had {
								print!(", ");
							}
							println!("note: '''");
							let spaces =
								common_spaces_prefix(column.docs.iter().map(String::as_str));
							for doc in column.docs.iter() {
								println!("\t\t{}", &doc[spaces..]);
							}
							print!("\t'''");
						}
						print!("]");
					}
					println!();
				}
				if t.docs.len() == 1 {
					println!("\tNote: '{}'", t.docs[0].trim_start())
				} else if !t.docs.is_empty() {
					println!("\tNote: '''");
					let spaces = common_spaces_prefix(t.docs.iter().map(String::as_str));
					for doc in &t.docs {
						println!("\t\t{}", &doc[spaces..]);
					}
					println!("\t'''");
				}
				println!("}}");
				for fk in t.foreign_keys() {
					print!("Ref: {}.(", t.name);
					{
						let mut out = String::new();
						t.print_column_list(&mut out, fk.source_columns().iter().copied());
						print!("{out}");
					}
					print!(") ");
					match fk.cardinality() {
						(Cardinality::One, Cardinality::One) => print!("-"),
						(Cardinality::One, Cardinality::Many) => print!("<"),
						(Cardinality::Many, Cardinality::One) => print!(">"),
						(Cardinality::Many, Cardinality::Many) => print!("<>"),
					}
					print!(" {}.(", fk.target_table().name);
					{
						let mut out = String::new();
						t.print_column_list(&mut out, fk.target_columns().iter().copied());
						print!("{out}");
					}
					println!(")");
				}
			}
			Item::Enum(_) => {}
			Item::Scalar(_) => {}
		}
	}
	Ok(())
}
