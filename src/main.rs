use std::collections::HashMap;
use std::env::current_dir;
use std::fs::{self, read_dir};
use std::io::ErrorKind;
use std::iter::Peekable;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::str::Lines;

use self::schema::column::Column;
use self::schema::index::{Constraint, Index};
use self::schema::scalar::{Enum, Scalar};
use self::schema::sql::Sql;
use self::schema::table::ForeignKey;
use anyhow::{anyhow, bail, Context};
use clap::Parser;
use imara_diff::intern::InternedInput;
use imara_diff::{diff, Algorithm, UnifiedDiffBuilder};
use ouroboros::self_referencing;
use patch::{Line, Patch};
use schema::{root::Schema, table::Table};

pub mod ids;
pub mod names;
pub mod parser;
pub mod schema;

#[macro_export]
macro_rules! w {
    ($out:expr, $($tt:tt)*) => {{
        use std::fmt::Write;
        write!($out, $($tt)*).unwrap();
    }};
}
#[macro_export]
macro_rules! wl {
    ($out:expr, $($tt:tt)*) => {{
        use std::fmt::Write;
        writeln!($out, $($tt)*).unwrap();
    }};
}
#[macro_export]
macro_rules! newty_enum {
    (
        $(#[$($attr:tt)+])*
        $vis:vis enum $name:ident {$($Variant:ident = $accessor:ident),* $(,)+}
    ) => {
        $(#[$($attr)+])*
        $vis enum $name {
            $($Variant($Variant),)*
        }
        paste::paste!{impl $name {$(
            $vis fn [<is_ $accessor>](&self) -> bool {
                matches!(self, Self::$Variant(_))
            }
            $vis fn [<as_ $accessor>](&self) -> Option<&$Variant> {
                match self {
                    Self::$Variant(value) => Some(value),
                    _ => None,
                }
            }
            $vis fn [<as_ $accessor _mut>](&mut self) -> Option<&mut $Variant> {
                match self {
                    Self::$Variant(value) => Some(value),
                    _ => None,
                }
            }
        )*}}
    };
}

pub struct TableItem<'a, I> {
	pub table: SchemaTable<'a>,
	value: &'a I,
}
impl<'a, I> TableItem<'a, I> {
	pub fn unchecked_new(table: SchemaTable<'a>, value: &'a I) -> Self {
		Self { table, value }
	}
}
impl<I> Deref for TableItem<'_, I> {
	type Target = I;
	fn deref(&self) -> &Self::Target {
		self.value
	}
}
impl<I> Clone for TableItem<'_, I> {
	fn clone(&self) -> Self {
		Self {
			table: self.table,
			value: self.value,
		}
	}
}
impl<I> Copy for TableItem<'_, I> {}
pub type TableIndex<'a> = TableItem<'a, Index>;
pub type TableColumn<'a> = TableItem<'a, Column>;
pub type TableConstraint<'a> = TableItem<'a, Constraint>;
pub type TableForeignKey<'a> = TableItem<'a, ForeignKey>;
pub type TableSql<'a> = TableItem<'a, Sql>;

pub struct Diff<I> {
	old: I,
	new: I,
}
pub type SchemaDiff<'a> = Diff<&'a Schema>;
pub type TableDiff<'a> = Diff<SchemaTable<'a>>;
pub type EnumDiff<'a> = Diff<SchemaEnum<'a>>;
pub type ColumnDiff<'a> = Diff<TableColumn<'a>>;

#[derive(Clone, Copy)]
pub struct SchemaEnum<'a> {
	pub schema: &'a Schema,
	pub en: &'a Enum,
}
impl Deref for SchemaEnum<'_> {
	type Target = Enum;

	fn deref(&self) -> &Self::Target {
		self.en
	}
}

#[derive(Clone, Copy)]
pub struct SchemaScalar<'a> {
	pub schema: &'a Schema,
	pub scalar: &'a Scalar,
}
impl Deref for SchemaScalar<'_> {
	type Target = Scalar;

	fn deref(&self) -> &Self::Target {
		self.scalar
	}
}
#[derive(Clone, Copy)]
pub struct SchemaTable<'a> {
	pub schema: &'a Schema,
	pub table: &'a Table,
}
impl SchemaTable<'_> {
	fn sql<'a>(&'a self, sql: &'a Sql) -> TableSql<'a> {
		TableSql {
			table: *self,
			value: sql,
		}
	}
	fn format_sql(&self, sql: &Sql) -> String {
		let mut out = String::new();
		self.sql(sql).print(&mut out);
		out
	}
}
impl Deref for SchemaTable<'_> {
	type Target = Table;

	fn deref(&self) -> &Self::Target {
		self.table
	}
}

#[derive(Parser)]
enum Opts {
	/// Create migrations directory and empty schema
	Init,
	/// Commit schema changes
	Commit {
		/// How to name the change
		#[clap(long, short = 'm')]
		message: String,
		/// Sql to execute before migration
		#[clap(long)]
		pre_sql: Option<String>,
		/// Sql to execute after migration
		#[clap(long)]
		post_sql: Option<String>,
	},
	/// List migrations
	List,
}

struct MigrationId {
	id: u32,
	slug: String,
	dirname: String,
}
fn list_ids(path: &Path) -> anyhow::Result<Vec<MigrationId>> {
	let dir = read_dir(path)?;
	let mut ids = Vec::new();
	for ele in dir {
		let ele = ele?;
		let meta = ele.metadata()?;
		if !meta.is_dir() {
			continue;
		}
		let dirname = ele.file_name();
		let dirname = dirname
			.to_str()
			.ok_or_else(|| anyhow!("file name is not utf-8"))?;

		let mut split = dirname.splitn(2, '_');
		let Some(id) = split.next() else {
            bail!("invalid migration dir: {dirname} doesn't start with NUM-name");
        };
		let Ok(id) = id.parse::<u32>()  else {
            bail!("invalid migration dir: {dirname}: {id} is not a number");
        };
		let Some(slug) = split.next() else {
            bail!("invalid migration dir: {dirname} doesn't contains slug");
        };

		ids.push(MigrationId {
			id,
			slug: slug.to_string(),
			dirname: dirname.to_owned(),
		});
	}
	if ids.is_empty() {
		return Ok(Vec::new());
	}
	ids.sort_by_key(|id| id.id);
	// Disallow duplicates
	{
		let mut has = HashMap::new();
		for id in ids.iter() {
			if let Some(old) = has.insert(id.id, id.slug.clone()) {
				bail!("two migrations have the same id: {old} and {}", id.slug);
			}
		}
	}
	// Disallow holes
	{
		for (i, id) in ids.iter().enumerate() {
			if id.id as usize != i {
				bail!("id hole: {}", i);
			}
		}
	}
	Ok(ids)
}
fn list(root: &Path) -> anyhow::Result<Vec<Migration>> {
	let mut out = Vec::new();
	let ids = list_ids(root)?;
	let mut path = root.to_path_buf();
	for id in ids {
		let slug = id.slug.clone();
		path.push(&id.dirname);
		path.push("db.update");

		let update = fs::read_to_string(&path).context("reading update source")?;
		let migration =
			parse_migration(id, &update).with_context(|| format!("parsing update {}", slug))?;
		out.push(migration);

		path.pop();
		path.pop();
	}
	Ok(out)
}

fn find_root(from: &Path) -> anyhow::Result<PathBuf> {
	let mut out = from.to_path_buf();
	loop {
		out.push("migrations");
		if out.is_dir() {
			return Ok(out);
		}
		out.pop();
		if !out.pop() {
			bail!("could not find 'migrations' in {from:?} or any parent directory. Have you called 'immigrant init' first?");
		}
	}
}

struct Migration {
	id: MigrationId,
	name: String,
	schema_diff: Option<OwnedPatch>,
	before_up_sql: String,
	after_up_sql: String,
	before_down_sql: String,
	after_down_sql: String,
}

fn skip_empty(l: &mut Peekable<Lines>) {
	if l.peek().map(|l| l.is_empty()).unwrap_or(false) {
		l.next();
	}
}
fn until_next_header(l: &mut Peekable<Lines>) -> String {
	let mut out = Vec::new();
	skip_empty(l);
	loop {
		let Some(line) = l.next_if(|l| !l.starts_with('#')) else {
            break;
        };
		out.push(line);
	}
	while out.last().map(|l| l.trim().is_empty()).unwrap_or(false) {
		out.pop();
	}
	out.join("\n")
}

#[self_referencing]
struct OwnedPatch {
	text: String,
	#[borrows(text)]
	#[not_covariant]
	patch: Patch<'this>,
}
impl OwnedPatch {
	fn parse(mut input: String) -> anyhow::Result<Self> {
		input.insert_str(0, "--- a\n+++ b\n");
		input.push('\n');
		// I have no idea how to make OwnedPatchTryBuilder work with this annotation
		let _ = Patch::from_single(&input).map_err(|mut e| {
			e.line -= 2;
			anyhow!("patch parse: {e}")
		})?;
		Ok(OwnedPatchBuilder {
			text: input,
			patch_builder: |text: &String| {
				Patch::from_single(text.as_str())
					.expect("first parse is for check, second is for store")
			},
		}
		.build())
	}
}

fn parse_migration(id: MigrationId, migration: &str) -> anyhow::Result<Migration> {
	let mut lines = migration.lines().peekable();
	skip_empty(&mut lines);
	let Some(header) = lines.next() else {
        bail!("file is empty");
    };
	let Some(name) = header.strip_prefix("# ") else {
        bail!("file should start with the header, which is prefixed by '# ' (notice it should have a space after sharp)");
    };

	let schema_diff = until_next_header(&mut lines);

	let before_up_sql = if lines.next_if(|v| v == &"## Before").is_some() {
		until_next_header(&mut lines)
	} else {
		"".to_owned()
	};
	let after_up_sql = if lines.next_if(|v| v == &"## After").is_some() {
		until_next_header(&mut lines)
	} else {
		"".to_owned()
	};
	let before_down_sql = if lines.next_if(|v| v == &"## Before (down)").is_some() {
		until_next_header(&mut lines)
	} else {
		"".to_owned()
	};
	let after_down_sql = if lines.next_if(|v| v == &"## After (down)").is_some() {
		until_next_header(&mut lines)
	} else {
		"".to_owned()
	};
	if let Some(line) = lines.next() {
		bail!("unexpected header: {line}");
	}

	let schema_diff = if !schema_diff.trim().is_empty() {
		Some(OwnedPatch::parse(schema_diff).context("patch parse")?)
	} else {
		None
	};

	Ok(Migration {
		id,
		name: name.to_owned(),
		schema_diff,
		before_up_sql,
		after_up_sql,
		before_down_sql,
		after_down_sql,
	})
}

fn current_schema(dir: &Path) -> anyhow::Result<(String, Schema)> {
	let mut name = dir.to_path_buf();
	name.push("db.schema");
	let schema_str = fs::read_to_string(&name)?;
	let schema = parser::parse(&schema_str);
	Ok((schema_str, schema))
}
fn apply_patch(diff: &Patch, old: &str) -> anyhow::Result<String> {
	let old_lines = old.lines().collect::<Vec<&str>>();
	let mut out: Vec<&str> = vec![];
	let mut old_line = 0;
	for hunk in &diff.hunks {
		while hunk.old_range.start != 0 && old_line < hunk.old_range.start - 1 {
			out.push(old_lines[old_line as usize]);
			old_line += 1;
		}
		for line in &hunk.lines {
			match line {
				Line::Context(line) => {
					let old = old_lines.get(old_line as usize);
					if old != Some(line) {
						bail!(
							"failed to find context at {old_line}: {}",
							old.unwrap_or(&"<unknown>")
						);
					}
					if (old_line as usize) < old_lines.len() {
						out.push(line);
					}
					old_line += 1;
				}
				Line::Add(s) => out.push(s),
				Line::Remove(line) => {
					let old = old_lines[old_line as usize];
					if &old != line {
						bail!("old line at {old_line} doesn't match schema: {old}");
					}
					old_line += 1;
				}
			}
		}
	}
	for line in old_lines.get((old_line as usize)..).unwrap_or(&[]) {
		out.push(line);
	}
	if old.ends_with('\n') {
		out.push("");
	}
	Ok(out.join("\n"))
}
fn stored_schema(list: &[Migration]) -> anyhow::Result<(String, Schema)> {
	let mut schema_str = String::new();
	for migration in list {
		if let Some(schema_diff) = &migration.schema_diff {
			schema_str = schema_diff
				.with_patch(|patch| apply_patch(patch, &schema_str))
				.with_context(|| format!("bad schema diff: {}", migration.id.slug))?;
		}
	}
	let schema = parser::parse(&schema_str);
	Ok((schema_str, schema))
}

fn main() -> anyhow::Result<()> {
	let opts = Opts::parse();
	//
	match opts {
		Opts::Init => {
			match fs::metadata("migrations") {
				Ok(_) => anyhow::bail!("migrations directory already exists"),
				Err(e) if e.kind() == ErrorKind::NotFound => {}
				Err(e) => return Err(e.into()),
			}
			fs::create_dir("migrations")?;
			fs::write("migrations/db.schema", "")?;
		}
		Opts::Commit {
			message,
			pre_sql,
			post_sql,
		} => {
			let root = find_root(&current_dir()?)?;
			let list = list(&root)?;
			let id = list.last().map(|m| m.id.id + 1).unwrap_or_default();

			let (original_str, original) =
				stored_schema(&list).context("failed to load past migrations")?;

			let (current_str, current) = current_schema(&root).context("parsing current schema")?;

			let diff_input = InternedInput::new(original_str.as_str(), current_str.as_str());
			let update = diff(
				Algorithm::Histogram,
				&diff_input,
				UnifiedDiffBuilder::new(&diff_input),
			);

			let mut up = String::new();
			let mut down = String::new();
			original.diff(&current, &mut down);
			current.diff(&original, &mut up);

			if up.trim().is_empty()
				&& down.trim().is_empty()
				&& pre_sql.is_none()
				&& post_sql.is_none()
			{
				println!("No changes found");
				return Ok(());
			}

			let mut dir = root;
			let slug = slug::slugify(&message);
			dir.push(format!("{id:0>14}_{slug}"));

			fs::create_dir(&dir).context("creating migration directory")?;

			let mut schema_update = dir.to_owned();

			let mut full_update = format!("# {}", message);
			if !update.trim().is_empty() {
				full_update.push('\n');
				full_update.push_str(update.trim());
			}
			if let Some(pre_sql) = pre_sql {
				full_update.push_str("\n\n## Before\n");
				full_update.push_str(pre_sql.trim());
			}
			if let Some(post_sql) = post_sql {
				full_update.push_str("\n\n## After\n");
				full_update.push_str(post_sql.trim());
			}

			schema_update.push("db.update");
			fs::write(schema_update, full_update).context("writing down.sql")?;

			let mut up_path = dir.to_owned();
			up_path.push("up.sql");
			fs::write(up_path, up).context("writing up.sql")?;
			let mut down_path = dir.to_owned();
			down_path.push("down.sql");
			fs::write(down_path, down).context("writing down.sql")?;
		}
		Opts::List => {
			let root = find_root(&current_dir()?)?;
			let list = list_ids(&root)?;
			for migration in list {
				println!("{}. {}", migration.id, migration.slug);
			}
		}
	}
	// let v = crate::parser::parse(include_str!("test.schema"));
	// let mut out = String::new();
	// wl!(out, "// up");
	// v.create(&mut out);
	// wl!(out, "// down");
	// v.drop(&mut out);
	//
	// print!("{out}");

	Ok(())
}

// fn diff(a: &Schema, b: &Schema) -> String {
//     let mut out = String::new();
//
//     for old_ver in a.tables() {
//         if let Some(new_ver) = b.table(&old_ver.name.db) {
//             let mut alternations = Vec::new();
//             {
//                 let mut renames = Vec::new();
//                 for old_idx in old_ver.indexes.iter() {
//                     if let Some(new_idx) = new_ver.indexes.iter().find(|ni| ni.isomorphic(&old_idx))
//                     {
//                         if new_idx.name(&old_ver.name.db) != old_idx.name(&old_ver.name.db) {
//                             renames.push(format!(
//                                 "RENAME CONSTRAINT {} TO {}",
//                                 old_idx.name(&old_ver),
//                                 new_idx.name(&new_ver)
//                             ));
//                         }
//                     } else {
//                         alternations.push(format!("DROP CONSTRAINT {}", old_idx.name(&old_ver),));
//                     }
//                 }
//                 alternations.extend(renames.into_iter());
//             }
//             for old_field in &old_ver.fields {
//                 if let Some(new_field) = new_ver.field(&old_field.name.db) {
//                     let new_ty = b.native_type(&new_field.ty);
//                     if a.native_type(&old_field.ty) != new_ty {
//                         alternations.push(format!(
//                             "ALTER COLUMN {} TYPE {new_ty}",
//                             old_field.name.db_name()
//                         ))
//                     }
//                     if old_field.nullable != new_field.nullable {
//                         let name = old_field.name.db_name();
//                         if new_field.nullable {
//                             alternations.push(format!("ALTER COLUMN {name} DROP NOT NULL"))
//                         } else {
//                             alternations.push(format!("ALTER COLUMN {name} SET NOT NULL"))
//                         }
//                     }
//                 } else {
//                     alternations.push(format!("DROP COLUMN {}", old_field.name.db_name()));
//                 }
//             }
//             for new_field in &new_ver.fields {
//                 if old_ver.field(&new_field.name.db).is_some() {
//                     continue;
//                 }
//                 alternations.push(format!(
//                     "ADD COLUMN {} {}",
//                     new_field.name.db_name(),
//                     b.native_type(&new_field.ty)
//                 ));
//             }
//             {
//                 for new_idx in new_ver.indexes.iter() {
//                     if !old_ver.indexes.iter().any(|ni| ni.isomorphic(&new_idx)) {
//                         new_idx.create(&mut out, &new_ver);
//                     }
//                 }
//             }
//             if !alternations.is_empty() {
//                 writeln!(
//                     out,
//                     "ALTER TABLE {}\n\t{};",
//                     old_ver.name.db_name(),
//                     alternations.join("\n,\t")
//                 )
//                 .unwrap();
//             }
//         }
//     }
//
//     for old_ver in a.tables() {
//         if b.table(&old_ver.name.db).is_none() {
//             writeln!(out, "DROP TABLE {};\n", old_ver.name.db_name()).unwrap();
//         }
//     }
//
//     // Create before altering, as new fields may
//     for new_ver in b.tables() {
//         if a.table(&new_ver.name.db).is_none() {
//             new_ver.create(&mut out, &b);
//         }
//     }
//
//     out
// }

#[cfg(test)]
mod tests {
	use super::parser::parse as root;
	fn showdiff(a: &Schema, b: &Schema) {
		println!();
		println!("// up.sql");
		let mut out = String::new();
		b.diff(a, &mut out);
		print!("{out}");

		println!("// down.sql");
		let mut out = String::new();
		a.diff(b, &mut out);
		print!("{out}")
	}
	use super::*;
	#[test]
	fn create_table() {
		let from = root(
			r#"
                        scalar id "INTEGER";
                        table Test {id;}
        "#,
		);
		let to = root("");
		showdiff(&from, &to);
	}
	#[test]
	fn correct_index() {
		let to = root(
			r#"
                        scalar id "INTEGER";
                        table Test {
                            a "acorrect": id @primary_key;
                            b "bcorrect": id @index;
                        }
        "#,
		);
		let from = root("");
		showdiff(&from, &to);
	}

	#[test]
	fn rename_index() {
		let from = root(r#"scalar id "INTEGER"; table Test { id @index "test1"; }"#);
		let to = root(r#"scalar id "INTEGER"; table Test { id @index; }"#);
		showdiff(&from, &to);
	}
	#[test]
	fn change_type() {
		let from = root(r#"scalar id "VARCHAR(10)"; table Test { id; }"#);
		let to = root(r#"scalar id "VARCHAR(20)"; table Test { id; id2: id; }"#);
		showdiff(&from, &to);
	}

	#[test]
	fn nullable() {
		let from = root(r#"scalar id "INTEGER"; table Test { id?; }"#);
		let to = root(r#"scalar id "INTEGER"; table Test { id; }"#);
		showdiff(&from, &to);
	}

	#[test]
	fn should_recreate_index_on_type_change() {
		let from = root(r#"scalar id "VARCHAR(1)"; table Test { id @index; }"#);
		let to = root(r#"scalar id "VARCHAR(2)"; table Test { id @index; }"#);
		showdiff(&from, &to);
	}
	#[test]
	fn enums() {
		let from = root(
			r#"
            enum removed_enum{hello; world};
            enum altered_enum{a; b};
        "#,
		);
		let to = root(
			r#"
            enum added_enum{hello; world};
            enum altered_enum{b; c};
        "#,
		);
		showdiff(&from, &to);
	}

	#[test]
	fn added_constraint() {
		let from = root(
			r#"
			scalar int "INTEGER";
			table A {
				a: int @primary_key;
				b: int;
			};
		"#,
		);
		showdiff(&root(""), &from);
		let to = root(
			r#"
			scalar int "INTEGER";
			table A {
				a: int;
				b: int @primary_key;
			};
		"#,
		);
		showdiff(&from, &to);
	}
}
