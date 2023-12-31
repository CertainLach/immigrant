use std::{
	collections::{BTreeMap, BTreeSet},
	env::{self, current_dir, current_exe},
	ffi::OsString,
	fs::{self, read_dir},
	io::ErrorKind,
	path::{Path, PathBuf},
	process,
};

use anyhow::{bail, Context};
use clap::{CommandFactory, FromArgMatches, Parser};
use cli::{current_schema, parse_schema};
use file_diffs::{find_root, list, list_ids, Migration, MigrationId};
use generator_postgres::Pg;
use schema::{root::Schema, uid::RenameMap};

#[derive(Parser)]
#[clap(author, version, allow_external_subcommands = true)]
enum Opts {
	/// Create migrations directory and empty schema.
	Init,
	/// Commit schema changes.
	Commit {
		/// How to name the change.
		///
		/// If not set - commit editor will be opened.
		#[clap(long, short = 'm')]
		message: Option<String>,
		#[clap(long)]
		before_up_sql: Option<String>,
		#[clap(long)]
		after_up_sql: Option<String>,
		#[clap(long)]
		before_down_sql: Option<String>,
		#[clap(long)]
		after_down_sql: Option<String>,
	},
	/// Show the difference between stored and uncommited schema.
	Diff,
	/// Regenerate up.sql/down.sql files.
	///
	/// Run it in case if you have altered the schema file for before/after sql injection.
	RegenerateSql,
	/// List migrations
	List,
}

fn decorate_automatically_generated(text: &mut String, has_before: bool, has_after: bool) {
	if !has_before && !has_after {
		text.insert_str(0, "-- THIS FILE IS AUTOMATICALLY GENERATED, DO NOT EDIT\n")
	} else if !has_after {
		text.insert_str(
			0,
			"-- THE REST OF THE FILE IS AUTOMATICALLY GENERATED, DO NOT EDIT\n",
		)
	} else {
		text.insert_str(0, "-- BEGIN OF AUTOMATICALLY GENERATED, DO NOT EDIT!\n");
		text.push_str("\n-- END OF AUTOMATICALLY GENERATED");
	}
}
fn decorate_before(text: &mut String, before: &str) {
	if before.is_empty() {
		return;
	}
	text.insert_str(0, "\n-- END OF USER ADDED\n\n");
	text.insert_str(0, before);
	text.insert_str(
		0,
		"-- BEGIN OF USER ADDED, DO NOT EDIT, UPDATE db.update INSTEAD!\n",
	);
}
fn decorate_after(text: &mut String, after: &str) {
	if after.is_empty() {
		return;
	}
	text.push_str("\n\n-- BEGIN OF USER ADDED, DO NOT EDIT, UPDATE db.update INSTEAD\n");
	text.push_str(after);
	text.push_str("\n-- END OF USER ADDED");
}

fn generate_sql(
	migration: &Migration,
	old_schema: &Schema,
	new_schema: &Schema,
	rn: &RenameMap,
	out: &Path,
) -> anyhow::Result<()> {
	let mut up = String::new();
	let mut down = String::new();
	Pg(new_schema).diff(&Pg(old_schema), &mut up, &mut rn.clone());
	Pg(old_schema).diff(&Pg(new_schema), &mut down, &mut rn.clone());

	decorate_automatically_generated(
		&mut up,
		!migration.before_up_sql.is_empty(),
		!migration.after_up_sql.is_empty(),
	);
	decorate_automatically_generated(
		&mut down,
		!migration.before_down_sql.is_empty(),
		!migration.after_down_sql.is_empty(),
	);
	decorate_before(&mut up, &migration.before_up_sql);
	decorate_before(&mut down, &migration.before_down_sql);
	decorate_after(&mut up, &migration.after_up_sql);
	decorate_after(&mut down, &migration.after_down_sql);

	let mut out = out.to_path_buf();
	out.push("up.sql");
	fs::write(&out, up.as_bytes())?;
	out.pop();
	out.push("down.sql");
	fs::write(&out, down.as_bytes())?;
	Ok(())
}

fn stored_schema(list: &[(MigrationId, Migration)]) -> anyhow::Result<(String, Schema, RenameMap)> {
	let mut schema_str = String::new();
	for (_, migration) in list {
		schema_str = migration.apply_diff(schema_str)?;
	}
	let mut rn = RenameMap::default();
	let schema = parse_schema(&schema_str, &mut rn)?;
	Ok((schema_str, schema, rn))
}

fn external_cmds_from_dir(dir: &Path, prefix: &str, out: &mut BTreeMap<String, PathBuf>) {
	let Ok(read) = read_dir(dir) else {
		return;
	};
	for entry in read {
		let Ok(entry) = entry else {
			continue;
		};
		let file_name = entry.file_name();
		let Some(name) = file_name.to_str() else {
			continue;
		};
		let Some(cmd) = name.strip_prefix(prefix) else {
			continue;
		};
		if cmd.ends_with(".d") {
			// Exists in cargo output directory
			continue;
		}
		if !entry.metadata().map(|e| e.is_file()).unwrap_or(false) {
			continue;
		}
		// Prefer first found
		if out.contains_key(cmd) {
			continue;
		}
		out.insert(cmd.to_owned(), entry.path());
	}
}

fn external_cmds_from_env(env: &str, prefix: &str, out: &mut BTreeMap<String, PathBuf>) {
	let path = env::var_os(env).unwrap_or_default();
	for ele in env::split_paths(&path) {
		external_cmds_from_dir(&ele, prefix, out)
	}
}

fn parse_or_external<T: CommandFactory + FromArgMatches>() -> T {
	let mut external = BTreeMap::new();
	if let Ok(mut exe) = current_exe() {
		exe.pop();
		external_cmds_from_dir(&exe, "immigrant-", &mut external);
	}
	external_cmds_from_env("PATH", "immigrant-", &mut external);

	let mut cmd = T::command().allow_external_subcommands(true);
	let mut disabled_external = BTreeSet::new();
	for name in external.keys() {
		if cmd.find_subcommand(name).is_some() {
			eprintln!("external subcommand has conflict: {name}");
			disabled_external.insert(name.clone());
			continue;
		}
	}
	for disabled in disabled_external {
		external.remove(&disabled);
	}
	if !external.is_empty() {
		// TODO: use unicode width?
		let longest = external.keys().map(|n| n.len()).max().expect("not empty");
		let after = match cmd.get_after_help() {
			Some(v) => color_print::cformat!("\n\n{}", v.ansi()),
			None => String::new(),
		};
		let mut full: String =
			color_print::cformat!("<bold><underline>External commands:</underline></bold>");
		for (name, path) in &external {
			let line = color_print::cformat!(
				// Having padding on name, instead of manually implementing it
				// causes some terminal formatting ugliness for me
				"\n  <bold>{name}</bold>{:<padding$}{}",
				"",
				path.display(),
				padding = longest + 2 - name.len()
			);
			full.push_str(&line);
		}

		full.push_str(&after);
		cmd = cmd.after_help(full);
	}
	let mut matches = match cmd.try_get_matches() {
		Ok(v) => v,
		Err(e) => e.exit(),
	};
	match matches.subcommand() {
		Some((name, args)) if external.contains_key(name) => {
			let args = args.get_many::<OsString>("").unwrap().collect::<Vec<_>>();
			let status = process::Command::new(external.get(name).expect("exists"))
				.args(&args)
				.status()
				.expect("failed to spawn exernal");
			process::exit(status.code().unwrap_or(1));
		}
		_ => {}
	}

	match T::from_arg_matches_mut(&mut matches) {
		Ok(opts) => opts,
		Err(e) => {
			let e = e.format(&mut Opts::command());
			e.exit()
		}
	}
}

fn main() -> anyhow::Result<()> {
	let opts: Opts = parse_or_external();
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
			before_up_sql,
			after_up_sql,
			before_down_sql,
			after_down_sql,
		} => {
			let root = find_root(&current_dir()?)?;
			let list = list(&root)?;
			let id = list.last().map(|(id, _)| id.id + 1).unwrap_or_default();

			let (original_str, original, orig_rn) =
				stored_schema(&list).context("failed to load past migrations")?;

			let (current_str, current, current_rn) =
				current_schema(&root).context("failed to parse current schema")?;

			let mut rn = orig_rn;
			rn.merge(current_rn);

			let should_use_editor = message.is_none();

			let message = message.unwrap_or_default();
			let mut message = message.splitn(2, '\n');
			let name = message.next().expect("at least one");
			let description = message.next().unwrap_or("");

			let slug = slug::slugify(name);
			let id = MigrationId::new(id, slug);
			let mut migration = Migration::new(
				name.to_owned(),
				description.to_owned(),
				before_up_sql,
				after_up_sql,
				before_down_sql,
				after_down_sql,
				current_str,
			);

			if should_use_editor {
				bail!("$EDITOR usage is not yet supported")
				// let editor = env::var_os("EDITOR")
				// 	.ok_or_else(|| anyhow!("editor is requested, but $EDITOR is not set"))?;
				// let mut migration_str = migration.to_string();
				// loop {
				// 	let mut editorfile = tempfile::NamedTempFile::new()?;
				// 	editorfile.write_all(migration.to_string().as_bytes())?;
				// 	editorfile.flush()?;
				// 	let status = Command::new(&editor)
				// 		.arg(editorfile.path())
				// 		.status()
				// 		.context("spawn editor")?;
				// 	if !status.success() {
				// 		println!("Editor failed");
				// 		return Ok(());
				// 	}
				// 	editorfile.seek(std::io::SeekFrom::Start(0))?;
				// 	let mut new_contents = String::new();
				// 	editorfile.read_to_string(&mut new_contents)?;
				// }
			}

			let mut dir = root;
			dir.push(&id.dirname);

			fs::create_dir(&dir).context("creating migration directory")?;

			migration.to_diff(original_str)?;

			if migration.is_noop() {
				println!("No changes found");
				return Ok(());
			}

			{
				let mut schema_update = dir.to_owned();
				schema_update.push("db.update");
				fs::write(schema_update, migration.to_string()).context("writing db.update")?;
			}

			generate_sql(&migration, &original, &current, &rn, &dir)?;
		}
		Opts::Diff => {
			let root = find_root(&current_dir()?)?;
			let list = list(&root)?;

			let (_, original, orig_rn) =
				stored_schema(&list).context("failed to load past migrations")?;

			let (_, current, current_rn) =
				current_schema(&root).context("failed to parse current schema")?;

			let mut rn = orig_rn;
			rn.merge(current_rn);

			let mut up = String::new();

			Pg(&current).diff(&Pg(&original), &mut up, &mut rn.clone());
			println!("{up}")
		}
		Opts::List => {
			let root = find_root(&current_dir()?)?;
			let list = list_ids(&root)?;
			for migration in list {
				println!("{}. {}", migration.id, migration.slug);
			}
		}
		Opts::RegenerateSql => {
			let mut root = find_root(&current_dir()?)?;
			let list = list(&root)?;
			let mut current_schema_str = String::new();
			let mut current_schema = Schema::default();
			let mut rn = RenameMap::default();
			for (id, migration) in list {
				current_schema_str = migration.apply_diff(current_schema_str)?;
				let mut crn = RenameMap::default();
				let updated_schema = parse_schema(&current_schema_str, &mut crn)?;
				rn.merge(crn);
				generator_postgres::validate::validate(&current_schema_str, &updated_schema, &rn);

				root.push(&id.dirname);
				generate_sql(&migration, &current_schema, &updated_schema, &rn, &root)?;
				root.pop();

				current_schema = updated_schema;
			}
		}
	}

	Ok(())
}

/* #[cfg(test)]
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
} */
