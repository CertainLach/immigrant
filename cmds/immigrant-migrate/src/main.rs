use std::{env, env::current_dir, fs};

use anyhow::{bail, Context, Result};
use clap::Parser;
use cli::{current_schema, generate_sql, generate_sql_nowrite, stored_schema};
use file_diffs::{find_root, list, Migration, MigrationId};
use futures::StreamExt;
use sqlx::{pool::PoolConnection, postgres::PgPoolOptions, Acquire, Executor, Postgres};

#[derive(Parser)]
enum Subcommand {
	Commit {
		/// How to name the change.
		///
		/// If not set - commit editor will be opened.
		// FIXME: Duplicates way too many code from `immigrant commit`
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
}
#[derive(Parser)]
#[clap(author, version)]
struct Opts {
	#[arg(long, default_value = "__immigrant_migrations")]
	migrations_table: String,
	#[command(subcommand)]
	cmd: Subcommand,
}

async fn run_migrations(
	conn: &mut PoolConnection<Postgres>,
	id: u32,
	migration: String,
	migrations_table: &str,
) -> Result<()> {
	let mut tx = conn.begin().await?;
	tx.execute(format!("INSERT INTO {migrations_table}(version) VALUES ({id});").as_str())
		.await?;
	{
		let mut executing = tx.execute_many(migration.as_str());
		while let Some(v) = executing.next().await {
			let _res = v?;
		}
	}

	tx.commit().await?;
	Ok(())
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
	tracing_subscriber::fmt::init();
	let opts = Opts::parse();

	let migrations_table = &opts.migrations_table;

	let database_url = env::var("DATABASE_URL")?;
	let pool = PgPoolOptions::new().connect(&database_url).await?;
	let mut conn = pool.acquire().await?;
	conn.execute(
		format!(
			r#"
				CREATE TABLE IF NOT EXISTS {migrations_table} (
					version INTEGER NOT NULL PRIMARY KEY,
					run_on TIMESTAMP NOT NULL DEFAULT NOW()
				);
			"#
		)
		.as_str(),
	)
	.await?;
	let (last_ver,) = sqlx::query_as::<_, (i32,)>(
		"SELECT COALESCE(MAX(version), 0) AS version FROM __immigrant_migrations",
	)
	.fetch_one(&mut *conn)
	.await?;
	assert!(last_ver >= 0);
	let last_ver = last_ver as u32;

	match opts.cmd {
		Subcommand::Commit {
			message,
			before_up_sql,
			after_up_sql,
			before_down_sql,
			after_down_sql,
		} => {
			let root = find_root(&current_dir()?)?;
			let list = list(&root)?;

			for (id, _, path) in &list {
				if id.id <= last_ver {
					continue;
				}
				let mut path = path.to_owned();
				path.push("up.sql");
				let sql = fs::read_to_string(&path).context("reading migration up.sql file")?;
				run_migrations(&mut conn, id.id, sql, &migrations_table).await?;
			}
			let id = list.last().map(|(id, _, _)| id.id + 1).unwrap_or_default();

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
			}

			migration.to_diff(original_str)?;

			if migration.is_noop() {
				println!("No changes found");
				return Ok(());
			}

			let (sql, _) = generate_sql_nowrite(&migration, &original, &current, &rn)?;
			if let Err(e) = run_migrations(&mut conn, id.id, sql.clone(), migrations_table).await {
				eprintln!("Won't commit failed migration:\n\n{sql}");
				return Err(e);
			};

			let mut dir = root;
			dir.push(&id.dirname);
			fs::create_dir(&dir).context("creating migration directory")?;

			{
				let mut schema_update = dir.to_owned();
				schema_update.push("db.update");
				fs::write(schema_update, migration.to_string()).context("writing db.update")?;
			}

			generate_sql(&migration, &original, &current, &rn, &dir)?;
		}
	}
	Ok(())
}
