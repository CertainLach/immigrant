use std::{collections::BTreeSet, env::current_dir};

use clap::Parser;
use cli::current_schema;
use file_diffs::find_root;
use inflector::{
	cases::{pascalcase::to_pascal_case, snakecase::to_snake_case},
	string::pluralize,
};
use schema::{
	column::SchemaType, names::EnumItemDefName, table::Cardinality, w, wl, SchemaEnum, SchemaTable,
	TableColumn,
};

#[derive(PartialEq, Clone, Copy)]
enum TableKind {
	New,
	Load,
	Update,
}

fn table_mod_ident(t: &SchemaTable) -> String {
	let ident = to_snake_case(&t.name.id().name());
	pluralize::to_plural(&ident)
}
fn fk_method_ident(t: &SchemaTable, plural: bool) -> String {
	let ident = to_snake_case(&t.name.id().name());
	if plural {
		pluralize::to_plural(&ident)
	} else {
		ident
	}
}
fn table_struct_ident(t: &SchemaTable) -> String {
	t.name.id().name()
}
fn column_ident(c: &TableColumn) -> String {
	to_snake_case(&c.name.id().name())
}
fn type_ident(c: &SchemaType) -> String {
	to_pascal_case(&c.ident().name())
}
fn enum_ident(c: &SchemaEnum) -> String {
	to_pascal_case(&c.name.id().name())
}
fn enum_item_ident(c: &EnumItemDefName) -> String {
	to_pascal_case(&c.id().name())
}

fn is_copy(t: &SchemaType) -> bool {
	match t {
		SchemaType::Scalar(s) => s
			.attrlist
			.get_single("diesel", "copy")
			.expect("diesel copy"),
		// Enums are always copy
		SchemaType::Enum(_) => true,
	}
}
// jojo in name was derived from join-join, but this function was later repurposed...
// Yet the name is awesome, so it is left
#[allow(clippy::nonminimal_bool)]
fn is_jojo_reference(k: TableKind, copy: bool, column: &TableColumn) -> bool {
	// Load struct is always owned
	k != TableKind::Load
		// Copy types are copied
		&& !copy
		// In update table, pks are immutable
		&& !(k == TableKind::Update && column.is_pk_part())
		// default fields are skipped for New table
		&& !(k == TableKind::New && column.default().is_some())
}
fn table_has_lifetime(k: TableKind, table: &SchemaTable) -> bool {
	match k {
		TableKind::New | TableKind::Update => {
			let mut has_reference = false;
			for column in table.columns() {
				let copy = is_copy(&column.ty());
				if is_jojo_reference(k, copy, &column) {
					has_reference = true;
					break;
				}
			}
			has_reference
		}
		TableKind::Load => false,
	}
}
// fn format_where(_out: &mut String) {
// 	// w!(out, "\twhere C: diesel_async::AsyncConnection");
// 	// if check_be {
// 	// w!(out, "<Backend = diesel::pg::Pg>");
// 	// }
// 	// wl!(out,);
// }
fn format_db_arg(out: &mut String) {
	wl!(out, "crate::Sqlite<'_>");
}
fn column_ty_name(jojo_reference: bool, ty: &SchemaType, out: &mut String) {
	let id = type_ident(ty);
	match ty {
		SchemaType::Scalar(s) => {
			let native = s
				.attrlist
				.try_get_single::<String>("diesel", "native")
				.expect("diesel native");
			let native_ref = s
				.attrlist
				.try_get_single::<String>("diesel", "native_ref")
				.expect("diesel native");
			let custom: bool = s
				.attrlist
				.get_single("diesel", "custom")
				.expect("diesel custom");

			if native_ref.is_some() && native.is_none() {
				panic!("if you use native_ref, you should also set native");
			}
			if custom && native.is_some() {
				panic!("custom is mutually exclusive with native");
			}
			if !custom && native.is_none() {
				panic!("type should be either native or custom");
			}

			// TODO: disallow setting both at the same time
			if let Some(s) = native {
				match native_ref {
					Some(native_ref) if jojo_reference => {
						w!(out, "{native_ref}");
					}
					_ => {
						w!(out, "{s}");
					}
				}
			} else if custom {
				w!(out, "ut::{id}");
			} else {
				panic!("either custom or native type is expected")
			}
		}
		SchemaType::Enum(_) => {
			// Always copy
			w!(out, "ut::{id}")
		}
	}
}

/// immigrant diesel helpers
#[derive(Parser)]
#[clap(author, version)]
enum Opts {
	/// Similarly to diesel print-schema, generates a diesel database structure definition
	///
	/// Unlike diesel print-schema, requires no database connection, and also generates
	/// ORM-like structure definitions, enum values, supports view, etc.
	PrintSchema,
}

fn main() -> anyhow::Result<()> {
	let opts = Opts::parse();
	match opts {
		Opts::PrintSchema => print_schema(),
	}
}
fn print_schema() -> anyhow::Result<()> {
	let root = find_root(&current_dir()?)?;

	let mut config = root.clone();
	config.push("immigrant-diesel.jsonnet");

	let (_, schema) = current_schema(&root)?;
	println!("pub mod user_types;");
	println!("use user_types as ut;");
	println!("use diesel::sql_types as dt;");
	println!("pub mod sql_types {{");
	println!("\tuse diesel::{{sql_types::SqlType, QueryId}};");
	for en in schema.enums() {
		let en = SchemaEnum {
			schema: &schema,
			en,
		};
		let id = enum_ident(&en);
		let name = en.name.db();
		println!("\t#[derive(SqlType, QueryId)]");
		println!("\t#[diesel(postgres_type(name = {name:?}))]");
		println!("\tpub struct {id};");
	}
	println!("}}");
	println!("use sql_types as st;");
	println!("pub mod enum_types {{");
	for en in schema.enums() {
		let en = SchemaEnum {
			schema: &schema,
			en,
		};
		let id = enum_ident(&en);

		print!("\t#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq, Eq, Clone, Copy, Hash");
		for derive in en
			.attrlist
			.get_multi::<String>("diesel", "derive")
			.expect("diesel derive")
		{
			print!(", {derive}");
		}
		println!(")]");

		let name = format!("crate::schema::sql_types::{id}");
		println!("\t#[ExistingTypePath = {name:?}]");
		println!("\tpub enum {id} {{");
		for item in &en.items {
			let id = enum_item_ident(item);
			let name = item.db();
			println!("\t\t#[db_rename = {name:?}]");
			println!("\t\t{id},");
		}
		println!("\t}}");
	}
	println!("}}");
	for table in schema.tables() {
		println!();
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		println!("diesel::table! {{");
		println!("\tuse super::sql_types::*;");
		println!("\tuse super::{{dt,st}};");
		println!();
		for line in table.docs.iter() {
			println!("\t///{line}");
		}
		let table_name = table.name.db().to_string();
		let table_ident = table_mod_ident(&table);
		{
			if table_name != table_ident {
				println!("\t#[sql_name = {table_name:?}]");
			}
		}
		print!("\t{table_ident} (");
		for (i, column) in table.columns().filter(TableColumn::is_pk_part).enumerate() {
			if i != 0 {
				print!(", ");
			}
			let ident = column_ident(&column);
			print!("{ident}");
		}
		println!(") {{");
		for column in table.columns() {
			let name = column.name.db().to_string();
			let db_ty = column.db_type();
			{
				for line in column.docs.iter() {
					println!("\t\t///{line}");
				}
				if !column.docs.is_empty() {
					println!("\t\t///");
				}
				println!("\t\t/// `{table_name}`.`{name}` column");
				println!("\t\t/// The SQL type is `{db_ty}`");
			}

			let ident = column_ident(&column);
			if name != ident {
				println!("\t\t#[sql_name = {name:?}]");
			}
			print!("\t\t{ident} -> ");
			if column.nullable {
				print!("dt::Nullable<");
			}
			match column.ty() {
				SchemaType::Enum(en) => {
					let ident = enum_ident(&en);
					print!("st::{ident}");
				}
				SchemaType::Scalar(s) => {
					let v: String = s
						.attrlist
						.get_single("diesel", "type")
						.expect("diesel type");
					print!("{v}");
				}
			}
			if column.nullable {
				print!(">");
			}
			println!(",");
		}
		println!("\t}}");
		println!("}}");
	}
	let mut allowed = BTreeSet::new();
	for table in schema.tables() {
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		let this = table.name.id();
		for fk in table.foreign_keys() {
			let other = fk.target_table().name.id();
			let key = if this.name() < other.name() {
				(this, other)
			} else {
				(other, this)
			};
			allowed.insert(key);
		}
	}
	let mut allowed: Vec<_> = allowed.into_iter().collect();
	allowed.sort_by_key(|(_, b)| b.name());
	allowed.sort_by_key(|(a, _)| a.name());
	for (a, b) in allowed {
		let a = schema.schema_table(&a).expect("a");
		let b = schema.schema_table(&b).expect("b");
		let a = table_mod_ident(&a);
		let b = table_mod_ident(&b);
		println!("diesel::prelude::allow_tables_to_appear_in_same_query!({a}, {b});");
	}

	for table in schema.tables() {
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		let a = table_mod_ident(&table);
		for fk in table.foreign_keys() {
			// Both should be equal
			if fk.source_columns().len() != 1 || fk.target_columns().len() != 1 {
				continue;
			}
			let target = fk.target_columns()[0];
			let target_table = fk.target_table();
			let target_column = target_table.schema_column(target);
			if !target_column.is_pk_full() {
				continue;
			}
			let b = table_mod_ident(&target_table);
			let column = column_ident(&target_column);
			println!("diesel::prelude::joinable!({a} -> {b} ({column}));")
		}
	}

	println!();
	println!("#[allow(unused_parens)]");
	println!("pub mod orm {{");
	println!("pub use std::future::Future;");
	println!("use super::sql_types::*;");
	println!("use diesel::{{Insertable, Identifiable, Queryable, Selectable, AsChangeset, sql_types as dt, QueryDsl, SelectableHelper, result::Error, ExpressionMethods}};");
	println!("use diesel_async::RunQueryDsl;");
	println!("use super::user_types as ut;");

	for table in schema.tables() {
		println!();
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		let struct_ident = table_struct_ident(&table);
		let table_ident = table_mod_ident(&table);

		let mut new_table = String::new();
		let mut load_table = String::new();
		let mut update_table = String::new();

		let mut tables = [
			(TableKind::New, &mut new_table),
			(TableKind::Load, &mut load_table),
			(TableKind::Update, &mut update_table),
		];

		for (k, t) in &mut tables {
			match k {
				TableKind::New => {
					wl!(t, "#[derive(Insertable, Identifiable)]");
				}
				TableKind::Load => {
					wl!(
						t,
						"#[derive(Queryable, Selectable, Identifiable, PartialEq)]"
					);
				}
				TableKind::Update => {
					wl!(t, "#[derive(AsChangeset, Identifiable)]");
				}
			}
		}

		for (_k, t) in &mut tables {
			w!(t, "#[diesel(table_name = super::{table_ident}");
			// if check_be && *k == TableKind::Load {
			// 	w!(t, ", check_for_backend(diesel::pg::Pg)");
			// }
			w!(t, ", primary_key(");
			for (i, column) in table.columns().filter(TableColumn::is_pk_part).enumerate() {
				if i != 0 {
					w!(t, ", ");
				}
				let column_name = column_ident(&column);
				w!(t, "{column_name}");
			}

			wl!(t, "))]");
		}

		for (k, t) in &mut tables {
			let k = *k;
			w!(t, "pub struct ");
			match k {
				TableKind::New => w!(t, "New{struct_ident}"),
				TableKind::Load => w!(t, "{struct_ident}"),
				TableKind::Update => w!(t, "{struct_ident}Update"),
			}
			if table_has_lifetime(k, &table) {
				w!(t, "<'a>")
			}
			wl!(t, " {{");
		}
		for (k, t) in &mut tables {
			let k = *k;
			for column in table.columns() {
				if k == TableKind::New && column.default().is_some() {
					continue;
				}
				match column.ty() {
					SchemaType::Scalar(s) => {
						// TODO: disallow setting both at the same time
						if let Some(s) = s
							.attrlist
							.try_get_single::<String>("diesel", "serialize_as")
							.expect("diesel serialize_as")
						{
							wl!(t, "\t#[diesel(serialize_as = {s})]");
						}
					}
					SchemaType::Enum(_) => {}
				}
				w!(t, "\t");

				// Forbid updating pk
				if k == TableKind::Update && column.is_pk_part() {
					// Pass
				} else {
					w!(t, "pub ");
				}

				let field_name = column_ident(&column);
				w!(t, "{field_name}: ");
				if k == TableKind::Update && !column.is_pk_part() {
					w!(t, "Option<");
				}
				if column.nullable {
					w!(t, "Option<");
				}
				let jojo_reference = is_jojo_reference(k, is_copy(&column.ty()), &column);
				if jojo_reference {
					w!(t, "&'a ");
				}
				column_ty_name(jojo_reference, &column.ty(), t);
				if column.nullable {
					w!(t, ">");
				}
				if k == TableKind::Update && !column.is_pk_part() {
					w!(t, ">");
				}
				wl!(t, ",");
			}
		}
		for (_k, t) in &mut tables {
			wl!(t, "}}");
		}
		for (k, t) in &mut tables {
			let k = *k;
			w!(t, "impl<'a> ");
			match k {
				TableKind::New => w!(t, "New{struct_ident}"),
				TableKind::Load => w!(t, "{struct_ident}"),
				TableKind::Update => w!(t, "{struct_ident}Update"),
			}
			if table_has_lifetime(k, &table) {
				w!(t, "<'a>")
			}
			wl!(t, " {{");
		}
		for (k, t) in &mut tables {
			let k = *k;
			if k == TableKind::New {
				let table_ident = table_mod_ident(&table);
				w!(t, "\tpub async fn insert(self, db: &mut ");
				format_db_arg(t);
				wl!(t, ") -> Result<(), Error>");
				// format_where(t);
				wl!(t, "\t{{");
				wl!(t, "\t\tassert_send(diesel::insert_into(super::{table_ident}::table).values(self).execute(db)).await?;");
				wl!(t, "\t\tOk(())");
				wl!(t, "\t}}");
				w!(t, "\tpub async fn delete(&self, db: &mut ");
				format_db_arg(t);
				wl!(t, ") -> Result<(), Error>");
				// format_where(t);
				wl!(t, "\t{{");
				wl!(
					t,
					"\t\tassert_send(diesel::delete(self).execute(db)).await?;"
				);
				wl!(t, "\t\tOk(())");
				wl!(t, "\t}}");
			} else if k == TableKind::Load {
				// TODO: find works with eq_all, and eq_all is working through eq instead of is_not_distinct_from
				// Thus this method might work poorly with nullable pk fields
				w!(t, "\tpub async fn get(");
				for column in table.columns().filter(TableColumn::is_pk_part) {
					let id = column_ident(&column);
					w!(t, "{id}: ");
					let is_reference = !is_copy(&column.ty());
					if is_reference {
						w!(t, "&");
					}
					column_ty_name(is_reference, &column.ty(), t);
					w!(t, ", ");
				}
				w!(t, "db: &mut ");
				format_db_arg(t);
				wl!(t, ") -> Result<Self, Error>");
				// format_where(t);
				wl!(t, "\t{{");
				w!(
					t,
					"\t\tassert_send(super::{table_ident}::table.select(Self::as_select()).find(("
				);
				for (i, column) in table.columns().filter(TableColumn::is_pk_part).enumerate() {
					if i != 0 {
						w!(t, ", ");
					}
					let id = column_ident(&column);
					w!(t, "{id}")
				}
				wl!(t, ")).get_result(db)).await");
				wl!(t, "\t}}");

				for source_table in schema.tables() {
					if source_table.name.id() == table.name.id() {
						continue;
					}
					let source_table = SchemaTable {
						schema: &schema,
						table: source_table,
					};
					for fk in source_table.foreign_keys() {
						if fk.target_table().name.id() != table.name.id() {
							continue;
						}

						w!(t, "\tpub async fn ");
						let many = fk.cardinality().0 == Cardinality::Many;
						w!(t, "{}", fk_method_ident(&source_table, many));
						w!(t, "(&self, db: &mut ");
						format_db_arg(t);
						wl!(t, ") -> Result<");

						if many {
							w!(t, "Vec<");
						}
						w!(t, "{}", table_struct_ident(&source_table));
						if many {
							w!(t, ">");
						}

						wl!(t, ", Error>");
						// format_where(t);
						wl!(t, "\t{{");
						let table_name = table_mod_ident(&source_table);
						wl!(t, "\t\tuse super::{table_name}::{{dsl, table}};");
						// super::username_histories::table
						// 	.select(UsernameHistory::as_select())
						// 	.filter(
						// 		super::username_histories::dsl::user_id.eq(&self.user_id),
						// 	)
						// 	.load(db)
						// 	.await;
						let struct_ident = table_struct_ident(&source_table);
						wl!(t, "\t\tassert_send(table");
						wl!(t, "\t\t\t.select({struct_ident}::as_select())");
						for (a, b) in fk
							.source_columns()
							.into_iter()
							.zip(fk.target_columns().into_iter())
						{
							let a = fk.table.schema_column(a);
							let b = table.schema_column(b);
							let a = column_ident(&a);
							let b = column_ident(&b);
							wl!(t, "\t\t\t.filter(dsl::{a}.eq(&self.{b}))");
						}
						if many {
							wl!(t, "\t\t\t.load(db)");
						} else {
							wl!(t, "\t\t\t.get_result(db)");
						}
						wl!(t, "\t\t\t).await");
						wl!(t, "\t}}");
					}
				}
			}
		}

		for (_k, t) in &mut tables {
			wl!(t, "}}");
		}

		let only_pk_columns = table.columns().all(|v| v.is_pk_part());

		println!("{new_table}");
		println!("{load_table}");
		if !only_pk_columns {
			println!("{update_table}");
		}
	}
	println!("}}");
	Ok(())
}
