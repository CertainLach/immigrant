use std::{collections::BTreeSet, env::current_dir};

use clap::Parser;
use cli::current_schema;
use file_diffs::find_root;
use inflector::{
	cases::{pascalcase::to_pascal_case, snakecase::to_snake_case},
	string::pluralize,
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, TokenStreamExt};
use rust_format::{Config, Edition, Formatter, PostProcess};
use schema::{
	composite::CompositeField,
	process::NamingConvention,
	root::Schema,
	scalar::EnumItem,
	table::Cardinality,
	uid::{RenameExt, RenameMap},
	HasIdent, SchemaComposite, SchemaEnum, SchemaTable, SchemaType, TableColumn,
};
use syn::{Ident, Index, Path, Type};

mod util;
use util::disjoint_unions;

#[derive(PartialEq, Clone, Copy)]
enum TableKind {
	New,
	Load,
	Update,
}

fn table_mod_ident(t: &SchemaTable) -> Ident {
	let ident = to_snake_case(&t.id().name());
	format_ident!("{}", pluralize::to_plural(&ident))
}
fn fk_method_ident(t: &SchemaTable, plural: bool) -> Ident {
	let ident = to_snake_case(&t.id().name());
	let v = if plural {
		pluralize::to_plural(&ident)
	} else {
		ident
	};
	format_ident!("{v}")
}
fn table_struct_ident(t: &SchemaTable) -> Ident {
	format_ident!("{}", &t.id().name())
}
fn column_ident(c: &TableColumn) -> Ident {
	format_ident!("{}", to_snake_case(&c.id().name()))
}
fn type_ident(c: &SchemaType) -> Ident {
	format_ident!("{}", to_pascal_case(&c.ident().name()))
}
fn enum_ident(c: &SchemaEnum) -> Ident {
	format_ident!("{}", to_pascal_case(&c.id().name()))
}
fn enum_item_ident(c: &EnumItem) -> Ident {
	format_ident!("{}", to_pascal_case(&c.id().name()))
}
fn composite_ident(c: &SchemaComposite) -> Ident {
	format_ident!("{}", to_pascal_case(&c.id().name()))
}
fn field_ident(c: &CompositeField) -> Ident {
	format_ident!("{}", to_snake_case(&c.id().name()))
}

fn is_copy(t: &SchemaType) -> bool {
	match t {
		SchemaType::Scalar(s) => s
			.attrlist
			.get_single("diesel", "copy")
			.expect("diesel copy"),
		// Enums are always copy
		SchemaType::Enum(_) => true,
		SchemaType::Composite(c) => {
			for ele in c.fields() {
				if !is_copy(&c.schema.schema_ty(ele.ty)) {
					return false;
				}
			}
			true
		}
	}
}

/// Should column be excluded from table update structs
fn is_immutable(column: TableColumn<'_>) -> bool {
	// It is dangerous to update PKs, let's disable this pattern in diesel generator, at least for now.
	if column.is_pk_part() {
		return true;
	}
	column
		.attrs
		.get_single("diesel", "immutable")
		.expect("diesel immutable")
}

/// Fields with default values may be fully autogenerated, (e.g created_at, updated_at), or just have some default
/// for database management purposes. Lets assume it is autogenerated, unless it has diesel(manual) attribute.
fn is_autogenerated(column: TableColumn) -> bool {
	let has_default = column.has_default();
	if column
		.attrs
		.get_single("diesel", "manual")
		.expect("diesel manual")
	{
		assert!(
			has_default,
			"diesel(manual) has no effect on columns without default values"
		);
		return false;
	}
	has_default
}

#[allow(clippy::nonminimal_bool)]
fn should_use_reference(k: TableKind, copy: bool, column: TableColumn) -> bool {
	// Load struct is always owned
	k != TableKind::Load
		// Copy types are copied
		&& !copy
		// In update table, pks are immutable
		&& !(k == TableKind::Update && is_immutable(column))
		// default fields are skipped for New table
		&& !(k == TableKind::New && is_autogenerated(column))
}
fn table_has_lifetime(k: TableKind, table: &SchemaTable) -> bool {
	match k {
		TableKind::New | TableKind::Update => {
			let mut has_reference = false;
			for column in table.columns() {
				let copy = is_copy(&column.ty());
				if k == TableKind::New && is_autogenerated(column) {
					continue;
				}
				if should_use_reference(k, copy, column) {
					has_reference = true;
					break;
				}
			}
			has_reference
		}
		TableKind::Load => false,
	}
}

fn column_raw_ty(ty: SchemaType<'_>, nullable: bool) -> TokenStream {
	let ty = match ty {
		SchemaType::Enum(en) => {
			let ident = enum_ident(&en);
			quote!(st::#ident)
		}
		SchemaType::Scalar(s) => {
			let v: String = s
				.attrlist
				.get_single("diesel", "type")
				.expect("!!! failed to parse diesel(type) attribute, it should have name of type implementing SqlType, e.g dt::Text\n!!! See the list of builtin types here: https://docs.rs/diesel/latest/diesel/sql_types/\nUnderlying error");
			let v: Path = syn::parse_str(&v).expect("disesl path");
			quote!(#v)
		}
		SchemaType::Composite(c) => {
			let ident = composite_ident(&c);
			quote!(st::#ident)
		}
	};

	let ty = nullable.then(|| quote!(dt::Nullable<#ty>)).unwrap_or(ty);
	quote! {#ty}
}

fn column_ty(kind: TableKind, column: TableColumn<'_>) -> TokenStream {
	let should_use_reference = should_use_reference(kind, is_copy(&column.ty()), column);
	let ty = column_ty_name(should_use_reference, &column.ty());
	let ty = should_use_reference.then(|| quote!(&'a #ty)).unwrap_or(ty);
	let ty = column.nullable.then(|| quote!(Option<#ty>)).unwrap_or(ty);
	(kind == TableKind::Update && !column.is_pk_part()
		|| kind == TableKind::New && column.has_default())
	.then(|| quote!(Option<#ty>))
	.unwrap_or(ty)
}

fn field_ty(field: CompositeField<'_>) -> TokenStream {
	let ty = column_ty_name(false, &field.ty());
	field.nullable.then(|| quote!(Option<#ty>)).unwrap_or(ty)
}

fn format_db_arg() -> TokenStream {
	quote!(crate::user_types::Db<'_>)
}
fn column_ty_name(jojo_reference: bool, ty: &SchemaType) -> TokenStream {
	let id = type_ident(ty);
	match ty {
		SchemaType::Scalar(s) => {
			let native = s
				.attrlist
				.try_get_single::<String>("diesel", "native")
				.expect("failed to parse #diesel(native = \"...\")");
			let native_ref = s
				.attrlist
				.try_get_single::<String>("diesel", "native_ref")
				.expect("failed to parse #diesel(native_ref = \"...\")");
			let custom: bool = s
				.attrlist
				.get_single("diesel", "custom")
				.expect("failed to parse #diesel(custom)");

			if native_ref.is_some() && native.is_none() {
				panic!("if you use native_ref, you should also set native");
			}
			if custom && native.is_some() {
				panic!("#diesel(custom) is mutually exclusive with #diesel(native)");
			}
			if !custom && native.is_none() {
				panic!("#diesel() annotation should specify type kind, i.e native = \"u32\" or custom (Then its implementation will be delegated to user_types module)");
			}

			// TODO: disallow setting both at the same time
			if let Some(s) = native {
				match native_ref {
					Some(native_ref) if jojo_reference => {
						let v: Type = syn::parse_str(&native_ref).expect("invalid ty");
						quote!(#v)
					}
					_ => {
						let v: Type = syn::parse_str(&s).expect("invalid ty");
						quote!(#v)
					}
				}
			} else if custom {
				quote!(ut::#id)
			} else {
				panic!("either custom or native type is expected")
			}
		}
		SchemaType::Enum(_) => {
			// Always copy
			quote!(enum_types::#id)
		}
		SchemaType::Composite(_) => {
			// Always copy
			quote!(composite_types::#id)
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
	PrintSchema {
		#[clap(long)]
		naming_convention: NamingConvention,
	},
}

fn main() -> anyhow::Result<()> {
	let opts = Opts::parse();
	match opts {
		// FIXME: Use naming convention
		Opts::PrintSchema {
			naming_convention: _,
		} => print_schema(),
	}
}
fn print_schema() -> anyhow::Result<()> {
	let root = find_root(&current_dir()?)?;

	let mut config = root.clone();
	config.push("immigrant-diesel.jsonnet");

	let (_, schema, rn) = current_schema(&root)?;
	let rn = &rn;
	let generated = generate_schema(schema, rn)?;
	println!("{generated}");
	Ok(())
}

fn generate_schema(schema: Schema, rn: &RenameMap) -> anyhow::Result<String> {
	let mut out = TokenStream::new();
	out.append_all(quote! {
		pub mod user_types;
		use user_types as ut;
		use diesel::sql_types as dt;
	});

	let mut types = TokenStream::new();
	for en in schema.enums() {
		let en = SchemaEnum {
			schema: &schema,
			en,
		};
		let id = enum_ident(&en);
		let name = en.db(rn);
		let name = name.raw();
		types.append_all(quote! {
			#[derive(SqlType, QueryId)]
			#[diesel(postgres_type(name = #name))]
			pub struct #id;
		});
	}
	for composite in schema.composites() {
		let composite = SchemaComposite {
			schema: &schema,
			composite,
		};
		let id = composite_ident(&composite);
		let name = composite.db(rn);
		let name = name.raw();
		types.append_all(quote! {
			#[derive(SqlType, QueryId)]
			#[diesel(postgres_type(name = #name))]
			pub struct #id;
		});
	}
	out.append_all(quote! {
		pub mod sql_types {
			use diesel::{sql_types::SqlType, QueryId};
			#types
		}
		use sql_types as st;
	});

	let mut enums = TokenStream::new();
	for en in schema.enums() {
		let en = SchemaEnum {
			schema: &schema,
			en,
		};
		let id = enum_ident(&en);

		let derives = en
			.attrlist
			.get_multi::<String>("diesel", "derive")
			.expect("diesel derive")
			.into_iter()
			.map(|v| syn::parse_str::<Path>(&v).unwrap());

		let serde: bool = en
			.attrlist
			.get_single("diesel", "serde")
			.expect("diesel serde");

		let serde_derive = serde.then(|| quote!(, serde::Serialize, serde::Deserialize));

		let name = format!("crate::sql_types::{id}");
		let items = en.items.iter().map(|i| {
			let id = enum_item_ident(i);
			let name = i.db(rn);
			let name = name.raw();
			let docs = &i.docs;
			let serde_rename = serde.then(|| quote!(#[serde(rename = #name)]));
			quote! {
				#[db_rename = #name]
				#serde_rename
				#(#[doc = #docs])*
				#id
			}
		});

		let values = en.items.iter().map(|i| {
			let name = i.db(rn);
			let name = name.raw();
			quote!{#name}
		});
		let value_count = en.items.len();

		let docs = &en.docs;
		enums.append_all(quote! {
			#(#[doc = #docs])*
			#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq, Eq, Clone, Copy, Hash #serde_derive #(, #derives)*)]
			#[ExistingTypePath = #name]
			pub enum #id {
				#(#items,)*
			}

			impl #id {
				pub const VALUES: [&str; #value_count] = [
					#(#values,)*
				];
			}
		});
	}
	out.append_all(quote! {
		pub mod enum_types {
			#enums
		}
	});

	let mut composites = TokenStream::new();
	for composite in schema.composites() {
		let composite = SchemaComposite {
			schema: &schema,
			composite,
		};
		let id = composite_ident(&composite);

		let derives = composite
			.attrlist
			.get_multi::<String>("diesel", "derive")
			.expect("diesel derive")
			.into_iter()
			.map(|v| syn::parse_str::<Path>(&v).unwrap());

		let mut copy = true;
		let fields = composite
			.fields()
			.map(|f| {
				if !is_copy(&f.ty()) {
					copy = false;
				}
				let id = field_ident(&f);
				let ty = field_ty(f);
				let docs = &f.docs;
				quote! {
					#(#[doc = #docs])*
					pub #id: #ty,
				}
			})
			.collect::<Vec<_>>();

		let ser_list = composite.fields().map(|f| {
			let id = field_ident(&f);
			quote! {&self.#id}
		});

		let des_list = composite.fields().enumerate().map(|(i, f)| {
			let id = field_ident(&f);
			let i = Index::from(i);
			quote! {#id: val.#i}
		});

		let raw_tys = composite
			.fields()
			.map(|f| column_raw_ty(f.ty(), f.nullable))
			.collect::<Vec<_>>();
		let tys = composite.fields().map(|f| field_ty(f));

		let sql_type = quote!(crate::sql_types::#id);

		let docs = &composite.docs;
		composites.append_all(quote! {
			#(#[doc = #docs])*
			#[derive(Debug, PartialEq, Eq, Clone, Hash, diesel::expression::AsExpression, diesel::deserialize::FromSqlRow #(, #derives)*)]
			#[diesel(sql_type = #sql_type)]
			pub struct #id {
				#(#fields)*
			}

			impl diesel::serialize::ToSql<#sql_type, diesel::pg::Pg> for #id {
				fn to_sql<'b>(
					&'b self,
					out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
				) -> diesel::serialize::Result {
					diesel::serialize::WriteTuple::<(#(#raw_tys,)*)>::write_tuple(&(#(#ser_list),*), out)
				}
			}

			impl diesel::deserialize::FromSql<#sql_type, diesel::pg::Pg> for #id {
				fn from_sql(
					bytes: <diesel::pg::Pg as diesel::backend::Backend>::RawValue<'_>,
				) -> diesel::deserialize::Result<Self> {
					let val = <(#(#tys),*) as diesel::deserialize::FromSql<diesel::sql_types::Record<(#(#raw_tys),*)>, diesel::pg::Pg>>::from_sql(bytes)?;
					Ok(Self {
						#(#des_list,)*
					})
				}
			}
		});
	}

	out.append_all(quote! {
		pub mod composite_types {
			use super::*;
			#composites
		}
	});

	for table in schema.tables() {
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		let docs = table.docs.iter();
		let table_name = table.db(rn);
		let table_name = table_name.raw();
		let table_ident = table_mod_ident(&table);
		#[allow(clippy::cmp_owned)]
		let sql_name_attr = if table_name != table_ident.to_string() {
			quote! {#[sql_name = #table_name]}
		} else {
			quote!()
		};

		let pk_columns = table
			.columns()
			.filter(TableColumn::is_pk_part)
			.map(|c| column_ident(&c));

		let mut columns = Vec::new();
		for column in table.columns() {
			let name = column.db(rn);
			let name = name.raw();

			let ident = column_ident(&column);
			#[allow(clippy::cmp_owned)]
			let sql_name_attr = if name != ident.to_string() {
				quote! {#[sql_name = #name]}
			} else {
				quote!()
			};
			let ty = column_raw_ty(column.ty(), column.nullable);
			let docs = &column.docs;
			let doc_sep = if !docs.is_empty() {
				quote!(#[doc = ""])
			} else {
				quote!()
			};
			let db_ty = column.db_type(rn);
			let db_ty = db_ty.raw();
			let column_doc = format!("`{table_name}`.`{name} column`");
			let ty_doc = format!("The SQL type is \"{db_ty}\"`");
			columns.push(quote! {
				#(#[doc = #docs])*
				#doc_sep
				#[doc = #column_doc]
				#[doc = #ty_doc]
				#sql_name_attr
				#ident -> #ty
			})
		}
		out.append_all(quote! {
			diesel::table! {
				use super::{sql_types::*, dt, st};
				#(#[doc = #docs])*
				#sql_name_attr
				#table_ident(#(#pk_columns),*) {
					#(#columns,)*
				}
			}
		});
	}
	let mut allowed = BTreeSet::new();
	for table in schema.tables() {
		let table = SchemaTable {
			schema: &schema,
			table,
		};
		let this = table.id();
		for fk in table.foreign_keys() {
			let other = fk.target_table().id();
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

	{
		let disjoint_input = allowed.to_vec();
		for union in disjoint_unions(&disjoint_input) {
			let tables = union.iter().map(|t| {
				let t = schema.schema_table(t).expect("a");
				table_mod_ident(&t)
			});
			out.append_all(quote!(
				diesel::prelude::allow_tables_to_appear_in_same_query!(#(#tables),*);
			));
		}
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
			out.append_all(quote!(diesel::prelude::joinable!(#a -> #b (#column));));
		}
	}

	let mut orm = TokenStream::new();
	for table in schema.tables() {
		for kind in [TableKind::New, TableKind::Load, TableKind::Update] {
			let table = SchemaTable {
				schema: &schema,
				table,
			};
			let struct_ident = table_struct_ident(&table);
			let table_ident = table_mod_ident(&table);

			let derive_attr = match kind {
				TableKind::New => {
					quote!(#[derive(Insertable, Identifiable)])
				}
				TableKind::Load => {
					let derives = table
						.attrlist
						.get_multi::<String>("diesel", "derive")
						.expect("diesel derive")
						.into_iter()
						.map(|v| syn::parse_str::<Path>(&v).unwrap());
					quote!(#[derive(Queryable, Selectable, Identifiable, PartialEq #(, #derives)*)])
				}
				TableKind::Update => {
					quote!(#[derive(AsChangeset, Identifiable)])
				}
			};

			let pk = table
				.columns()
				.filter(TableColumn::is_pk_part)
				.map(|c| column_ident(&c));
			let diesel_attr =
				quote!(#[diesel(table_name = super::#table_ident, primary_key(#(#pk),*))]);

			let struct_ident = match kind {
				TableKind::New => format_ident!("New{struct_ident}"),
				TableKind::Load => format_ident!("{struct_ident}"),
				TableKind::Update => format_ident!("{struct_ident}Update"),
			};
			let struct_lifetime = table_has_lifetime(kind, &table).then(|| quote!(<'a>));
			let mut columns = Vec::new();
			for column in table.columns() {
				if kind == TableKind::New && is_autogenerated(column) {
					continue;
				}
				let diesel_attr = match column.ty() {
					SchemaType::Scalar(s) => {
						// TODO: disallow setting both at the same time
						if let Some(s) = s
							.attrlist
							.try_get_single::<String>("diesel", "serialize_as")
							.expect("diesel serialize_as")
						{
							quote!(#[diesel(serialize_as = #s)])
						} else {
							quote!()
						}
					}
					SchemaType::Enum(_) => quote!(),
					SchemaType::Composite(_) => quote! {},
				};

				let vis = if kind == TableKind::Update && column.is_pk_part() {
					// Forbid updating pk, pass
					quote!()
				} else if kind == TableKind::Update && is_immutable(column) {
					// Forbid updating immutable columns, note that PK is still included in the struct for
					// #[derive(Identifiable)] implementation.
					continue;
				} else {
					quote!(pub)
				};

				let field_name = column_ident(&column);
				let ty = column_ty(kind, column);
				columns.push(quote! {
					#diesel_attr
					#vis #field_name: #ty
				});
			}
			let mut methods = Vec::new();
			if kind == TableKind::New {
				let table_ident = table_mod_ident(&table);
				let db = format_db_arg();
				// format_where(t);
				methods.push(quote! {
					pub async fn insert(self, db: &mut #db) -> Result<(), Error> {
						assert_send(diesel::insert_into(super::#table_ident::table).values(self).execute(db)).await?;
						Ok(())
					}
				});
				methods.push(quote! {
					pub async fn delete(&self, db: &mut #db) -> Result<(), Error> {
						assert_send(diesel::delete(self).execute(db)).await?;
						Ok(())
					}
				});
			} else if kind == TableKind::Load {
				{
					// TODO: find works with eq_all, and eq_all is working through eq instead of is_not_distinct_from
					// Thus this method might work poorly with nullable pk fields
					let pk = table
						.columns()
						.filter(TableColumn::is_pk_part)
						.map(|column| {
							let id = column_ident(&column);
							let is_reference = !is_copy(&column.ty());
							let reference = is_reference.then(|| quote!(&)).unwrap_or_default();
							let t = column_ty_name(is_reference, &column.ty());
							quote!(#id: #reference #t)
						});
					// format_where(t);
					let find_columns = table
						.columns()
						.filter(TableColumn::is_pk_part)
						.map(|c| column_ident(&c))
						.collect::<Vec<_>>();

					let find_arg = if find_columns.len() == 1 {
						quote!(#(#find_columns)*)
					} else {
						quote!((#(#find_columns),*))
					};

					let db = format_db_arg();
					methods.push(quote! {
						pub async fn get(#(#pk,)* db: &mut #db) -> Result<Self, Error> {
							assert_send(super::#table_ident::table.select(Self::as_select()).find(#find_arg).get_result(db)).await
						}
					});
				}

				for source_table in schema.tables() {
					if source_table.id() == table.id() {
						continue;
					}
					let source_table = SchemaTable {
						schema: &schema,
						table: source_table,
					};
					for fk in source_table.foreign_keys() {
						if fk.target_table().id() != table.id() {
							continue;
						}

						let many = fk.cardinality().0 == Cardinality::Many;
						let method_name = fk_method_ident(&source_table, many);
						// format_where(t);
						let table_name = table_mod_ident(&source_table);
						// super::username_histories::table
						// 	.select(UsernameHistory::as_select())
						// 	.filter(
						// 		super::username_histories::dsl::user_id.eq(&self.user_id),
						// 	)
						// 	.load(db)
						// 	.await;
						let struct_ident = table_struct_ident(&source_table);
						// for (a, b) in
						let filters = fk
							.source_columns()
							.into_iter()
							.zip(fk.target_columns())
							.map(|(a, b)| {
								let a = fk.table.schema_column(a);
								let b = table.schema_column(b);
								let a = column_ident(&a);
								let b = column_ident(&b);
								quote!(.filter(dsl::#a.eq(&self.#b)))
							});
						// }
						let load = if many {
							quote!(.load(db))
						} else {
							quote!(.get_result(db))
						};

						let db = format_db_arg();
						let ret = table_struct_ident(&source_table);
						let ret = many
							.then(|| quote!(Vec<#ret>))
							.unwrap_or_else(|| quote!(#ret));
						methods.push(quote! {
							pub async fn #method_name(&self, db: &mut #db) -> Result<#ret, Error> {
								use super::#table_name::{dsl, table};
								assert_send(
									table
										.select(#struct_ident::as_select())
										#(#filters)*
										#load
								).await
							}
						})
					}
				}
			}

			let only_pk_columns = table.columns().all(|v| v.is_pk_part());

			if kind == TableKind::Update && only_pk_columns {
				// Pass, no updateable columns
			} else {
				orm.append_all(quote! {
					#derive_attr
					#diesel_attr
					pub struct #struct_ident #struct_lifetime {
						#(#columns,)*
					}
					impl #struct_lifetime #struct_ident #struct_lifetime {
						#(#methods)*
					}
				});
			}
		}
	}
	out.append_all(quote!{
		#[allow(unused_parens)]
		pub mod orm {
			use super::*;
			use std::future::Future;
			use super::sql_types::*;
			use diesel::{Insertable, Identifiable, Queryable, Selectable, AsChangeset, sql_types as dt, QueryDsl, SelectableHelper, result::Error, ExpressionMethods};
			use diesel_async::RunQueryDsl;
			use super::user_types as ut;
			fn assert_send<'u, R>(fut: impl 'u + Send + Future<Output = R>) -> impl 'u + Send + Future<Output = R> { fut }

			#orm
		}
	});
	let mut fmt = out.to_string().to_owned();
	fmt.push('\n');
	let config = Config::new_str()
		.edition(Edition::Rust2021)
		.post_proc(PostProcess::ReplaceMarkers);
	let fmt = rust_format::RustFmt::from_config(config)
		.format_str(fmt)
		.expect("rustfmt");
	Ok(fmt)
}

#[cfg(test)]
mod tests {
	use schema::parser::parse;
	use schema::process::NamingConvention;
	use schema::root::SchemaProcessOptions;
	use schema::uid::RenameMap;

	use crate::generate_schema;

	pub fn default_options() -> SchemaProcessOptions {
		SchemaProcessOptions {
			generator_supports_domain: true,
			naming_convention: NamingConvention::Postgres,
		}
	}

	fn test_generator(schema: &str) {
		let mut rn = RenameMap::default();
		let schema = parse(schema, &default_options(), &mut rn).expect("parse result");
		let generated = generate_schema(schema, &rn).expect("generate");
		println!("{generated}");
	}

	#[test]
	fn test_basic() {
		test_generator(include_str!("../tests/basic.schema"));
	}
}
