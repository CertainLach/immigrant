use std::{
	collections::{BTreeMap, HashSet},
	mem,
	ops::{Deref, DerefMut},
	str::FromStr,
};

use inflector::{cases::snakecase::to_snake_case, string::pluralize::to_plural};
use itertools::{Either, Itertools};

use crate::{
	composite::Composite,
	diagnostics::Report,
	ids::{DbIdent, Ident},
	index::{Check, Index, PrimaryKey, UniqueConstraint},
	root::{Item, Schema},
	scalar::{Enum, Scalar, ScalarAnnotation},
	sql::Sql,
	table::{Table, TableAnnotation},
	uid::{RenameExt, RenameMap},
	view::View,
	w, HasIdent, SchemaComposite, SchemaEnum, SchemaItem, SchemaTable,
};

/// Can be updated in database source code, and some are already doing that,
/// but for simplicity assuming default here.
const MAX_IDENTIFIER_LEN: usize = 63;

#[derive(Clone, Copy)]
pub enum NamingConvention {
	Postgres,
}
impl FromStr for NamingConvention {
	type Err = &'static str;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"postgres" => Ok(Self::Postgres),
			_ => Err("unknown naming convention"),
		}
	}
}

pub struct Pgnc<T>(pub T);
impl<T> Deref for Pgnc<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}
impl<T> DerefMut for Pgnc<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl Pgnc<&mut Schema> {
	// TODO: Split into merge and renaming phases, so that renames may work with `SchemaItem` instead of raw `Item`?
	pub fn process_naming(&mut self, rn: &mut RenameMap) {
		for item in self.0 .0.iter_mut() {
			match item {
				Item::Table(t) => {
					let mut t = Pgnc(t);
					t.generate_name(rn);
					t.generate_column_names(rn);
					t.merge(rn);
					t.generate_names(rn);
				}
				Item::Scalar(s) => {
					let mut s = Pgnc(s);
					s.generate_name(rn);
					s.generate_names(rn);
					s.merge_checks(rn);
				}
				Item::Enum(e) => {
					let e = Pgnc(e);
					e.generate_name(rn);
					e.generate_item_names(rn);
				}
				Item::Composite(c) => {
					let c = Pgnc(c);
					c.generate_name(rn);
					c.generate_item_names(rn);
				}
				Item::View(v) => {
					let c = Pgnc(v);
					c.generate_name(rn);
				}
				Item::Mixin(_) => unreachable!("mixins are assimilated"),
			}
		}
	}
}

/// First generate names, because only check constraint are mergeable, and there is no problem with merging unrelated
/// constraints, then merge constraints with the same name.
impl Pgnc<&mut Scalar> {
	/// Generate name for the scalar itself
	pub fn generate_name(&self, rn: &mut RenameMap) {
		if self.db_assigned(rn) {
			return;
		}
		let id = self.id().name();
		self.set_db(rn, DbIdent::new(&id));
	}
	/// In postgres, first check constraint has autogenerated `table_check` name, second is `table_check1` and so on.
	/// In immigrant, constraints with the same name are merged, so only name we should save to every constraint is
	/// `table_check`
	pub fn generate_names(&mut self, rn: &mut RenameMap) {
		let name = self.db(rn);
		for ann in &mut self.annotations {
			if let ScalarAnnotation::Check(c) = ann {
				if c.db_assigned(rn) {
					continue;
				}
				c.set_db(rn, DbIdent::new(&format!("{}_check", name.raw())));
			}
		}
	}
	/// All checks with the same name are merged using AND
	pub fn merge_checks(&mut self, rn: &RenameMap) {
		let (checks, mut annotations): (Vec<_>, Vec<_>) = mem::take(&mut self.annotations)
			.into_iter()
			.partition_map(|a| match a {
				ScalarAnnotation::Check(c) => Either::Left(c),
				a => Either::Right(a),
			});
		let merged_checks = checks
			.into_iter()
			.map(|c| (c.db(rn), c.check))
			.into_group_map()
			.into_iter()
			.collect::<BTreeMap<_, _>>();
		for (name, checks) in merged_checks {
			annotations.push(ScalarAnnotation::Check(Check::new(
				Some(name),
				Sql::all(checks),
			)))
		}
		self.annotations = annotations;
	}
}

fn truncate_auto_name(name: String, suf: &str) -> String {
	use blake2::Digest;
	// FIXME: MAX_IDENTIFIER_LEN should be provided from generator engine.
	#[allow(clippy::int_plus_one)]
	if name.len() + suf.len() + 1 <= MAX_IDENTIFIER_LEN {
		return format!("{name}_{suf}");
	}
	let hash = blake2::Blake2s256::digest(name.as_bytes());
	let hash = base32::encode(base32::Alphabet::Crockford, hash.as_slice());
	let suf = format!("_{}_{suf}", &hash[0..6]);
	// format!("", name[])
	// FIXME: Technically, it is not valid to slice name, because it might be utf-8, but in reality it won't.
	format!("{}{suf}", &name[..MAX_IDENTIFIER_LEN - suf.len()])
}

/// First merge constraints, to allow specifying partial primary keys, i.e
/// ```immigrant
/// a @primary_key;
/// b @primary_key;
/// c @primary_key;
/// ```
///
/// Should be equivalent to
/// ```immigrant
/// a;
/// b;
/// c;
/// @primary_key(a, b, c)
/// ```
///
/// Then generate names, i.e for the specified example it will be `table_a_b_c_pk`
impl Pgnc<&mut Table> {
	/// Generate name for the table itself
	pub fn generate_name(&self, rn: &mut RenameMap) {
		if self.db_assigned(rn) {
			return;
		}
		let id = self.id().name();
		let id = if self.attrlist.get_single("pgnc", "as_is") == Ok(true) {
			id
		} else {
			let id = to_snake_case(&id);
			to_plural(&id)
		};
		self.set_db(rn, DbIdent::new(&id));
	}
	pub fn generate_column_names(&self, rn: &mut RenameMap) {
		for column in self.columns.iter() {
			if column.db_assigned(rn) {
				continue;
			}
			column.set_db(rn, DbIdent::new(&column.id().name()))
		}
	}
	/// Merge annotations:
	/// - Primary keys are always merged, it is assumed at most only name will be set. TODO: It gets weird in presence of mixins, e.g mixin can add another primary key, how that should be handled?
	/// - Checks with the same name (+all unnamed) are merged using AND
	/// - Unique constraints are merged the same way as the primary key, but unnamed uniques are not merged
	/// - Indexes are merged the same way as unique constraints, except accounting for the uniqueness flag
	pub fn merge(&mut self, rn: &RenameMap) {
		let annotations = mem::take(&mut self.annotations);

		// PK
		let pk_name = annotations
			.iter()
			.filter_map(TableAnnotation::as_primary_key)
			.filter_map(|pk| pk.try_db(rn))
			.at_most_one()
			.expect("at most one pk have name set");
		let (pks, mut annotations): (Vec<_>, Vec<_>) =
			annotations.into_iter().partition_map(|a| match a {
				TableAnnotation::PrimaryKey(pk) => Either::Left(pk),
				a => Either::Right(a),
			});
		if !pks.is_empty() {
			annotations.push(TableAnnotation::PrimaryKey(PrimaryKey::new(
				pk_name,
				pks.into_iter().flat_map(|pk| pk.columns).collect(),
			)));
		}

		// Unique
		let (unqs, mut annotations): (Vec<_>, Vec<_>) =
			annotations.into_iter().partition_map(|a| match a {
				TableAnnotation::Unique(u) => Either::Left(u),
				a => Either::Right(a),
			});
		let (named_unqs, unnamed_unqs) = unqs
			.into_iter()
			.partition::<Vec<_>, _>(|u| u.db_assigned(rn));
		let named_unqs = named_unqs
			.into_iter()
			.map(|u| (u.db(rn), u.columns))
			.into_group_map()
			.into_iter()
			.collect::<BTreeMap<_, _>>();
		for (name, cols) in named_unqs {
			annotations.push(TableAnnotation::Unique(UniqueConstraint::new(
				Some(name),
				cols.into_iter().flatten().collect(),
			)))
		}
		for unq in unnamed_unqs {
			annotations.push(TableAnnotation::Unique(unq));
		}

		// Check
		let (checks, mut annotations): (Vec<_>, Vec<_>) =
			annotations.into_iter().partition_map(|a| match a {
				TableAnnotation::Check(c) => Either::Left(c),
				a => Either::Right(a),
			});
		let (named_cks, unnamed_cks) = checks
			.into_iter()
			.partition::<Vec<_>, _>(|c| c.db_assigned(rn));
		let named_cks = named_cks
			.into_iter()
			.map(|c| (c.db(rn), c.check))
			.into_group_map()
			.into_iter()
			.collect::<BTreeMap<_, _>>();
		for (name, checks) in named_cks {
			annotations.push(TableAnnotation::Check(Check::new(
				Some(name),
				Sql::all(checks),
			)))
		}
		if !unnamed_cks.is_empty() {
			annotations.push(TableAnnotation::Check(Check::new(
				None,
				Sql::all(unnamed_cks.into_iter().map(|c| c.check)),
			)));
		}

		// Index
		let (indexes, mut annotations): (Vec<_>, Vec<_>) =
			annotations.into_iter().partition_map(|a| match a {
				TableAnnotation::Index(i) => Either::Left(i),
				a => Either::Right(a),
			});
		let (named_idxs, unnamed_idxs) = indexes
			.into_iter()
			.partition::<Vec<_>, _>(|i| i.db_assigned(rn));
		let named_idxs = named_idxs
			.into_iter()
			.map(|i| {
				(
					(
						i.unique,
						i.using.clone(),
						i.default_opclass.clone(),
						i.with.clone(),
						i.db(rn),
					),
					i.fields().to_vec(),
				)
			})
			.into_group_map()
			.into_iter()
			.collect::<BTreeMap<_, _>>();
		for ((unique, using, default_opclass, with, name), fields) in named_idxs {
			annotations.push(TableAnnotation::Index(Index::new(
				Some(name),
				unique,
				fields.into_iter().flatten().collect(),
				using,
				default_opclass,
				with,
			)))
		}
		for idx in unnamed_idxs {
			annotations.push(TableAnnotation::Index(idx))
		}
		self.annotations = annotations;
	}
	pub fn generate_names(&mut self, rn: &mut RenameMap) {
		let mut decided_names = Vec::new();
		for ann in self.annotations.iter() {
			match ann {
				TableAnnotation::Index(i) if !i.db_assigned(rn) => {
					let mut out = self.db(rn).raw().to_string();
					for column in self.db_names(i.fields().iter().map(|v| &v.0).cloned(), rn) {
						w!(out, "_{}", column.raw());
					}

					decided_names.push(Some(truncate_auto_name(
						out,
						if i.unique { "key" } else { "idx" },
					)));
				}
				TableAnnotation::Check(c) if !c.db_assigned(rn) => {
					let mut out = self.db(rn).raw().to_string();
					for ele in self.db_names(c.check.affected_columns(), rn) {
						w!(out, "_{}", ele.raw());
					}
					decided_names.push(Some(truncate_auto_name(out, "check")));
				}
				TableAnnotation::Unique(u) if !u.db_assigned(rn) => {
					let mut out = self.db(rn).raw().to_string();
					for ele in self.db_names(u.columns.iter().cloned(), rn) {
						w!(out, "_{}", ele.raw());
					}
					decided_names.push(Some(truncate_auto_name(out, "key")));
				}
				TableAnnotation::PrimaryKey(p) if !p.db_assigned(rn) => {
					let mut out = self.db(rn).raw().to_string();
					for ele in self.db_names(p.columns.iter().cloned(), rn) {
						w!(out, "_{}", ele.raw());
					}
					decided_names.push(Some(truncate_auto_name(out, "pkey")));
				}
				_ => decided_names.push(None),
			}
		}
		assert_eq!(decided_names.len(), self.annotations.len());
		for (i, ann) in self.annotations.iter_mut().enumerate() {
			let name = decided_names[i].clone();
			match ann {
				TableAnnotation::Index(i) if !i.db_assigned(rn) => {
					i.set_db(rn, DbIdent::new(&name.unwrap()));
				}
				TableAnnotation::PrimaryKey(p) if !p.db_assigned(rn) => {
					p.set_db(rn, DbIdent::new(&name.unwrap()));
				}
				TableAnnotation::Check(c) if !c.db_assigned(rn) => {
					c.set_db(rn, DbIdent::new(&name.unwrap()));
				}
				TableAnnotation::Unique(u) if !u.db_assigned(rn) => {
					u.set_db(rn, DbIdent::new(&name.unwrap()));
				}
				_ => assert!(name.is_none(), "unexpected name for {ann:?}: {name:?}"),
			}
		}
		for fk in self.foreign_keys.iter() {
			let mut out = self.db(rn).raw().to_string();
			let fields = fk
				.source_fields
				.as_ref()
				.or(fk.target_fields.as_ref())
				.expect("source or target should be set");
			for ele in self.db_names(fields.iter().cloned(), rn) {
				w!(out, "_{}", ele.raw());
			}
			fk.set_db(rn, DbIdent::new(&truncate_auto_name(out, "fk")));
		}
	}
}

impl Pgnc<&mut Enum> {
	fn generate_name(&self, rn: &mut RenameMap) {
		if self.db_assigned(rn) {
			return;
		}
		let id = self.id().name();
		self.set_db(rn, DbIdent::new(&id));
	}
	fn generate_item_names(&self, rn: &mut RenameMap) {
		for ele in self.items.iter() {
			if ele.db_assigned(rn) {
				continue;
			}
			let id = ele.id().name();
			ele.set_db(rn, DbIdent::new(&id));
		}
	}
}

impl Pgnc<&mut Composite> {
	fn generate_name(&self, rn: &mut RenameMap) {
		if self.db_assigned(rn) {
			return;
		}
		let id = self.id().name();
		self.set_db(rn, DbIdent::new(&id));
	}
	fn generate_item_names(&self, rn: &mut RenameMap) {
		for ele in self.fields.iter() {
			if ele.db_assigned(rn) {
				continue;
			}
			let id = ele.id().name();
			ele.set_db(rn, DbIdent::new(&id));
		}
	}
}

impl Pgnc<&mut View> {
	/// Generate name for the table itself
	pub fn generate_name(&self, rn: &mut RenameMap) {
		if self.db_assigned(rn) {
			return;
		}
		let id = self.id().name();
		// FIXME: Report error on truncation
		let id = if self.attrlist.get_single("pgnc", "as_is") == Ok(true) {
			id
		} else {
			let id = to_snake_case(&id);
			to_plural(&id)
		};
		self.set_db(rn, DbIdent::new(&id));
	}
}

pub fn check_unique_in_table(table: &SchemaTable, diagnostics: &mut Report) {
	let seen = &mut HashSet::new();
	for column in table.columns() {
		check_unique(seen, column.id().to_unknown(), diagnostics);
	}
}
pub fn check_unique_in_enum(en: &SchemaEnum, diagnostics: &mut Report) {
	let seen = &mut HashSet::new();
	for item in en.items() {
		check_unique(seen, item.id().to_unknown(), diagnostics);
	}
}
pub fn check_unique_in_composite(comp: &SchemaComposite, diagnostics: &mut Report) {
	let seen = &mut HashSet::new();
	for field in comp.fields() {
		check_unique(seen, field.id().to_unknown(), diagnostics);
	}
}

pub fn check_unique_mixin_identifiers(schema: &Schema, diagnostics: &mut Report) {
	let seen = &mut HashSet::new();
	for mixin in &schema.0 {
		let Item::Mixin(mixin) = mixin else {
			continue;
		};
		check_unique(seen, mixin.id(), diagnostics);
	}
}

fn check_unique<K>(seen: &mut HashSet<Ident<K>>, id: Ident<K>, diagnostics: &mut Report) {
	if !seen.insert(id) {
		let old = seen.get(&id).expect("exists");

		// old.span()
		diagnostics
			.error("duplicate identifier")
			.annotate("declared here", id.span())
			.annotate("previously declared here", old.span());
	}
}

/// Basic check if there is any definition with duplicate identifier
/// Database identifiers are not checked here, because they might be only available after naming convention processing,
/// and there is some variance between different database implementations.
/// E.g some databases allow domain and table types to be conflicting, and some not, there is different behavior for
/// truncation, there might be forbidden system tables, and so on.
/// Identifiers are unique to immigrant, so checking of them is trivial.
pub fn check_unique_identifiers(schema: &Schema, diagnostics: &mut Report) {
	let seen = &mut HashSet::new();
	for item in &schema.items() {
		match item {
			SchemaItem::Table(t) => {
				check_unique(seen, t.id().to_unknown(), diagnostics);
				check_unique_in_table(t, diagnostics);
			}
			SchemaItem::Enum(e) => {
				check_unique(seen, e.id().to_unknown(), diagnostics);
				check_unique_in_enum(e, diagnostics);
			}
			SchemaItem::Scalar(s) => {
				check_unique(seen, s.id().to_unknown(), diagnostics);
			}
			SchemaItem::Composite(c) => {
				check_unique(seen, c.id().to_unknown(), diagnostics);
				check_unique_in_composite(c, diagnostics);
			}
			SchemaItem::View(v) => {
				check_unique(seen, v.id().to_unknown(), diagnostics);
			}
		}
	}
}
