use std::{collections::HashSet, fmt::Debug};

use crate::{
	renamelist::{reorder_renames, RenameOp, RenameTemp, RenameTempAllocator},
	uid::{RenameExt, RenameMap},
	Diff, HasIdent,
};

#[derive(Debug, PartialEq)]
pub struct ChangeList<T: RenameExt + HasIdent> {
	pub dropped: Vec<(T, Option<RenameTemp>)>,
	pub renamed: Vec<RenameOp<T>>,
	pub updated: Vec<Diff<T>>,
	pub created: Vec<T>,
}
impl<T: RenameExt + HasIdent> ChangeList<T> {
	fn new() -> Self {
		Self {
			renamed: vec![],
			created: vec![],
			updated: vec![],
			dropped: vec![],
		}
	}
}

pub trait IsCompatible {
	fn is_compatible(&self, new: &Self) -> bool;
}

/// Given the old and new list of named items, classify differences between them as renames (see `crate::renamelist`),
/// creates, drops, and updates.
///
/// If values are not compatible by the decision of `IsCompatible` trait, then they will not be updated, and instead
/// old version will be dropped, and the new version will be created.
pub fn mk_change_list<T: RenameExt + Clone + IsCompatible + HasIdent + Debug>(
	rn: &RenameMap,
	old: &[T],
	new: &[T],
) -> ChangeList<T> {
	let mut out = <ChangeList<T>>::new();

	let mut old_listed = HashSet::new();
	let mut new_listed = HashSet::new();

	let occupancy = new.iter().map(|n| n.db(rn)).collect::<HashSet<_>>();

	for (oid, old) in old.iter().cloned().enumerate() {
		let mut new_by_exact = new.iter().cloned().enumerate().filter(|(i, f)| {
			!new_listed.contains(i) && f.id().name() == old.id().name() && f.db(rn) == old.db(rn)
		});
		if let Some((nid, new)) = new_by_exact.next() {
			{
				let other = new_by_exact.next();
				assert!(
					other.is_none(),
					"second exact match shouldn't be possible: {nid} {new:?} {other:?}"
				);
			}
			old_listed.insert(oid);
			new_listed.insert(nid);
			out.updated.push(Diff { old, new });
			continue;
		}
	}
	for (oid, old) in old.iter().cloned().enumerate() {
		if old_listed.contains(&oid) {
			continue;
		}
		let mut new_by_code = new
			.iter()
			.cloned()
			.enumerate()
			.filter(|(i, f)| f.id().name() == old.id().name() && !new_listed.contains(i));
		if let Some((nid, new)) = new_by_code.next() {
			assert!(new_by_code.next().is_none());
			old_listed.insert(oid);
			new_listed.insert(nid);
			out.updated.push(Diff { old, new });
			continue;
		}
	}

	let mut allocator = RenameTempAllocator::default();
	for (oid, old) in old.iter().cloned().enumerate() {
		if old_listed.contains(&oid) {
			continue;
		}
		let mut new_by_db = new
			.iter()
			.cloned()
			.enumerate()
			.filter(|(i, f)| f.db(rn) == old.db(rn) && !new_listed.contains(i));
		if let Some((nid, new)) = new_by_db.next() {
			assert!(new_by_db.next().is_none());
			old_listed.insert(oid);
			new_listed.insert(nid);
			out.updated.push(Diff { old, new });
			continue;
		}
		let tmp = if occupancy.contains(&old.db(rn)) {
			Some(allocator.next_temp())
		} else {
			None
		};
		out.dropped.push((old, tmp));
	}

	for recreated in out
		.updated
		.iter()
		.filter(|diff| !diff.old.is_compatible(&diff.new))
		.collect::<Vec<_>>()
	{
		out.created.push(recreated.new.clone());
		out.dropped
			.push((recreated.old.clone(), Some(allocator.next_temp())));
	}
	out.updated.retain(|diff| diff.old.is_compatible(&diff.new));

	for (_, new) in new
		.iter()
		.cloned()
		.enumerate()
		.filter(|(i, _)| !new_listed.contains(i))
	{
		out.created.push(new);
	}

	let mut to_rename = vec![];
	for updated in out.updated.iter() {
		to_rename.push((updated.old.clone(), updated.new.db(rn)));
	}
	let mut moveaways = vec![];
	for old_dropped in out.dropped.iter() {
		if let Some(tmp) = old_dropped.1 {
			moveaways.push((old_dropped.0.clone(), tmp));
		} else if new.iter().any(|n| n.db(rn) == old_dropped.0.db(rn)) {
			moveaways.push((old_dropped.0.clone(), allocator.next_temp()));
		}
	}
	out.renamed = reorder_renames(rn, to_rename, moveaways, &mut allocator);

	out
}

#[cfg(test)]
mod tests {
	use crate::{
		changelist::{mk_change_list, ChangeList, IsCompatible},
		ids::{in_allocator, DbIdent, Ident},
		names::{DefName, TypeKind},
		renamelist::{RenameOp, RenameTempAllocator},
		span::{register_source, SimpleSpan},
		uid::{next_uid, HasUid, RenameMap, Uid},
		HasDefaultDbName, HasIdent,
	};
	#[test]
	fn changelist_conflict() {
		tracing_subscriber::fmt().init();
		in_allocator(|| {
			#[derive(Clone, Debug, PartialEq)]
			struct P(Uid, DefName<TypeKind>);
			impl IsCompatible for P {
				fn is_compatible(&self, _new: &Self) -> bool {
					true
				}
			}
			impl HasDefaultDbName for P {
				type Kind = TypeKind;
				fn default_db(&self) -> Option<DbIdent<Self::Kind>> {
					self.1.default_db()
				}
			}
			impl HasIdent for P {
				type Kind = TypeKind;
				fn id(&self) -> Ident<Self::Kind> {
					self.1.id()
				}
			}
			impl HasUid for P {
				fn uid(&self) -> Uid {
					self.0
				}
			}
			fn p(a: &'static str, b: &'static str) -> P {
				let s = a.to_string();
				let s = register_source(s);
				P(
					next_uid(),
					DefName::alloc((SimpleSpan::new(s, 0, a.len() as u32), a, Some(b))),
				)
			}
			fn i(n: &'static str) -> DbIdent<TypeKind> {
				DbIdent::new(n)
			}
			macro_rules! diff {
				($a:expr, $b:expr) => {
					crate::Diff { old: $a, new: $b }
				};
			}
			let mut ren = RenameTempAllocator::default();
			let ren1 = ren.next_temp();

			assert_eq!(
				mk_change_list(
					&RenameMap::default(),
					&[p("A", "D"), p("C", "B")],
					&[p("C", "D"), p("A", "B")]
				),
				ChangeList {
					renamed: vec![
						RenameOp::Store(p("A", "D"), ren1),
						RenameOp::Rename(p("C", "B"), i("D")),
						RenameOp::Restore(ren1, i("B"))
					],
					updated: vec![
						diff!(p("A", "D"), p("A", "B")),
						diff!(p("C", "B"), p("C", "D"))
					],

					created: vec![],
					dropped: vec![],
				}
			);
			assert_eq!(
				mk_change_list(
					&RenameMap::default(),
					&[p("A", "B")],
					&[p("A", "C"), p("D", "B")]
				),
				ChangeList {
					renamed: vec![RenameOp::Rename(p("A", "B"), i("C"))],
					updated: vec![diff!(p("A", "B"), p("A", "C"))],
					created: vec![p("D", "B")],

					dropped: vec![],
				}
			);
			assert_eq!(
				mk_change_list(
					&RenameMap::default(),
					&[p("D", "B"), p("A", "C")],
					&[p("A", "B")]
				),
				ChangeList {
					renamed: vec![
						RenameOp::Store(p("D", "B"), ren1),
						RenameOp::Rename(p("A", "C"), i("B"))
					],
					updated: vec![diff!(p("A", "C"), p("A", "B")),],
					dropped: vec![(p("D", "B"), Some(ren1))],

					created: vec![],
				}
			);
		});
	}
}
