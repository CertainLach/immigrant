use std::collections::HashSet;

use crate::{
	renamelist::{reorder_renames, RenameOp, RenameTemp, RenameTempAllocator},
	Diff, HasDbName, HasName,
};

#[derive(Debug, PartialEq)]
pub struct ChangeList<T: HasName> {
	pub dropped: Vec<(T, Option<RenameTemp>)>,
	pub renamed: Vec<RenameOp<T>>,
	pub updated: Vec<Diff<T>>,
	pub created: Vec<T>,
}
impl<T: HasName> ChangeList<T> {
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
pub fn mk_change_list<T: HasName + Clone + IsCompatible>(old: &[T], new: &[T]) -> ChangeList<T> {
	let mut out = <ChangeList<T>>::new();

	let mut old_listed = HashSet::new();
	let mut new_listed = HashSet::new();

	let occupancy = new.iter().map(|n| n.db()).collect::<HashSet<_>>();

	for (oid, old) in old.iter().cloned().enumerate() {
		let mut new_by_exact = new.iter().cloned().enumerate().filter(|(i, f)| {
			!new_listed.contains(i)
				&& f.name().id().name() == old.name().id().name()
				&& f.name().db() == old.name().db()
		});
		if let Some((nid, new)) = new_by_exact.next() {
			assert!(new_by_exact.next().is_none());
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
		let mut new_by_code = new.iter().cloned().enumerate().filter(|(i, f)| {
			f.name().id().name() == old.name().id().name() && !new_listed.contains(i)
		});
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
			.filter(|(i, f)| f.name().db() == old.name().db() && !new_listed.contains(i));
		if let Some((nid, new)) = new_by_db.next() {
			assert!(new_by_db.next().is_none());
			old_listed.insert(oid);
			new_listed.insert(nid);
			out.updated.push(Diff { old, new });
			continue;
		}
		let tmp = if occupancy.contains(&old.db()) {
			Some(allocator.next())
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
			.push((recreated.old.clone(), Some(allocator.next())));
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
	for ele in out.updated.iter() {
		to_rename.push((ele.old.clone(), ele.new.db()));
	}
	let mut moveaways = vec![];
	for ele in out.dropped.iter() {
		if let Some(tmp) = ele.1 {
			moveaways.push((ele.0.clone(), tmp));
		} else if new.iter().any(|n| n.name().db() == ele.0.name().db()) {
			moveaways.push((ele.0.clone(), allocator.next()));
		}
	}
	out.renamed = reorder_renames(to_rename, moveaways, &mut allocator);

	out
}

#[cfg(test)]
mod tests {
	use crate::{
		changelist::{mk_change_list, ChangeList, IsCompatible},
		ids::{in_allocator, DbIdent},
		names::{DefName, TypeKind},
		renamelist::{RenameOp, RenameTempAllocator},
		HasName,
	};
	#[test]
	fn changelist_conflict() {
		tracing_subscriber::fmt().init();
		in_allocator(|| {
			#[derive(Clone, Debug, PartialEq)]
			struct P(DefName<TypeKind>);
			impl HasName for P {
				type Kind = TypeKind;

				fn name(&self) -> DefName<Self::Kind> {
					self.0.clone()
				}
			}
			impl IsCompatible for P {
				fn is_compatible(&self, _new: &Self) -> bool {
					true
				}
			}
			fn p(a: &'static str, b: &'static str) -> P {
				P(DefName::alloc((a, Some(b))))
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
			let ren1 = ren.next();

			assert_eq!(
				mk_change_list(&[p("A", "D"), p("C", "B")], &[p("C", "D"), p("A", "B")]),
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
				mk_change_list(&[p("A", "B")], &[p("A", "C"), p("D", "B")]),
				ChangeList {
					renamed: vec![RenameOp::Rename(p("A", "B"), i("C"))],
					updated: vec![diff!(p("A", "B"), p("A", "C"))],
					created: vec![p("D", "B")],

					dropped: vec![],
				}
			);
			assert_eq!(
				mk_change_list(&[p("D", "B"), p("A", "C")], &[p("A", "B")]),
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
