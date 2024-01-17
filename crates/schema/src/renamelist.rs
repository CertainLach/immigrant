//! Bulk renames with topological sorting and cycle breaking

use std::collections::HashSet;

use derivative::Derivative;
use replace_with::replace_with_or_abort_and_return;
use tracing::{info, trace, trace_span};

use crate::{
	ids::{DbIdent, Kind},
	uid::{RenameExt, RenameMap},
};

#[derive(Hash, PartialEq, Eq, Clone, Debug, Copy)]
pub struct RenameTemp(usize);
impl RenameTemp {
	pub fn db<K: Kind>(&self) -> DbIdent<K> {
		DbIdent::new(&format!("tmp_{}", self.0))
	}
}

#[derive(Derivative)]
#[derivative(Clone, PartialEq, Eq, Debug)]
pub enum RenameOp<T: RenameExt> {
	/// Normal rename.
	Rename(T, DbIdent<T::Kind>),
	/// Name conflict has occured, to resolve it, the item should be temporarily renamed...
	/// (Executor should save `T` by index, `RenameTemp`)
	Store(T, RenameTemp),
	/// And then, after the target name is finally free, renamed from the temporary name to the normal.
	/// (Always queued after corresponding `Self::Store`, you should keep track of stored items)
	Restore(RenameTemp, DbIdent<T::Kind>),

	Moveaway(T, RenameTemp),
}
impl<T: RenameExt + Clone> RenameOp<T> {
	fn target(&self) -> NameOrTemp<T> {
		match self {
			RenameOp::Rename(_, v) => NameOrTemp::Name(v.clone()),
			RenameOp::Store(_, v) | RenameOp::Moveaway(_, v) => NameOrTemp::Temp(*v),
			RenameOp::Restore(_, v) => NameOrTemp::Name(v.clone()),
		}
	}
	fn source(&self, rn: &RenameMap) -> NameOrTemp<T> {
		match self {
			RenameOp::Rename(v, _) => NameOrTemp::Name(v.db(rn)),
			RenameOp::Store(v, _) | RenameOp::Moveaway(v, _) => NameOrTemp::Name(v.db(rn)),
			RenameOp::Restore(v, _) => NameOrTemp::Temp(*v),
		}
	}
}
#[derive(Derivative)]
#[derivative(Hash, PartialEq, Eq, Debug)]
enum NameOrTemp<T: RenameExt> {
	#[derivative(Debug = "transparent")]
	Name(DbIdent<T::Kind>),
	#[derivative(Debug = "transparent")]
	Temp(RenameTemp),
}

/// Given renames, create a list of rename operations.
/// This task is not as easy, because during renames, some other names may be reused, i.e
/// ```txt
/// a => b
/// b => c
/// c => a
/// ```
/// Can't be performed directly, because during `a => b` we get a conflict with `b`, because it is only gets renamed
/// at the next line.
///
/// In this cases, temporary name is used:
/// ```txt
/// a => tmp
/// b => c
/// c => a
/// tmp => b
/// ```
///
/// And in the rest of cases, reordering of operations is needed, this method does everything for you.
pub fn reorder_renames<T: RenameExt + Clone>(
	rn: &RenameMap,
	renames: Vec<(T, DbIdent<T::Kind>)>,
	moveaways: Vec<(T, RenameTemp)>,
	allocator: &mut RenameTempAllocator,
) -> Vec<RenameOp<T>> {
	let mut out = Vec::new();
	let mut ops = Vec::new();
	for (t, temp) in moveaways {
		ops.push(RenameOp::Moveaway(t, temp))
	}
	ops.extend(renames.into_iter().map(|(a, b)| RenameOp::Rename(a, b)));
	reorder_renames_inner(rn, ops, &mut out, allocator);
	out
}

fn reorder_renames_inner<T: RenameExt + Clone>(
	rn: &RenameMap,
	mut renames: Vec<RenameOp<T>>,
	out: &mut Vec<RenameOp<T>>,
	id: &mut RenameTempAllocator,
) {
	// No loops possible/reordering needed
	if renames.len() == 1 {
		if let RenameOp::Rename(a, b) = &renames[0] {
			if a.db(rn) == *b {
				return;
			}
		}
		out.extend(renames);
		return;
	}
	let _span = trace_span!("reordering",).entered();
	let mut current_state: HashSet<NameOrTemp<T>> = renames.iter().map(|o| o.source(rn)).collect();

	loop {
		trace!("loop");
		trace!("state: {current_state:?}");
		trace!(
			"items: {:?}",
			renames
				.iter()
				.map(|r| (r.source(rn), r.target()))
				.collect::<Vec<_>>()
		);
		let mut performed = vec![];
		for (i, op) in renames.iter().enumerate() {
			trace!("trying to perform {:?} => {:?}", op.source(rn), op.target());
			if let RenameOp::Rename(a, b) = &op {
				if a.db(rn) == *b {
					// Already has needed name
					// out.push(RenameOp::Rename(a.clone(), *b));
					trace!("success");
					performed.push(i);
					continue;
				}
			}
			if current_state.contains(&op.target()) {
				trace!("delaying");
				continue;
			}
			performed.push(i);
			current_state.remove(&op.source(rn));
			current_state.insert(op.target());
			out.push(op.clone());
		}
		if performed.is_empty() {
			if !renames.is_empty() {
				trace!("postponed some")
			}
			break;
		}
		for ele in performed.into_iter().rev() {
			renames.remove(ele);
		}
	}

	// rest remaining are the loops
	while !renames.is_empty() {
		eprintln!("loop!");
		let mut looped = vec![];
		let mut step = renames.remove(0);
		loop {
			let Some(pos) = renames.iter().position(|r| r.source(rn) == step.target()) else {
				// Finished the loop
				break;
			};
			looped.push(step);
			step = renames.remove(pos);
		}
		looped.push(step);
		assert_eq!(
			looped.first().unwrap().source(rn),
			looped.last().unwrap().target(),
			"this meant to be a loop: {:?} (possible invariant violation, i.e naming conflict in input data?)",
			looped
				.iter()
				.map(|r| (r.source(rn), r.target()))
				.collect::<Vec<_>>()
		);
		let temp = id.next_temp();
		let target = replace_with_or_abort_and_return(looped.first_mut().unwrap(), |v| match v {
			RenameOp::Rename(v, t) => (t, RenameOp::Store(v, temp)),
			_ => unreachable!(),
		});
		let mut fixed_loop = Vec::new();
		reorder_renames_inner(rn, looped, &mut fixed_loop, id);
		fixed_loop.push(RenameOp::Restore(temp, target));
		out.extend(fixed_loop);
	}
}

#[derive(Default)]
pub struct RenameTempAllocator(usize);
impl RenameTempAllocator {
	pub fn next_temp(&mut self) -> RenameTemp {
		self.0 += 1;
		RenameTemp(self.0)
	}
}
