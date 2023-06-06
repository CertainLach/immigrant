use std::cell::{Cell, RefCell};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::marker::PhantomData;

#[derive(Default)]
pub struct CodeIdentAllocator {
	ids: RefCell<HashMap<String, u16>>,
	max_id: Cell<u16>,

	generation: Cell<u8>,
	max_kind: Cell<u8>,
}
impl CodeIdentAllocator {
	pub fn next_generation(&self) {
		let max_kind = self.max_kind.get();
		self.generation.set(
			self.generation
				.get()
				.checked_add(max_kind + 1)
				.expect("out of generations"),
		);
		self.max_kind.set(0);
	}
	fn name(&self, id: u16) -> String {
		let ids = self.ids.borrow();
		for (k, v) in ids.iter() {
			if *v == id {
				return k.to_owned();
			}
		}
		unreachable!()
	}
	fn ident<K: Kind>(&self, name: &str) -> Ident<K> {
		let kind = K::id();
		self.max_kind.set(self.max_kind.get().max(kind));
		let kind = self
			.generation
			.get()
			.checked_add(kind)
			.expect("out of kinds");
		let mut ids = self.ids.borrow_mut();
		match ids.entry(name.to_owned()) {
			Entry::Occupied(v) => {
				return Ident {
					kind,
					id: *v.get(),
					_marker: PhantomData,
				}
			}
			Entry::Vacant(v) => {
				let id = self.max_id.get();
				self.max_id.set(id.checked_add(1).expect("out of ids"));
				v.insert(id);
				Ident {
					kind,
					id,
					_marker: PhantomData,
				}
			}
		}
	}
}

pub trait Kind {
	fn id() -> u8;
}

thread_local! {
	static ALLOCATOR: (Cell<bool>, CodeIdentAllocator) = (Cell::new(false), CodeIdentAllocator::default());
}
pub fn in_allocator<T>(f: impl FnOnce() -> T) -> T {
	ALLOCATOR.with(|a| {
		assert!(!a.0.get(), "already in allocator");
		a.0.set(true);
	});
	let v = f();
	ALLOCATOR.with(|a| {
		assert!(a.0.get(), "should be in allocator");
		a.0.set(false);
		a.1.next_generation();
	});
	v
}

pub struct Ident<K> {
	kind: u8,
	id: u16,
	_marker: PhantomData<fn() -> K>,
}
impl<K: Kind> Ident<K> {
	pub fn alloc(name: &str) -> Self {
		ALLOCATOR.with(|a| {
			assert!(a.0.get(), "should be in allocator");
			a.1.ident(name)
		})
	}
}
impl<K> Clone for Ident<K> {
	fn clone(&self) -> Self {
		Self {
			kind: self.kind,
			id: self.id,
			_marker: PhantomData,
		}
	}
}
impl<K> Copy for Ident<K> {}
impl<K> PartialEq for Ident<K> {
	fn eq(&self, other: &Ident<K>) -> bool {
		assert_eq!(
			self.kind, other.kind,
			"comparing idents of a different generations"
		);
		self.id == other.id
	}
}
impl<K> Eq for Ident<K> {}
impl<K> Debug for Ident<K> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let n = ALLOCATOR.with(|a| a.1.name(self.id));
		write!(f, "{n}")
	}
}

pub struct DbIdent<K> {
	id: String,
	_marker: PhantomData<K>,
}
impl<K> DbIdent<K> {
	pub fn new(v: &str) -> Self {
		Self {
			id: v.to_owned(),
			_marker: PhantomData,
		}
	}
}
impl<K: Kind> PartialEq for DbIdent<K> {
	fn eq(&self, other: &Self) -> bool {
		self.id == other.id
	}
}
impl<K> Display for DbIdent<K> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.id)
	}
}
impl<K> Debug for DbIdent<K> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}", self.id)
	}
}
impl<K> Clone for DbIdent<K> {
	fn clone(&self) -> Self {
		Self {
			id: self.id.clone(),
			_marker: PhantomData,
		}
	}
}
