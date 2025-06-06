use std::collections::HashMap;
use std::hash::Hash;

pub trait UniqueMap<K, V> {
	fn insert_unique(&mut self, key: K, value: V);
}

impl<K, V> UniqueMap<K, V> for HashMap<K, V>
where
	K: Eq + Hash,
{
	fn insert_unique(&mut self, key: K, value: V) {
		self.insert(key, value);
	}
}
