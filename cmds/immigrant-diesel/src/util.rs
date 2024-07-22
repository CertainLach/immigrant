use std::collections::BTreeSet;
use std::mem;

pub fn disjoint_unions<T: Ord + Clone>(input: &[(T, T)]) -> Vec<BTreeSet<T>> {
	let mut out = Vec::new();
	let mut seen = BTreeSet::new();
	let mut current = BTreeSet::new();

	loop {
		loop {
			let mut changed = false;
			for (a, b) in input {
				#[allow(clippy::collapsible_if)]
				if (current.contains(a) || current.contains(b))
					|| (current.is_empty() && !seen.contains(a) && !seen.contains(b))
				{
					if current.insert(a.clone()) || current.insert(b.clone()) {
						changed = true;
					}
				}
			}
			if !changed {
				break;
			}
		}

		if !current.is_empty() {
			seen.extend(current.iter().cloned());
			out.push(mem::take(&mut current));
		} else {
			break;
		}
	}

	out
}

#[test]
fn disjoint_union_works() {
	assert_eq!(
		disjoint_unions(&[(1, 2), (2, 3), (3, 4), (5, 6), (7, 8), (4, 10)]),
		vec![
			[1, 2, 3, 4, 10].into_iter().collect(),
			[5, 6].into_iter().collect(),
			[7, 8].into_iter().collect(),
		]
	);
}
