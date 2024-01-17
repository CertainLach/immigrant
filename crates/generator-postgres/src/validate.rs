use schema::{
	column::ColumnAnnotation,
	root::Schema,
	scalar::ScalarAnnotation,
	table::TableAnnotation,
	uid::{RenameExt, RenameMap},
};

const MAX_IDENTIFIER_LEN: usize = 63;

fn validate_db<T: RenameExt>(v: &T, rn: &RenameMap) {
	let str = v.db(rn).to_string();
	assert!(
		str.len() <= MAX_IDENTIFIER_LEN,
		"{str} is larger than max allowed identifier! consider renaming"
	);
}

pub fn validate(_code: &str, schema: &Schema, rn: &RenameMap) {
	for ele in schema.items() {
		validate_db(&ele, rn);
		match ele {
			schema::SchemaItem::Table(t) => {
				for ele in t.columns() {
					validate_db(&ele, rn);
					for ele in &ele.annotations {
						match ele {
							ColumnAnnotation::Check(_)
							| ColumnAnnotation::Unique(_)
							| ColumnAnnotation::PrimaryKey(_)
							| ColumnAnnotation::Index(_) => panic!("should be propagated"),
							ColumnAnnotation::Default(_) | ColumnAnnotation::InitializeAs(_) => {}
						}
					}
				}
				for ele in &t.annotations {
					match ele {
						TableAnnotation::Check(c) => validate_db(c, rn),
						TableAnnotation::Unique(u) => validate_db(u, rn),
						TableAnnotation::PrimaryKey(p) => validate_db(p, rn),
						TableAnnotation::Index(i) => validate_db(i, rn),
						TableAnnotation::External => {}
					}
				}
			}
			schema::SchemaItem::Enum(e) => {
				for ele in &e.items {
					validate_db(ele, rn);
				}
			}
			schema::SchemaItem::Scalar(s) => {
				for ele in &s.annotations {
					match ele {
						ScalarAnnotation::Check(c) => validate_db(c, rn),
						ScalarAnnotation::PrimaryKey(_)
						| ScalarAnnotation::Unique(_)
						| ScalarAnnotation::Index(_) => panic!("should be propagated"),
						ScalarAnnotation::Default(_)
						| ScalarAnnotation::Inline
						| ScalarAnnotation::External => {}
					}
				}
			}
			schema::SchemaItem::Composite(c) => {
				for ele in &c.fields {
					validate_db(ele, rn);
				}
			}
		}
	}
}
