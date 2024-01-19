use crate::{
	attribute::AttributeList,
	def_name_impls,
	names::{ColumnIdent, TableIdent, TableKind, ViewDefName, ViewKind},
	uid::{next_uid, Uid},
};

#[derive(Debug)]
pub enum DefinitionPart {
	Raw(String),
	TableRef(TableIdent),
	ColumnRef(TableIdent, ColumnIdent),
}
#[derive(Debug)]
pub struct Definition(pub Vec<DefinitionPart>);

#[derive(Debug)]
pub struct View {
	uid: Uid,
	name: ViewDefName,
	pub docs: Vec<String>,
	pub attrlist: AttributeList,
	pub definition: Definition,
}
def_name_impls!(View, ViewKind);
impl View {
	pub fn new(
		docs: Vec<String>,
		attrlist: AttributeList,
		name: ViewDefName,
		definition: Definition,
	) -> Self {
		Self {
			uid: next_uid(),
			name,
			docs,
			attrlist,
			definition,
		}
	}
}
