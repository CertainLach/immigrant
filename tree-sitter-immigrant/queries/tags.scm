(table_declaration
	name: (type_db_name ((type_identifier) @name))
) @definition.type
(enum_declaration
	name: (type_db_name ((type_identifier) @name))
) @definition.type
(scalar_declaration
	name: ((type_identifier) @name)
) @definition.type

(table_declaration
	name: (type_db_name ((type_identifier) @name))
	(table_field
		name: ((field_identifier) @name)
	)
) @definition.property
(enum_declaration
	name: (type_db_name ((type_identifier) @name))
	(enum_field
		name: (variant_db_name ((variant_identifier) @name))
	)
) @definition.property

; column has name, but no type => imlicit type == column name
(table_declaration
	(table_field 
		!type
		name: ((field_identifier) @name)
	)
) @reference.class
(table_declaration
	(table_field 
		type: ((type_identifier) @name)
	)
) @reference.class
