module.exports = grammar({
	name: 'immigrant',
	fileTypes: ['schema'],
	scope: 'immigrant',
	rules: {
		source_file: $ => repeat($.declaration),
		declaration: $ => choice(
			$.scalar_declaration,
			$.table_declaration,
			$.enum_declaration,
		),
		scalar_declaration: $ => seq(
			'scalar',
			field('name', $.type_identifier),
			$.string,
			optional($.field_annotation),
			';',
		),
		table_declaration: $ => seq(
			'table',
			field('name', $.type_db_name),
			'{',
			repeat1($.table_field),
			repeat($.table_annotation),
			'}',
			';',
		),
		enum_declaration: $ => seq(
			'enum',
			field('name', $.type_db_name),
			'{',
			repeat($.enum_field),
			'}',
			';',
		),

		enum_field: $ => seq(
			field('name', $.variant_db_name),
			';'
		),
		table_field: $ => seq(
			field('name', $.field_identifier),
			optional(field('type', seq(
				':',
				$.type_identifier,
			))),
			optional('?'),
			optional($.field_annotation),
			';',
		),

		type_identifier: $ => $.identifier,
		field_identifier: $ => $.identifier,
		variant_identifier: $ => $.identifier,
		function_identifier: $ => $.identifier,
		annotation_identifier: $ => $.identifier,
		identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,
		string: $ => /"[^"]*"/,
		number: $ => /[0-9]+/,

		type_db_name: $ => seq(
			$.type_identifier,
			optional($.string),
		),
		variant_db_name: $ => seq(
			$.variant_identifier,
			optional($.string),
		),
		db_name: $ => seq(
			$.identifier,
			optional($.string),
		),

		field_annotation: $ => repeat1(choice(
			$.expr_annotation,
			$.fk_annotation,
			seq($.pk_annotation, optional($.field_list)),
		)),
		table_annotation: $ => seq(choice(
			$.expr_annotation,
			seq(optional($.field_list), $.fk_annotation),
			seq($.pk_annotation, $.field_list),
		), ';'),
		fk_annotation: $ => seq(
			'~',
			optional(seq(
				'.',
				$.on_update,
			)),
			$.type_identifier,
			optional($.field_list),
		),
		on_update: $ => choice('set_null', 'set_default', 'restrict', 'noop', 'cascade'),
		expr_annotation: $ => seq(
			choice('@default', '@check'),
			'(',
			$.expression,
			')',
		),
		pk_annotation: $ => seq(
			choice('@primary_key', '@unique', '@index'),
		),
		field_list: $ => seq(
			'(',
			repeat(seq(
				$.field_identifier,
				','
			)),
			')',
		),
		expression: $ => choice(
			$.function_call,
			$.binary_expression,
			$.field_identifier,
			$.string,
			$.number,
		),
		function_call: $ => prec.left(seq(
			$.function_identifier,
			'(',
			repeat(seq(
				$.expression,
				optional(','),
			)),
			')',
		)),
		binary_expression: $ => choice(
			prec.left(1, seq($.expression, '||', $.expression)),
			prec.left(2, seq($.expression, '&&', $.expression)),
			prec.left(3, seq($.expression, '==', $.expression)),
			prec.left(4, seq($.expression, '::', $.type_identifier)),
		),
		comment: $ =>
			/\/\/[^\n]*\n/,
		ws: $ => /[ \t\n]+/,
	},
	conflicts: $ => [
		[$.function_call, $.expression],
	],
	extras: $=>[$.comment, $.ws],
});
