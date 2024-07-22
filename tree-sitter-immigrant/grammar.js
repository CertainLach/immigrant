module.exports = grammar({
	name: 'immigrant',
	fileTypes: ['schema'],
	scope: 'immigrant',
	rules: {
		source_file: $ => choice($.test_schema, $.normal_schema),

		test_schema: $ => seq(
			optional(field('setup', seq(
				'!!!SETUP',
				$.test_explaination,
				$.normal_schema,
			))),
			'!!!TEST',
			$.test_explaination,
			field('first_example', $.normal_schema),
			optional(field('examples', seq(
				'!!!UPDATE',
				$.test_explaination,
				$.normal_schema,
			))),
			optional(field('result', $.sql_block)),
		),
		sql_block: $ => seq('!!!RESULT', repeat(/.+/)),

		test_explaination: $ => /[^\n]*\n/,

		normal_schema: $ => repeat1($.declaration),
		declaration: $ => choice(
			$.scalar_declaration,
			$.table_declaration,
			$.view_declaration,
			$.enum_declaration,
			$.composite_declaration,
		),
		scalar_declaration: $ => seq(
			repeat($.attribute),
			'scalar',
			field('name', $.type_identifier),
			'=',
			$.string,
			optional($.field_annotation),
			';',
		),
		table_declaration: $ => seq(
			repeat($.attribute),
			'table',
			field('name', $.type_db_name),
			'{',
			repeat1($.table_field),
			repeat($.table_annotation),
			'}',
			';',
		),
		decl: $ => repeat1(choice(
			$.decl_column,
			$.decl_table,
			$.decl_raw,
		)),
		decl_raw: $ => prec.right(repeat1(choice(
			/[^\$]+/,
			/\$[^\$\{]/
		))),
		decl_column: $ => seq(
			'${',
			field('table', $.type_identifier),
			'.',
			field('column', $.field_identifier),
			'}',
		),
		decl_table: $ => seq(
			'${',
			field('table', $.type_identifier),
			'}',
		),
		view_declaration: $ => seq(
			repeat($.attribute),
			'view',
			field('name', $.type_db_name),
			'=',
			'$$',
			$.decl,
			'$$',
			';',
		),
		enum_declaration: $ => seq(
			repeat($.attribute),
			'enum',
			field('name', $.type_db_name),
			'{',
			repeat($.enum_field),
			'}',
			';',
		),
		composite_declaration: $ => seq(
			repeat($.attribute),
			'struct',
			field('name', $.type_db_name),
			'{',
			repeat($.composite_field),
			'}',
			';',
		),

		enum_field: $ => seq(
			field('name', $.variant_db_name),
			';'
		),
		composite_field: $ => seq(
			field('name', $.field_identifier),
				':',
				$.type_identifier,
			';'
		),
		table_field: $ => seq(
			repeat($.attribute),
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
		attribute_identifier: $ => $.identifier,
		attribute_field_identifier: $ => $.identifier,
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

		attribute: $ => seq(
			'#',
			$.attribute_identifier,
			optional(seq(
				'(',
				repeat1($.attribute_field),
				')',
			)),
		),
		attribute_field: $ => seq(
			$.attribute_field_identifier,
			optional(seq(
				'=',
				$.string,
			)),
			optional(','),
		),

		field_annotation: $ => repeat1(choice(
			'@external',
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
			choice('@default', '@check', '@initialize_as'),
			'(',
			$.expression,
			')',
		),
		pk_annotation: $ => seq(
			choice('@primary_key', '@unique', '@index', '@inline'),
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
			$.unary_expression,
			$.parened_expression,
			$.field_identifier,
			$.string,
			$.number,
		),
		function_call: $ => prec.left(seq(
			$.function_identifier,
			'(',
			optional(seq(
				repeat(seq($.expression, ',')),
				$.expression, optional(','),
			)),
			')',
		)),
		unary_expression: $ => seq('!', $.expression),
		// Precedence matching is wrong here
		binary_expression: $ => choice(
			prec.left(1, seq($.expression, '||', $.expression)),
			prec.left(2, seq($.expression, '&&', $.expression)),
			prec.left(3, seq($.expression, choice('==', '!=', '~~', "===", "!=="), $.expression)),
			prec.left(4, seq($.expression, '::', $.type_identifier)),
			prec.left(4, seq($.expression, '.', $.field_identifier)),
			prec.left(5, seq($.expression, choice('<', '>', '<=', '>='), $.type_identifier)),
		),
		parened_expression: $ => seq(
			'(',
			$.expression,
			')',
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
