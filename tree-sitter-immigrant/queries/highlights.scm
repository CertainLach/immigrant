["scalar"
"table"
"view"
"enum"
"struct"
"@default"
"@check"
"@initialize_as"
"@primary_key"
"@unique"
"@index"
"@external"
"@inline"
"!!!SETUP"
"!!!TEST"
"!!!UPDATE"
"!!!RESULT"] @keyword
["opclass"
"using"
"with"
"materialized"
(on_update)] @keyword.modifier

[";"
","
"."
":"] @punctuation.delimiter
["{"
"}"
"("
")"
] @punctuation.bracket
["?"
"~"] @punctuation.special
["||"
"&&"
"=="
"==="
"!="
"!=="
"::"
"="
"!"
">"
">="
"<"
"<="] @operator

["#"
] @keyword

(number) @number
(decl (["sql\"\"\""
"sql\""
"\""
"\"\"\""] @string.special))
(string) @string
(string_escape) @string.escape
(type_identifier) @type
(field_identifier) @property
(variant_identifier) @property
(function_identifier) @function
(attribute_identifier) @keyword
(attribute_field_identifier) @property
(comment) @comment
(test_explaination) @comment
