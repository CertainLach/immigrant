["scalar"
"table"
"enum"
"@default"
"@check"
"@primary_key"
"@unique"
"@index"
(on_update)] @keyword
[";"
","
"."] @punctuation.delimiter
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
"::"
"="
"!"] @operator

["#"
] @keyword

(number) @number
(string) @string
(type_identifier) @type
(field_identifier) @property
(variant_identifier) @property
(function_identifier) @function
(attribute_identifier) @keyword
(attribute_field_identifier) @property
(comment) @comment
