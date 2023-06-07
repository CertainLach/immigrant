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
"::"] @operator

(number) @number
(string) @string
(type_identifier) @type
(field_identifier) @property
(variant_identifier) @property
(function_identifier) @function
(comment) @comment
