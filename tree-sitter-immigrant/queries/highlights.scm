["scalar"
"table"
"enum"
"struct"
"@default"
"@check"
"@initialize_as"
"@primary_key"
"@unique"
"@index"
"@inline"
"!!!SETUP"
"!!!TEST"
"!!!UPDATE"
"!!!RESULT"
(on_update)] @keyword
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
(test_explaination) @comment
