(table_declaration
  "{" @delimiter
  "}" @delimiter @sentinel) @container

(enum_declaration
  "{" @delimiter
  "}" @delimiter @sentinel) @container

(composite_declaration
  "{" @delimiter
  "}" @delimiter @sentinel) @container

(decl_table
  "${" @delimiter
  "}" @delimiter @sentinel) @container

(decl_column
  "${" @delimiter
  "}" @delimiter @sentinel) @container

