((sql_block) @injection.content
    (#set! injection.language "sql")
    ; 9 for !!!RESULT
    (#offset! @injection.content 0 9 0 0)
    (#set! injection.include-children)
 )
((decl_raw) @injection.content
 (#set! injection.language "sql")
 (#set! injection.combined))
