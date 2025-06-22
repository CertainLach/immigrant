((comment) @injection.content
    (#set! injection.language "comment")
)

((sql_block) @injection.content
    (#set! injection.language "sql")
    ; 9 for !!!RESULT
    (#offset! @injection.content 0 9 0 0)
    (#set! injection.include-children)
 )
(decl_body_multiline ((decl_raw_multiline) @injection.content
 (#set! injection.language "sql")
))
((decl_table) (#set! injection.content "dummy_table"))
(decl_body_single (
    (decl_raw_single) @injection.content
        (#set! injection.language "sql")
        (#gsub! @injection.content "<([^>]+)>" "fake_identifier")
    )
)
