; Runnables for hop, which put a run button in the gutter beside each test:
; a test fn without parameters. The task tagged hop-test in tasks.json runs
; it, with the name of the test in $GRAM_CUSTOM_name.

((function_declaration
  (test_modifier)
  name: (_) @run @name
  parameters: (parameters) @_parameters)
  (#match? @_parameters "^\\(\\s*\\)$")
  (#set! tag hop-test))
