(module cairo.surface.svg
 ()

 (import scheme chicken foreign lolevel)
 (import-for-syntax srfi-1 srfi-13 data-structures)

 (include "types.scm")
 (foreign-declare "#include \"cairo.h\"")
 (foreign-declare "#include \"cairo-svg.h\"")

(defs
  (surface svg-surface-create c-string double double))
)