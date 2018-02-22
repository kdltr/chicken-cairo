(module cairo.surface.image
  ()
  (import scheme chicken lolevel foreign)
  (import-for-syntax srfi-1 srfi-13 data-structures)
  (include "types.scm")
  (foreign-declare "#include \"cairo.h\"")

(defs
  (surface image-surface-create format int int)
  (surface image-surface-create-for-data c-pointer format int int int)
  (c-pointer image-surface-get-data surface)
  (format image-surface-get-format surface)
  (int image-surface-get-width surface)
  (int image-surface-get-height surface)
  (int image-surface-get-stride surface))

)