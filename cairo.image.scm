(module cairo.image
  ()
  (import scheme chicken)
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