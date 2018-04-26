(module cairo.surface.image
  ()
  (import scheme chicken lolevel foreign)
  (import-for-syntax srfi-1 srfi-13 data-structures)
  (include "types.scm")
  (foreign-declare "#include \"cairo.h\"")

  ;; cairo does unfortunately not do this for us
  (foreign-declare "#ifndef CAIRO_HAS_PNG_FUNCTIONS")
  (foreign-declare "#define CAIRO_HAS_PNG_FUNCTIONS 0")
  (foreign-declare "#endif")

(defs
  (surface image-surface-create format int int)
  (surface image-surface-create-for-data c-pointer format int int int)
  (c-pointer image-surface-get-data surface)
  (format image-surface-get-format surface)
  (int image-surface-get-width surface)
  (int image-surface-get-height surface)
  (int image-surface-get-stride surface))

(when (foreign-value "CAIRO_HAS_PNG_FUNCTIONS" bool)
  (defs
    (surface image-surface-create-from-png c-string)
    (status surface-write-to-png! surface c-string)))

)
