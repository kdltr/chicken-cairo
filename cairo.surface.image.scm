(module cairo.surface.image
  ()
  (import scheme (chicken base) (chicken foreign))
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

(guarded-defs
 CAIRO_HAS_PNG_FUNCTIONS
 (surface image-surface-create-from-png c-string)
 (status surface-write-to-png! surface c-string))

)
