(import foreign lolevel)

;; defs: Defines a bunch of foreign procedures, renaming the symbol to be more scheme-y
(define-for-syntax (translate str)
  (string-append "cairo_"
    (string-translate
      (string-trim-right str (lambda (c) (or (eq? c #\!) (eq? c #\?))))
      #\- #\_)))

(define-syntax defs
  (ir-macro-transformer
    (lambda (exp inject compare)
      `(begin
         ,@(map
            (lambda (def)
              (let ((name (strip-syntax (second def)))
                    (return-type (first def))
                    (argument-types (cddr def)))
                `(begin
                   (export ,name)
                   (define ,name
                     (foreign-lambda ,return-type ,(translate (symbol->string name))
                                     ,@argument-types)))))
            (cdr exp))))))

(define ((check-pointer tag) o)
  (if (tagged-pointer? o tag)
      o
      (error (sprintf "Type error: ~A expected, got ~A~%" tag o))))

(define ((add-tag tag) o)
  (tag-pointer o tag))

;; Constants and enums
;; -----------------------------------------------

(export pi)
(define pi (foreign-value "M_PI" float))

(export +format-rgb24+)
(define +format-rgb24+ (foreign-value "CAIRO_FORMAT_RGB24" integer))
(export +antialias-none+ +antialias-fast+ +antialias-good+ +antialias-best+)
(define +antialias-none+ (foreign-value "CAIRO_ANTIALIAS_NONE" integer))
(define +antialias-fast+ (foreign-value "CAIRO_ANTIALIAS_FAST" integer))
(define +antialias-good+ (foreign-value "CAIRO_ANTIALIAS_GOOD" integer))
(define +antialias-best+ (foreign-value "CAIRO_ANTIALIAS_BEST" integer))

(export +extend-none+ +extend-repeat+ +extend-reflect+ +extend-pad+)
(define +extend-none+ (foreign-value "CAIRO_EXTEND_NONE" integer))
(define +extend-repeat+ (foreign-value "CAIRO_EXTEND_REPEAT" integer))
(define +extend-reflect+ (foreign-value "CAIRO_EXTEND_REFLECT" integer))
(define +extend-pad+ (foreign-value "CAIRO_EXTEND_PAD" integer))

;; TODO symbol / list of symbols to enum value conversion
(define-foreign-type status (enum "cairo_status_t"))
(define-foreign-type content integer)
(define-foreign-type antialias integer)
(define-foreign-type fill-rule integer)
(define-foreign-type line-cap integer)
(define-foreign-type line-join integer)
(define-foreign-type operator integer)
(define-foreign-type format integer)
(define-foreign-type surface-type integer)
(define-foreign-type extend integer)


;; Opaque data types
;; -----------------------------------------------

;; TODO cairo_region_t
;; TODO cairo_glyph_t
;; TODO cairo_text_cluster_t

(define-foreign-type context (c-pointer "cairo_t")
  (check-pointer 'cairo:context) (add-tag 'cairo:context))

(define-foreign-type pattern (c-pointer "cairo_pattern_t")
  (check-pointer 'cairo:pattern) (add-tag 'cairo:pattern))

(define-foreign-type font-face (c-pointer "cairo_font_face_t")
  (check-pointer 'cairo:font-face) (add-tag 'cairo:font-face))

(define-foreign-type scaled-font (c-pointer "cairo_scaled_font_t")
  (check-pointer 'cairo:scaled-font) (add-tag 'cairo:scaled-font))

(define-foreign-type font-options (c-pointer "cairo_font_options_t")
  (check-pointer 'cairo:font-options) (add-tag 'cairo:font-options))

(define-foreign-type device (c-pointer "cairo_device_t")
  (check-pointer 'cairo:device) (add-tag 'cairo:device))

(define-foreign-type surface (c-pointer "cairo_surface_t")
  (check-pointer 'cairo:surface) (add-tag 'cairo:surface))

(define-foreign-type path (c-pointer "cairo_path_t")
  (check-pointer 'cairo:path) (add-tag 'cairo:path))


;; Open data types
;; -----------------------------------------------

(define-foreign-type matrix nonnull-f64vector)
