(use lolevel)

;; defs: Defines a bunch of foreign procedures, renaming the symbol to be more scheme-y
(define-for-syntax (struct-name str)
  (string-append "cairo_"
    (string-translate
      (string-trim-right str (lambda (c) (or (eq? c #\!) (eq? c #\?))))
      #\- #\_)))

(define-for-syntax (symbol->enum-ident prefix sym)
  (string-append prefix (string-upcase (string-translate (symbol->string sym) #\- #\_))))

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
                     (foreign-lambda ,return-type ,(struct-name (symbol->string name))
                                     ,@argument-types)))))
            (cdr exp))))))

(define-syntax enum
  (ir-macro-transformer
    (lambda (exn inject compare)
      (let* ((typename (inject (strip-syntax (second exn))))
             (prefix (third exn))
             (symbols (map strip-syntax (cdddr exn)))
             (bindings (map (lambda (s) `(,s (foreign-value ,(symbol->enum-ident prefix s) integer)))
                              symbols))
             (sym->int (map (lambda (s) `(cons ',s ,s)) symbols))
             (int->sym (map (lambda (s) `(cons ,s ',s)) symbols)))
        `(let ,bindings
           (define-foreign-type ,typename integer
             (lambda (sym) (cdr (assq sym (list ,@sym->int))))
             (lambda (int) (cdr (assq int (list ,@int->sym)))))) ))))

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

(enum antialias "CAIRO_ANTIALIAS_"
      default none gray subpixel fast good best)

(enum fill-rule "CAIRO_FILL_RULE_"
      winding even-odd)

(enum line-cap "CAIRO_LINE_CAP_"
      butt round square)

(enum line-join "CAIRO_LINE_JOIN_"
      miter round bevel)

(enum operator "CAIRO_OPERATOR_"
      clear source over in out atop
      dest dest-over dest-in dest-out dest-atop
      xor add saturate multiply screen overlay
      darken lighten color-dodge color-burn
      hard-light soft-light difference exclusion
      hsl-hue hsl-saturation hsl-color hsl-luminosity)

(enum path-data "CAIRO_PATH_"
      move-to line-to curve-to close-path)

(enum extend "CAIRO_EXTEND_"
      none repeat reflect pad)

(enum filter "CAIRO_FILTER_"
      fast good best nearest bilinear gaussian)

(enum pattern-type "CAIRO_PATTERN_TYPE_"
      solid surface linear radial mesh raster-source)

(enum region-overlap "CAIRO_REGION_OVERLAP_"
      in out part)

(enum font-slant "CAIRO_FONT_SLANT_"
      normal italic oblique)

(enum font-weight "CAIRO_FONT_WEIGHT_"
      normal bold)

(enum text-cluster-flags "CAIRO_TEXT_CLUSTER_FLAG_"
      backward)

(enum font-type "CAIRO_FONT_TYPE_"
      toy ft win32 quartz user)

(enum subpixel-order "CAIRO_SUBPIXEL_ORDER_"
      default rgb bgr vrgb vbgr)

(enum hint-style "CAIRO_HINT_STYLE_"
      default none slight medium full)

(enum hint-metrics "CAIRO_HINT_METRICS_"
      default off on)

(enum device-type "CAIRO_DEVICE_TYPE_"
      drm gl script xcb xlib xml cogl win32 invalid)

(enum content "CAIRO_CONTENT_"
      color alpha color-alpha)

(enum surface-type "CAIRO_SURFACE_TYPE_"
      image pdf ps xlib xcb glitz quartz win32 beos
      directfb svg os2 win32-printing quartz-image
      script qt recording vg gl drm tee xml #;sika
      subsurface cogl)

(enum format "CAIRO_FORMAT_"
      invalid argb32 rgb24 a8 a1 rgb16-565 rgb30)


;; TODO move these into backend specific modules
#;(enum pdf-version "CAIRO_PDF_VERSION_"
      1-4 1-5)

#;(enum ps-level "CAIRO_PS_LEVEL"
      2 3)

#;(enum svg-version "CAIRO_SVG_VERSION_"
      1-1 1-2)

#;(enum script-mode "CAIRO_SCRIPT_MODE_"
      ascii binary)

(enum status "CAIRO_STATUS_"
      success no-memory invalid-restore invalid-pop-group
      no-current-point invalid-matrix invalid-status
      null-pointer invalid-string invalid-path-data
      read-error write-error surface-finished
      surface-type-mismatch pattern-type-mismatch
      invalid-content invalid-format invalid-visual
      file-not-found invalid-dash invalid-dsc-comment
      invalid-index clip-not-representable temp-file-error
      invalid-stride font-type-mismatch user-font-immutable
      user-font-error negative-count invalid-clusters
      invalid-slant invalid-weight invalid-size
      user-font-not-implemented device-type-mismatch
      device-error invalid-mesh-construction
      device-finished jbig2-global-missing)


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

(define-foreign-type font-face (c-pointer "cairo_font_face_t")
  (check-pointer 'cairo:font-face) (add-tag 'cairo:font-face))

(define-foreign-type device (c-pointer "cairo_device_t")
  (check-pointer 'cairo:device) (add-tag 'cairo:device))

(define-foreign-type surface (c-pointer "cairo_surface_t")
  (check-pointer 'cairo:surface) (add-tag 'cairo:surface))

(define-foreign-type path (c-pointer "cairo_path_t")
  (check-pointer 'cairo:path) (add-tag 'cairo:path))


;; Open data types
;; -----------------------------------------------

(define-foreign-type matrix nonnull-f64vector)
