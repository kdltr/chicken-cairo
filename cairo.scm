;; Cairo vector graphics bindings for Chicken Scheme
;
; Copyright (C) 2004, 2005 Michael Bridgen <mikeb@squaremobius.net>,
;                          Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;
; This library is free software; you can redistribute it and/or modify
; it under the terms of the GNU Library General Public License as
; published by the Free Software Foundation; either version 2 of the
; License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Library General Public License for more details.
;
; You should have received a copy of the GNU Library General Public
; License along with this library; if not, write to the Free
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
; USA

; --------------------------------------------------

(module cairo
 *
 (import chicken scheme foreign)

 (use srfi-1)
 (use srfi-4)
 (use srfi-13)
 (use lolevel)

 (import-for-syntax srfi-1)
 (import-for-syntax srfi-13)

 (foreign-declare #<<EOF

#include "cairo.h"
#include "cairo-svg.h"
#include "cairo-pdf.h"
#include "cairo-ps.h"
EOF
)

;; Define a list of DEFINEs or enums at once.
(define-syntax --cairo-flags
  (lambda (e r c)
    (let ((strs (cdr e)))
      `(begin
	 ,@(append-map (lambda (str)
			 (let* ((sym (string->symbol str))
				(psym (string->symbol (string-append "-" (symbol->string sym)))))
			   `((,(r 'define-foreign-variable) ,psym unsigned-integer ,str)
			     (,(r 'define) ,sym ,psym))))
		       strs)))))

(define *cairo-egg-version* '(0 1 15 alpha))

(define cairo-pi (foreign-value "M_PI" float))

;; Functions are mostly in the same order to
;; cairo.h

;; State manipulation
;; -----------------------------------------------

;; cairo_t encapsulates the current state

(define-foreign-type cairo_t (c-pointer "cairo_t")
  values
  (cut tag-pointer <> 'cairo))

; formerly didn't take the surface argument
(define cairo-create (foreign-lambda cairo_t "cairo_create"
                                     cairo_surface_t))

; For reference counting?
(define cairo-reference (foreign-lambda void "cairo_reference" cairo_t))

(define cairo-destroy (foreign-lambda void "cairo_destroy" cairo_t))

; Save the state of this context (and keep it as the state)
(define cairo-save (foreign-lambda void "cairo_save" cairo_t))

; Restore the last saved state of this context
(define cairo-restore (foreign-lambda void "cairo_restore" cairo_t))

; cairo-copy deprecated

; ------------------------------------------------

; Surfaces and backends
; ------------------------------------------------

;; cairo_surface_t is a drawing surface abstraction

(define-foreign-type cairo_surface_t (c-pointer "cairo_surface_t")
  values
  (cut tag-pointer <> 'cairo-surface))

(--cairo-flags "CAIRO_FORMAT_ARGB32"
	       "CAIRO_FORMAT_RGB24"
	       "CAIRO_FORMAT_A8"
	       "CAIRO_FORMAT_A1")

;; %%% TO DO Several surface creation procedures

(define cairo-image-surface-create-for-data (foreign-lambda cairo_surface_t "cairo_image_surface_create_for_data"
                                                            (c-pointer byte)
                                                            integer
                                                            int
                                                            int
                                                            int))

(define cairo-surface-create-for-rectangle (foreign-lambda cairo_surface_t "cairo_surface_create_for_rectangle"
                                                            cairo_surface_t
                                                            double
                                                            double
                                                            double
                                                            double))

(define cairo-svg-surface-create (foreign-lambda cairo_surface_t "cairo_svg_surface_create"
						 c-string
						 double
						 double))

(define cairo-pdf-surface-create (foreign-lambda cairo_surface_t "cairo_pdf_surface_create"
						 c-string
						 double
						 double))

(define cairo-ps-surface-create (foreign-lambda cairo_surface_t "cairo_ps_surface_create"
						c-string
						double
						double))

(define cairo-surface-create-for-rectangle
  (foreign-lambda cairo_surface_t "cairo_surface_create_for_rectangle"
                  cairo_surface_t
                  double
                  double
                  double
                  double))

; cairo-create-image deprecated

; cairo-set-target-image deprecated

(--cairo-flags "CAIRO_OPERATOR_CLEAR"
               
	           "CAIRO_OPERATOR_SOURCE"
	           "CAIRO_OPERATOR_OVER"
	           "CAIRO_OPERATOR_IN"
	           "CAIRO_OPERATOR_OUT"
	           "CAIRO_OPERATOR_ATOP"

               "CAIRO_OPERATOR_DEST"
               "CAIRO_OPERATOR_DEST_OVER"
               "CAIRO_OPERATOR_DEST_IN"
               "CAIRO_OPERATOR_DEST_OUT"
               "CAIRO_OPERATOR_DEST_ATOP"
               
	           "CAIRO_OPERATOR_XOR"
	           "CAIRO_OPERATOR_ADD"
	           "CAIRO_OPERATOR_SATURATE"

               "CAIRO_OPERATOR_MULTIPLY"
               "CAIRO_OPERATOR_SCREEN"
               "CAIRO_OPERATOR_OVERLAY"
               "CAIRO_OPERATOR_DARKEN"
               "CAIRO_OPERATOR_LIGHTEN"
               "CAIRO_OPERATOR_COLOR_DODGE"
               "CAIRO_OPERATOR_COLOR_BURN"
               "CAIRO_OPERATOR_HARD_LIGHT"
               "CAIRO_OPERATOR_SOFT_LIGHT"
               "CAIRO_OPERATOR_DIFFERENCE"
               "CAIRO_OPERATOR_EXCLUSION"
               "CAIRO_OPERATOR_HSL_HUE"
               "CAIRO_OPERATOR_HSL_SATURATION"
               "CAIRO_OPERATOR_HSL_COLOR"
               "CAIRO_OPERATOR_HSL_LUMINOSITY")

; Set the composition operator --
; see cairo docs or otherwise what they do.
; http://cairographics.org/samples/snippets.html has
; pictures of the results.
(define cairo-set-operator (foreign-lambda void "cairo_set_operator" cairo_t integer))

; formerly cairo-set-rgb-color
(define cairo-set-source-rgb (foreign-lambda void "cairo_set_source_rgb"
					    cairo_t
					    double ;red
					    double ;green
					    double ;blue
					    ))

;; Also see cairo-paint-with-alpha
(define cairo-set-source-rgba (foreign-lambda void "cairo_set_source_rgba"
					    cairo_t
					    double ;red
					    double ;green
					    double ;blue
                                            double ;alpha
					    ))

(define cairo-set-source-surface
  (foreign-lambda void "cairo_set_source_surface"
                  cairo_t
                  cairo_surface_t
                  double ;x
                  double ;y
                  ))

; ?
(define cairo-set-tolerance (foreign-lambda void "cairo_set_tolerance" cairo_t double))
;; -----------------------------------------------
;;
;; Antialias and fill
;; -----------------------------------------------

(--cairo-flags "CAIRO_ANTIALIAS_DEFAULT"
               "CAIRO_ANTIALIAS_NONE"
               "CAIRO_ANTIALIAS_GRAY"
               "CAIRO_ANTIALIAS_SUBPIXEL")

(define cairo-set-antialias (foreign-lambda void "cairo_set_antialias" cairo_t integer))

(--cairo-flags "CAIRO_FILL_RULE_WINDING"
	       "CAIRO_FILL_RULE_EVEN_ODD")

; Set the fill rule:
; 'winding' is like a union (anything inside a path)
; 'even-odd' is like xor (anything inside an odd number of paths)
(define cairo-set-fill-rule (foreign-lambda void "cairo_set_fill_rule" cairo_t integer))

; ------------------------------------------------

; Lines, joins, and caps
; ------------------------------------------------

(define cairo-set-line-width (foreign-lambda void "cairo_set_line_width" cairo_t double))

(--cairo-flags "CAIRO_LINE_CAP_BUTT"
               "CAIRO_LINE_CAP_ROUND"
               "CAIRO_LINE_CAP_SQUARE")

(define cairo-set-line-cap (foreign-lambda void "cairo_set_line_cap" cairo_t integer))

(--cairo-flags "CAIRO_LINE_JOIN_MITER"
               "CAIRO_LINE_JOIN_ROUND"
               "CAIRO_LINE_JOIN_BEVEL")

(define cairo-set-line-join (foreign-lambda void "cairo_set_line_join" cairo_t integer))

(define cairo-set-dash (foreign-lambda void "cairo_set_dash" cairo_t f64vector integer double))

(define cairo-set-miter-limit (foreign-lambda void "cairo_set_miter_limit" cairo_t double))

; ------------------------------------------------

; Context transform operations
; ------------------------------------------------

;; These following operate on the current transform of
;; the context -- there are also operations on arbitrary
;; transforms further on (but perhaps not yet)

(define cairo-translate (foreign-lambda void "cairo_translate" cairo_t double double))

(define cairo-scale (foreign-lambda void "cairo_scale" cairo_t double double))

(define cairo-rotate (foreign-lambda void "cairo_rotate" cairo_t double))

(define cairo-transform (foreign-lambda void "cairo_transform" cairo_t cairo_matrix_t))

(define cairo-set-matrix (foreign-lambda void "cairo_set_matrix" cairo_t cairo_matrix_t))

(define cairo-identity-matrix (foreign-lambda void "cairo_identity_matrix" cairo_t))

;; Matrix operations

(define-foreign-type cairo_matrix_t (c-pointer "cairo_matrix_t")
  values
  (cut tag-pointer <> 'cairo-matrix))

(define %fill-matrix
  (foreign-lambda* (c-pointer cairo_matrix_t)
                   ((f64vector v))
                   "cairo_matrix_t *m = malloc(sizeof(cairo_matrix_t));
                    int i=0;
                    m->xx = v[i++];
                    m->xy = v[i++];
                    m->yx = v[i++];
                    m->yx = v[i++];
                    m->x0 = v[i++];
                    m->y0 = v[i++];
                    C_return(m);"))

(define (cairo-transform/matrix ctx v)
  (when (not (= (f64vector-length v) 6))
    (error "Vector size mismatch should be 6, is " (f64vector-length v)))
  (let ((m (%fill-matrix v)))
    (cairo-transform ctx m)
    (free m)))

(define cairo-new-matrix
  (foreign-lambda* cairo_matrix_t ()
    "C_return(malloc(sizeof(cairo_matrix_t)));"))

; accessors for each element might be nice, but probably not
; immediately necessary

; cairo-concat-matrix replaced by cairo-transform

; cairo-matrix-set replaced by cairo-set-matrix

; was cairo-transform-point
(define cairo-user-to-device (foreign-lambda void "cairo_user_to_device" cairo_t f64vector f64vector))

; was cairo-transform-distance
(define cairo-user-to-device-distance (foreign-lambda void "cairo_user_to_device" cairo_t f64vector f64vector))

; was cairo-inverse-transform-point
(define cairo-device-to-user (foreign-lambda void "cairo_device_to_user" cairo_t f64vector f64vector))

; was cairo-inverse-transform-distance
(define cairo-device-to-user-distance (foreign-lambda void "cairo_device_to_user_distance" cairo_t f64vector f64vector))

; ------------------------------------------------

; Path creation
; ------------------------------------------------

;; Note that actually drawing paths is destructive;
;; it will erase the current path.  Use cairo_save
;; and cairo_restore if you want to use a path for
;; more than one drawing operation.  Clipping is
;; _not_ destructive.

(define cairo-new-path (foreign-lambda void "cairo_new_path" cairo_t))

(define cairo-move-to (foreign-lambda void "cairo_move_to" cairo_t double double))

(define cairo-line-to (foreign-lambda void "cairo_line_to" cairo_t double double))

(define cairo-curve-to (foreign-lambda void "cairo_curve_to" cairo_t double double double double double double))

(define cairo-arc (foreign-lambda void "cairo_arc" cairo_t double double double double double))

(define cairo-arc-negative (foreign-lambda void "cairo_arc_negative" cairo_t double double double double double))

;; there's some relative move_to etc. things here .. mneh

(define cairo-rectangle (foreign-lambda void "cairo_rectangle" cairo_t double double double double))

;; Finish the current shape so you can draw another disjoint one
(define cairo-close-path (foreign-lambda void "cairo_close_path" cairo_t))

; ------------------------------------------------

; Drawing operations
; ------------------------------------------------

(define cairo-stroke (foreign-lambda void "cairo_stroke" cairo_t))
(define cairo-stroke-preserve (foreign-lambda void "cairo_stroke_preserve" cairo_t))

(define cairo-fill (foreign-lambda void "cairo_fill" cairo_t))
(define cairo-fill-preserve (foreign-lambda void "cairo_fill_preserve" cairo_t))

; ------------------------------------------------

; Insideness testing ... mneh
; ------------------------------------------------

(define-foreign-variable sizeof-cairo-text-extents int "sizeof(cairo_text_extents_t)")

;; We want a garbage-collectable type here; wrap a record type around a byte vector buffer, and make
;; chicken type-pun the buffer to the C struct.  We only ever construct these to hand to a function
;; for filling in, so no initialisation needed aside from the buffer.
(define-record cairo-text-extents-type buffer)
(let ((maker make-cairo-text-extents-type))
  (set! make-cairo-text-extents-type
    (lambda () (maker (make-blob sizeof-cairo-text-extents)))))

(define-record-printer (cairo-text-extents-type te out)
  (for-each (lambda (x) (display x out))
                    (list "#<cairo-text-extents "
                          (cairo-text-extents-x-bearing te)" "
                          (cairo-text-extents-y-bearing te)" "
                          (cairo-text-extents-width te)" "
                          (cairo-text-extents-height te)" "
                          (cairo-text-extents-x-advance te)" "
                          (cairo-text-extents-y-advance te)">")))

(define-foreign-type cairo_text_extents_t scheme-pointer cairo-text-extents-type-buffer)

(define cairo-text-extents-x-bearing (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->x_bearing);"))
(define cairo-text-extents-y-bearing (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->y_bearing);"))
(define cairo-text-extents-width (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->width);"))
(define cairo-text-extents-height (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->height);"))
(define cairo-text-extents-x-advance (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->x_advance);"))
(define cairo-text-extents-y-advance (foreign-lambda* double ((cairo_text_extents_t te)) "C_return(((cairo_text_extents_t*)te)->y_advance);"))

(define cairo-text-extents-x-bearing-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->x_bearing = v;"))
(define cairo-text-extents-y-bearing-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->y_bearing = v;"))
(define cairo-text-extents-width-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->width = v;"))
(define cairo-text-extents-height-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->height = v;"))
(define cairo-text-extents-x-advance-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->x_advance = v;"))
(define cairo-text-extents-y-advance-set! (foreign-lambda* double ((cairo_text_extents_t te) (double v)) "((cairo_text_extents_t*)te)->y_advance = v;"))

(define-foreign-variable sizeof-cairo-font-extents int "sizeof(cairo_font_extents_t)")

(define-record cairo-font-extents-type buffer)
(let ((maker make-cairo-font-extents-type))
  (set! make-cairo-font-extents-type
    (lambda () (maker (make-blob sizeof-cairo-font-extents)))))

(define-record-printer (cairo-font-extents-type e out)
  (for-each (lambda (x) (display x out))
                    (list "#<cairo-font-extents "
                          (cairo-font-extents-ascent e)" "
                          (cairo-font-extents-descent e)" "
                          (cairo-font-extents-height e)" "
                          (cairo-font-extents-max-x-advance e)" "
                          (cairo-font-extents-max-y-advance e)">")))

(define-foreign-type cairo_font_extents_t scheme-pointer cairo-font-extents-type-buffer)

(define cairo-font-extents-ascent (foreign-lambda* double ((cairo_font_extents_t e)) "C_return(((cairo_font_extents_t*)e)->ascent);"))
(define cairo-font-extents-descent (foreign-lambda* double ((cairo_font_extents_t e)) "C_return(((cairo_font_extents_t*)e)->descent);"))
(define cairo-font-extents-height (foreign-lambda* double ((cairo_font_extents_t e)) "C_return(((cairo_font_extents_t*)e)->height);"))
(define cairo-font-extents-max-x-advance (foreign-lambda* double ((cairo_font_extents_t e)) "C_return(((cairo_font_extents_t*)e)->max_x_advance);"))
(define cairo-font-extents-max-y-advance (foreign-lambda* double ((cairo_font_extents_t e)) "C_return(((cairo_font_extents_t*)e)->max_y_advance);"))

(define cairo-font-extents-ascent-set! (foreign-lambda* double ((cairo_font_extents_t e) (double v)) "((cairo_font_extents_t*)e)->ascent = v;"))
(define cairo-font-extents-descent-set! (foreign-lambda* double ((cairo_font_extents_t e) (double v)) "((cairo_font_extents_t*)e)->descent = v;"))
(define cairo-font-extents-height-set! (foreign-lambda* double ((cairo_font_extents_t e) (double v)) "((cairo_font_extents_t*)e)->height = v;"))
(define cairo-font-extents-max-x-advance-set! (foreign-lambda* double ((cairo_font_extents_t e) (double v)) "((cairo_font_extents_t*)e)->max_x_advance = v;"))
(define cairo-font-extents-max-y-advance-set! (foreign-lambda* double ((cairo_font_extents_t e) (double v)) "((cairo_font_extents_t*)e)->max_y_advance = v;"))

(define cairo-text-extents (foreign-lambda void "cairo_text_extents" cairo_t c-string cairo_text_extents_t))

; was cairo-current-font-extents
(define cairo-font-extents (foreign-lambda void "cairo_font_extents" cairo_t cairo_font_extents_t))

; Clipping
; ------------------------------------------------

;; Clipping does reset the path, unless you use the
;; -preserve functions

; was cairo-init-clip
(define cairo-reset-clip (foreign-lambda void "cairo_reset_clip" cairo_t))

;; Intersect the current path with the clipping region
;; for the new clipping region
(define cairo-clip (foreign-lambda void "cairo_clip" cairo_t))

(define cairo-clip-extents (foreign-lambda void "cairo_clip_extents"
                                           cairo_t
                                           (c-pointer double)
                                           (c-pointer double)
                                           (c-pointer double)
                                           (c-pointer double)))

; ------------------------------------------------
;
; Fonts and text
; ------------------------------------------------

(--cairo-flags
; font weight
 "CAIRO_FONT_WEIGHT_NORMAL"
 "CAIRO_FONT_WEIGHT_BOLD"
 ; and slant
 "CAIRO_FONT_SLANT_NORMAL"
 "CAIRO_FONT_SLANT_ITALIC"
 "CAIRO_FONT_SLANT_OBLIQUE")

; An easy-osy function for choosing a font to use
; (as usual, affects the context)
; was cairo-select-font
(define cairo-select-font-face
  (foreign-lambda void "cairo_select_font_face"
                  cairo_t
                  c-string
                  integer ; slant
                  integer ; weight
                  ))

;; NB regarding font transforms:
;; cairo_gstate.c has notes regarding font co-ordinate
;; systems.  I should read them ;-)

; was cairo-scale-font
(define cairo-set-font-size
  (foreign-lambda void "cairo_set_font_size" cairo_t double))

; was cairo-transform-font
(define cairo-set-font-matrix
  (foreign-lambda void "cairo_set_font_matrix" cairo_t cairo_matrix_t))

(define cairo-show-text
  (foreign-lambda void "cairo_show_text" cairo_t c-string))

;; There's also cairo_show_glyphs(), which might be useful
;; if it's worthwhile caching glyphs (I'll look at the cairo
;; source -- it may well convert text to glyphs every time)

(define-foreign-type cairo_font_t (c-pointer "cairo_font_t")
  values
  (cut tag-pointer <> 'cairo-font))

;; Here would be the text extent stuff, certainly useful

;; .. and here, converting fonts to paths

;; .. and some functions for transforming fonts directly

; ------------------------------------------------
;
; Context status and query
; ------------------------------------------------

; All these used to be -current-, now -get-

(define cairo-get-target
  (foreign-lambda cairo_surface_t "cairo_get_target" cairo_t))

(define cairo-get-operator
  (foreign-lambda integer "cairo_get_operator" cairo_t))

; cairo-current-rgb-color deprecated

; cairo-current-alpha deprecated

(define cairo-get-matrix
  (foreign-lambda void "cairo_get_matrix" cairo_t cairo_matrix_t))

;; and some more skipped

;; cairo_status_t
(--cairo-flags
  "CAIRO_STATUS_SUCCESS"
  "CAIRO_STATUS_NO_MEMORY"
  "CAIRO_STATUS_INVALID_RESTORE"
  "CAIRO_STATUS_INVALID_POP_GROUP"
  "CAIRO_STATUS_NO_CURRENT_POINT"
  "CAIRO_STATUS_INVALID_MATRIX"
  "CAIRO_STATUS_INVALID_STATUS"
  "CAIRO_STATUS_NULL_POINTER"
  "CAIRO_STATUS_INVALID_STRING"
  "CAIRO_STATUS_INVALID_PATH_DATA"
  "CAIRO_STATUS_READ_ERROR"
  "CAIRO_STATUS_WRITE_ERROR"
  "CAIRO_STATUS_SURFACE_FINISHED"
  "CAIRO_STATUS_SURFACE_TYPE_MISMATCH"
  "CAIRO_STATUS_PATTERN_TYPE_MISMATCH"
  "CAIRO_STATUS_INVALID_CONTENT"
  "CAIRO_STATUS_INVALID_FORMAT"
  "CAIRO_STATUS_INVALID_VISUAL"
  "CAIRO_STATUS_FILE_NOT_FOUND"
  "CAIRO_STATUS_INVALID_DASH")



; ------------------------------------------------

; Matrix operations
; ------------------------------------------------

; (cairo_matrix_t defined earlier)

; NB: These may be renamed to cairo_transform_* in future
; versions of Cairo.

(define cairo-matrix-init-identity
  (foreign-lambda void
                  "cairo_matrix_init_identity"
                  cairo_matrix_t))

(define cairo-matrix-init
  (foreign-lambda void
                  "cairo_matrix_init"
                  cairo_matrix_t
                  double double ;; a, b
                  double double ;; c, d
                  double double ;; tx, ty
                  ))

(define cairo-matrix-translate
  (foreign-lambda void
                  "cairo_matrix_translate"
                  cairo_matrix_t
                  double double ;; x, y
                  ))

(define cairo-matrix-scale
  (foreign-lambda void
                  "cairo_matrix_scale"
                  cairo_matrix_t
                  double double ;; x, y
                  ))

(define cairo-matrix-rotate
  (foreign-lambda void
                  "cairo_matrix_rotate"
                  cairo_matrix_t
                  double ;; radians
                  ))

(define cairo-matrix-invert
  (foreign-lambda integer ;; cairo_status_t
                  "cairo_matrix_invert"
                  cairo_matrix_t))

(define cairo-matrix-multiply
  (foreign-lambda void
                  "cairo_matrix_multiply"
                  cairo_matrix_t ;; dest
                  cairo_matrix_t
                  cairo_matrix_t))

(define cairo-matrix-transform-distance
  (foreign-lambda void
                  "cairo_matrix_transform_distance"
                  cairo_matrix_t
                  f64vector f64vector))

(define cairo-matrix-transform-point
  (foreign-lambda void
                  "cairo_matrix_transform_point"
                  cairo_matrix_t
                  f64vector f64vector))

; PNG Support
(define cairo-image-surface-create-from-png (foreign-lambda cairo_surface_t
                                                            "cairo_image_surface_create_from_png"
                                                            c-string
                                                            ))

(define cairo-surface-write-to-png (foreign-lambda integer
                                                   "cairo_surface_write_to_png"
                                                   cairo_surface_t
                                                   c-string
                                                   ))

; Auxiliary
(define cairo-image-surface-create (foreign-lambda cairo_surface_t
                                                   "cairo_image_surface_create"
                                                   integer
                                                   integer
                                                   integer
                                     ))

(define cairo-image-surface-get-width (foreign-lambda int
                                                      "cairo_image_surface_get_width"
                                                      cairo_surface_t))

(define cairo-image-surface-get-height (foreign-lambda int
                                                       "cairo_image_surface_get_height"
                                                       cairo_surface_t))

(define cairo-mask-surface  (foreign-lambda void
                                            "cairo_mask_surface"
                                            cairo_t
                                            cairo_surface_t
                                            double
                                            double))



(define cairo-paint (foreign-lambda void
                                    "cairo_paint"
                                    cairo_t
                                    ))

(define cairo-paint-with-alpha  (foreign-lambda void
                                                "cairo_paint_with_alpha"
                                                cairo_t
                                                double
                                                ))

(define cairo-surface-destroy (foreign-lambda void
                                              "cairo_surface_destroy"
                                              cairo_surface_t
                                              ))

(define cairo-surface-flush (foreign-lambda void
                                            "cairo_surface_flush"
                                            cairo_surface_t
                                            ))

(define cairo-surface-finish (foreign-lambda void
                                             "cairo_surface_finish"
                                             cairo_surface_t))

(define cairo-surface-show-page (foreign-lambda void
                                                "cairo_surface_show_page"
                                                cairo_surface_t))

(define cairo-mask-surface (foreign-lambda void
                                           "cairo_mask_surface"
                                           cairo_t
                                           cairo_surface_t
                                           double
                                           double)))
