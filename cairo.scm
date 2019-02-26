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

(module cairo ()


(import chicken scheme foreign lolevel srfi-1 srfi-4)

 (import-for-syntax scheme srfi-1 srfi-13 srfi-14 data-structures)

 (foreign-declare "#include \"cairo.h\"")

(include "types.scm")


;; Context procedures
;; -----------------------------------------------

(defs
  (context create surface)
  (void destroy! context)
  (status status context)
  (void save! context)
  (void restore! context)
  (surface get-target context)
  (void push-group! context)
  (void push-group-with-content! context content)
  (pattern pop-group! context)
  (void pop-group-to-source! context)
  (surface get-group-target context)
  (void set-source-rgb! context double double double)
  (void set-source-rgba! context double double double double)
  (void set-source! context pattern)
  (void set-source-surface! context surface double double)
  (pattern get-source context)
  (void set-antialias! context antialias)
  (antialias get-antialias context)
  (void set-dash! context f64vector int double) ;; TODO better
  (int get-dash-count context)
  #; (void get-dash context f64vector (c-pointer double)) ;; TODO multiple return values
  (void set-fill-rule! context fill-rule)
  (fill-rule get-fill-rule context)
  (void set-line-cap! context line-cap)
  (line-cap get-line-cap context)
  (void set-line-join! context line-join)
  (line-join get-line-join context)
  (void set-line-width! context double)
  (double get-line-width context)
  (void set-miter-limit! context double)
  (double get-miter-limit context)
  (void set-operator! context operator)
  (operator get-operator context)
  (void set-tolerance! context double)
  (double get-tolerance context)
  (void clip! context)
  (void clip-preserve! context)
  #;(void clip-extents context double double double double) ;; TODO multiple return values
  (bool in-clip? context double double)
  (void reset-clip! context)
  #;(void rectangle-list-destroy! rectangle-list) ;; TODO rectangle-list
  #;(rectangle-list copy-clip-rectangle-list context) ;; TODO rectangle-list
  (void fill! context)
  (void fill-preserve! context)
  #;(void fill-extents context double double double double) ;; TODO multiple return values
  (bool in-fill? context double double)
  (void mask! context pattern)
  (void mask-surface! context surface double double)
  (void paint! context)
  (void paint-with-alpha! context double)
  (void stroke! context)
  (void stroke-preserve! context)
  #;(void stroke-extents context double double double double) ;; TODO multiple return values
  (bool in-stroke? context double double)
  (void copy-page! context)
  (void show-page! context)
  )


;; Paths procedures
;; -----------------------------------------------

(defs
  (path copy-path context) ;; TODO Path datatype
  (path copy-path-flat context)
  (void path-destroy! path) ;; TODO Path datatype
  (void append-path! context path) ;; TODO Path datatype
  (bool has-current-point? context)
  #;(void get-current-point context double double) ;; TODO multiple return values
  (void new-path! context)
  (void new-sub-path! context)
  (void close-path! context)
  (void arc! context double double double double double)
  (void arc-negative! context double double double double double)
  (void curve-to! context double double double double double double)
  (void line-to! context double double)
  (void move-to! context double double)
  (void rectangle! context double double double double)
  #;(void glyph-path! context glyph int) ;; TODO glyph
  (void text-path! context c-string)
  (void rel-curve-to! context double double double double double double)
  (void rel-line-to! context double double)
  (void rel-move-to! context double double)
  #;(void path-extents context double double double double) ;; TODO multiple return values
  )


;; Surface procedures
;; -----------------------------------------------

(defs
  (surface surface-create-similar surface content int int)
  (surface surface-create-similar-image surface format int int)
  (surface surface-create-for-rectangle surface double double double double)
  (void surface-destroy! surface)
  (status surface-status surface)
  (void surface-finish! surface)
  (void surface-flush! surface)
  (device surface-get-device surface)
  #;(void surface-get-font-options surface font-options) ;; TODO return value
  (content surface-get-content surface)
  (void surface-mark-dirty! surface)
  (void surface-mark-dirty-rectangle! surface int int int int)
  (void surface-set-device-offset! surface double double)
  #;(void surface-get-device-offset surface double double) ;; TODO multiple return values
  #;(void surface-get-device-scale surface double double) ;; TODO multiple return values
  (void surface-set-device-scale! surface double double)
  (void surface-set-fallback-resolution! surface double double)
  #;(void surface-get-fallback-resolution surface double double) ;; TODO multiple return values
  (surface-type surface-get-type surface)
  (void surface-copy-page! surface)
  (void surface-show-page! surface)
  (bool surface-has-show-text-glyphs? surface)
  #;(status surface-set-mime-data! surface c-string blob long …) ;; TODO function pointer
  #;(void surface-get-mime-data surface c-string c-pointer long) ;; TODO multiple return values
  (bool surface-supports-mime-type? surface c-string)
  #;(surface surface-map-to-image! surface rectangle-int) ;; TODO rectangle-int
  (void surface-unmap-image! surface surface)
  )


;; Patterns procedures
;; -----------------------------------------------

(defs
  (pattern pattern-create-radial double double double double double double)
  (pattern pattern-create-mesh)
  (void pattern-destroy! pattern)
  (void pattern-add-color-stop-rgb! pattern double double double double)
  (void pattern-add-color-stop-rgba! pattern double double double double double)
  (void mesh-pattern-begin-patch! pattern)
  (void mesh-pattern-end-patch! pattern)
  (void mesh-pattern-move-to! pattern double double)
  (void mesh-pattern-line-to! pattern double double)
  (void mesh-pattern-curve-to! pattern double double double double double double)
  (void mesh-pattern-set-corner-color-rgb! pattern unsigned-int double double double)
  (void mesh-pattern-set-corner-color-rgba! pattern unsigned-int double double double double)
  (void pattern-set-extend! pattern extend))


;; Transformations procedures
;; -----------------------------------------------

(defs
  (void translate! context double double)
  (void scale! context double double)
  (void rotate! context double)
  (void identity-matrix context)
  
  )


;; Text procedures
;; -----------------------------------------------

(defs
  (void select-font-face! context c-string font-slant font-weight)
  (void set-font-size! context double)
  (void show-text! context c-string)
  (void font-extents context nonnull-f64vector)
  (void text-extents context c-string nonnull-f64vector)
  (void set-font-options! context font-options)
  (void font-options-set-antialias! font-options antialias)
  (void font-options-set-hint-style! font-options hint-style)
  (void font-options-set-hint-metrics! font-options hint-metrics))


;; Font face procedures
;; -----------------------------------------------

(defs
  (void font-face-destroy! font-face)
  (status font-face-status font-face)
  (font-type font-face-get-type font-face)
  (font-options font-options-create)
  (font-face get-font-face context)
  
  )

)