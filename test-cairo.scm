; A test program for the Cairo bindings
; Michael Bridgen <mikeb@squaremobius.net>
; Tony Garnock-Jones <tonyg@kcbbs.gen.nz>

(use posix)
(use sdl)
(use cairo)
(import chicken scheme foreign)

(declare
 (foreign-declare "#include <SDL/SDL.h>\n")
 (run-time-macros)
)
(foreign-code "SDL_Init(SDL_INIT_EVERYTHING);")



(sdl-init SDL_INIT_EVERYTHING)

(define maxx 640)
(define maxy 480)

(sdl-wm-set-caption "TestCairo" "TestCairo")
(define s (sdl-set-video-mode maxx maxy 0 (+ SDL_HWSURFACE
					     SDL_HWPALETTE
					     SDL_DOUBLEBUF)))

(sdl-fill-rect s (make-sdl-rect 0 0 maxx maxy) (sdl-map-rgb (sdl-surface-pixel-format s) 0 0 0))
(sdl-flip s)

(define is (cairo-image-surface-create-for-data
            (sdl-surface-pixels s)
            CAIRO_FORMAT_RGB24 maxx maxy
            (sdl-surface-pitch s)))

(define c (cairo-create is))

(cairo-set-source-rgba c 1 1 0 1)

(cairo-set-line-width c 20)

(cairo-new-path c)
(cairo-set-line-cap c CAIRO_LINE_CAP_BUTT)
(cairo-move-to c 10 10)
(cairo-line-to c 10 80)

(cairo-stroke c)

(cairo-new-path c)
(cairo-set-line-cap c CAIRO_LINE_CAP_ROUND)
(cairo-move-to c 50 10)
(cairo-line-to c 50 80)
(cairo-stroke c)

(cairo-new-path c)
(cairo-set-line-cap c CAIRO_LINE_CAP_SQUARE)
(cairo-move-to c 90 10)
(cairo-line-to c 90 80)
(cairo-stroke c)

(cairo-set-line-join c CAIRO_LINE_JOIN_BEVEL)
(define (tri)
  (cairo-new-path c)
  (cairo-move-to c 110 110)
  (cairo-line-to c 110 190)
  (cairo-line-to c 190 190)
  (cairo-close-path c))

(cairo-set-line-width c 10)
(tri)
(cairo-set-source-rgb c 0 1 1)
(cairo-stroke c)
(tri)
(cairo-set-source-rgb c 1 0 1)
(cairo-fill c)

(define (radians degrees)
  (* 3.142 (/ degrees 180)))

(define (sector x y d)
  (cairo-new-path c)
  (cairo-move-to c x y)
  (cairo-line-to c (+ x d) y)
  (cairo-line-to c (+ x d) (+ y d))
  (cairo-arc c (+ x d) y d (radians 90) (radians 180)))

(sector 240 240 60)
(cairo-set-line-join c CAIRO_LINE_JOIN_MITER)
(cairo-set-source-rgb c 1 0.5 0)
(cairo-stroke c)

(cairo-reset-clip c)
(cairo-new-path c)
(cairo-rectangle c 30 240 70 300)
(cairo-clip c)
(cairo-new-path c)
(sector 20 250 100)
(cairo-set-source-rgb c 0 0.5 1)
(cairo-fill c)

(cairo-reset-clip c)
(sector 20 250 100)
(cairo-set-source-rgba c 0 0.5 1 0.3)
(cairo-fill c)

(cairo-select-font-face c "sans-serif" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
(cairo-set-font-size c 30)

(cairo-move-to c 300 100)
(cairo-set-source-rgba c 1 1 1 1)
(cairo-show-text c "Chicken Cairo")

(let ((ext (make-cairo-text-extents-type)))
  (cairo-text-extents c "Chicken Cairo" ext)
;  (display ext)(newline)
  (cairo-new-path c)
  (cairo-rectangle c 300 100 (cairo-text-extents-width ext) (- (cairo-text-extents-height ext)))
  (cairo-set-source-rgba c 1 1 1 0.5)
  (cairo-set-line-width c 2.0)
  (cairo-stroke c))

(sdl-flip s)

(let ((event (make-sdl-event)))
  (let loop ()
    (sdl-wait-event! event)
    (let ((t (sdl-event-type event)))
      (if (= t SDL_QUIT)
	  'done
	  (loop)))))

(exit 0)
