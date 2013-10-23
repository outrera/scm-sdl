;call this program Lisp Diamond

(module sdl-test "sdl.scm"

(define (blit x y dst src)
  (let ([dr (ptr-ref (malloc _SDL_Rect) _SDL_Rect)])
    (set-SDL_Rect-x! dr x)
    (set-SDL_Rect-y! dr y)
    (set-SDL_Rect-w! dr (SDL_Surface-w src))
    (set-SDL_Rect-h! dr (SDL_Surface-h src))
    (SDL_BlitSurface src #f dst dr)))

(provide-define-macro (rgb r g b)
  `(bitwise-ior (unsafe-fxlshift ,r 16) (unsafe-fxlshift ,g 8) ,b))

(define-macro (for a b c . body)
  `(let* ,a
     (while ,b
       ,@body
       ,c)))

(define-macro (times i n . body)
  (let ([nn (gensym)])
    `(for ((,i 0) (,nn ,n)) (< ,i ,nn) (inc ,i) ,@body)))

(define-const voxel-d 64) ; world's block dimension
(define-const world-d 16) ; world's dimension
(define-const world-b 32) ; world's depth

(define-const tile-w 64)
(define-const tile-h 32)

(define-const world-pw (* world-d voxel-d))
(define-const world-ph (* world-d (/ voxel-d 2)))



(define view-x 0)
(define view-y 0)
(define view-inc 5)
(define keys (make-hash))

(define tileset (make-vector 1000 #f))

;(define world (make-vector (* world-d)))
(define world (list->vector
  '(1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    1 1 1 1   1 1 1 1   1 1 1 1  1 1 1 1
    )))


#|

      00
    01  10
      11

      00
    01  10
  02  11  20
    12  21
      22

      00
    01  10
  02  11  20
03  12  21  30
  13  22  31
    23  32
      33
|#
#|
(define (test-diamond)
  (let* ([d world-d]
         [y 0]
         [yy d])
    (while (< y d)
      (times n (+ y 1)
        (let ([wx n]
              [wy (- y n)])
          (display (fmt "$wx$wy "))))
      (newline)
      (inc y))
    (while (> yy 0)
      (dec yy)
      (times n yy
        (let ([wx (+ (- d yy) n)]
              [wy (- d n 1)])
          (display (fmt "$wx$wy "))))
      (newline)
      (inc y))
    ))
|#




(define (map-to-view xy)
  (with-unsafe
   (let ([x (vector-ref xy 0)]
         [y (vector-ref xy 1)])
     (vector (- (* x (/ tile-w 2)) (* y (/ tile-w 2)))
             (- (* x (/ tile-h 2)) (* y (/ tile-h 2)))))))

(define (view-to-map xy)
  (with-unsafe
   (let ([x (vector-ref xy 0)]
         [y (vector-ref xy 1)])
     (vector (/ (+ (* y tile-w) (* x tile-h))
                (* tile-w tile-h))
             (/ (- (* y tile-w) (* x tile-h))
                (* tile-w tile-h))))))

(define (world-get x y)
  (with-unsafe
   (vector-ref world (+ (* y world-d) x))))

(define (draw-world tx ty)
  (let* ([d world-d]
         [y 0]
         [yy d])
    (while (< y d)
      (times n (+ y 1)
        (let* ([wx n]
               [wy (- y n)]
               [bx (+ (- tx (* y tile-h)) (* n tile-w))]
               [by (+ ty (* y (/ tile-h 2)))]
               [i (world-get wx wy)])
          (aif (vector-ref tileset i) (blit bx by SDL_Screen it))
          ))
      (inc y))
    (while (> yy 0)
      (dec yy)
      (times n yy
        (let* ([wx (+ (- d yy) n)]
               [wy (- d n 1)]
               [bx (+ (- tx (* (- yy 1) tile-h)) (* n tile-w))]
               [by (+ ty (* y (/ tile-h 2)))]
               [i (world-get wx wy)])
          (aif (vector-ref tileset i) (blit bx by SDL_Screen it))
          ))
      (inc y))
    ))

(define (v+ a b) (vector-map + a b))
(define (v- a b) (vector-map - a b))

(define (test-sdl)
  (SDL_Init '(SDL_INIT_VIDEO SDL_INIT_TIMER))
  (SDL_Resize 640 480)
  (let ([img (IMG_Load "data/tree-beach.png")]
        [done #f]
        [cycle 0])
    ;(vector-set! tileset 1 (IMG_Load "data/cube.png"))
    (vector-set! tileset 1 (IMG_Load "data/tile.png"))
    (till done
      (while (SDL_GetEvent)
        (cond [(SDL_ResizeEvent? it) (SDL_Resize (SDL_ResizeEvent-w it)
                                                 (SDL_ResizeEvent-h it))]
              [(SDL_QuitEvent? it) (set! done #t)]
              [(SDL_KeyboardEvent? it)
               (let* ([ks (SDL_KeyboardEvent-keysym it)]
                      [s (SDL_keysym-sym ks)]
                      [kn (SDL_GetKeyName s)]
                      [p (not (eq? (SDL_KeyboardEvent-state it)
                                   SDL_RELEASED))])
                 (hash-set! keys s p)
                 (say "kb: $kn $p ($s)"))]
              [(SDL_MouseButtonEvent? it)
               (let* ([b (SDL_MouseButtonEvent-button it)]
                      [x (SDL_MouseButtonEvent-x it)]
                      [y (SDL_MouseButtonEvent-y it)]
                      [wc (view-to-map (vector (- x view-x) (- y view-y)))])
                 (when (eq? b 1)
                   (say "$wc"))
                 ;;(say "mice-button ($b)")
                 )]
              ;;[(SDL_MouseMotionEvent? it) (say "mice-motion")]
              ))
      #| access SDL_Surface-pixels here |#
      ;;(set-point 320 240 #xFFFFFFFF)
      (SDL_FillRect SDL_Screen #f (rgb 0 #xA0 #xC0)) ; clear screen
      ;(blit 260 260 SDL_Screen cube)
      (when (hash-ref keys SDLK_UP #f) (set! view-y (+ view-y view-inc)))
      (when (hash-ref keys SDLK_DOWN #f) (set! view-y (- view-y view-inc)))
      (when (hash-ref keys SDLK_LEFT #f) (set! view-x (+ view-x view-inc)))
      (when (hash-ref keys SDLK_RIGHT #f) (set! view-x (- view-x view-inc)))
      (draw-world (- view-x (/ tile-w 2)) view-y)
      (SDL_Flip SDL_Screen)
      ;(say "$cycle")
      (set! cycle (+ cycle 1))
      )
    (SDL_Quit)
    ))
)