(module sdl-hlp "snv.scm"
(require (for-syntax "snv.scm"))
(require ffi/unsafe)
(require racket/unsafe/ops)
 
(define-for-syntax group
  (case-lambda
    [(n xs) (group n 'dont-pad xs)]
    [(n pad xs) (let* ([rs '()]
                       [l (length xs)]
                       [m (ceiling (/ l n))])
                  (times i m
                    (let ([ys '()])
                      (times j n
                        (if (null? xs)
                            (unless (eqv? pad 'dont-pad) (push pad ys))
                            (push (pop xs) ys)))
                      (push (reverse! ys) rs)))
                  (reverse! rs))]))
  
(define group
  (case-lambda
    [(n xs) (group n 'dont-pad xs)]
    [(n pad xs) (let* ([rs '()]
                       [l (length xs)]
                       [m (ceiling (/ l n))])
                  (times i m
                    (let ([ys '()])
                      (times j n
                        (if (null? xs)
                            (unless (eqv? pad 'dont-pad) (push pad ys))
                            (push (pop xs) ys)))
                      (push (reverse! ys) rs)))
                  (reverse! rs))]))
  
(define libsdl (ffi-lib "libSDL"))
(define libsdl-image (ffi-lib "libSDL_image"))
(define libsdl-mixer (ffi-lib "libSDL_mixer"))

(define-macro (sdlfun name . args)
  `(define ,name
     (get-ffi-obj ,(symbol->string name) libsdl
                  (_fun ,@args))))

(define-macro (sdlimgfun name . args)
  `(define ,name
     (get-ffi-obj ,(symbol->string name) libsdl-image
                  (_fun ,@args))))

(define-macro (sdlmixfun name . args)
  `(define ,name
     (get-ffi-obj ,(symbol->string name) libsdl-mixer
                  (_fun ,@args))))
  
(define _SdlSystems
  (_bitmask '(SDL_INIT_TIMER       = #x00000001
              SDL_INIT_AUDIO       = #x00000010
              SDL_INIT_VIDEO       = #x00000020
              SDL_INIT_CDROM       = #x00000100
              SDL_INIT_JOYSTICK    = #x00000200
              SDL_INIT_NOPARACHUTE = #x00100000
              SDL_INIT_EVENTTHREAD = #x01000000
              SDL_INIT_EVERYTHING  = #x0000FFFF)))

(define _SdlFlags
  (_bitmask '(SDL_SWSURFACE   = #x00000000
              SDL_HWSURFACE   = #x00000001
              SDL_ASYNCBLIT   = #x00000004
              SDL_ANYFORMAT   = #x10000000
              SDL_HWPALETTE   = #x20000000
              SDL_DOUBLEBUF   = #x40000000
              SDL_FULLSCREEN  = #x80000000
              SDL_OPENGL      = #x00000002
              SDL_RESIZEABLE  = #x00000010
              SDL_NOFRAME     = #x00000020
              SDL_HWACCEL     = #x00000100
              SDL_SRCCOLORKEY = #x00001000
              SDL_RLEACCELOK  = #x00002000
              SDL_RLEACCEL    = #x00004000
              SDL_SRCALPHA    = #x00010000
              SDL_PREALLOC    = #x01000000)))

(define-const SDL_RELEASED 0)
(define-const SDL_PRESSED 1)

;(map (fn (x xs) (cons x xs)) (rng 0 (length SdlEvents)) SdlEvents)
(define-for-syntax SdlEvents (group 3
   ; Symbol:            FFI Struct:           Description:
  '(SDL_NOEVENT         #f                    "Unused (do not remove)"
    SDL_ACTIVEEVENT     _SDL_ActiveEvent      "Application loses/gains visibility"
    SDL_KEYDOWN         _SDL_KeyboardEvent    "Keys pressed"
    SDL_KEYUP           _SDL_KeyboardEvent    "Keys released"
    SDL_MOUSEMOTION     _SDL_MouseMotionEvent "Mouse moved"
    SDL_MOUSEBUTTONDOWN _SDL_MouseButtonEvent "Mouse button pressed"
    SDL_MOUSEBUTTONUP   _SDL_MouseButtonEvent "Mouse button released"
    SDL_JOYAXISMOTION   _SDL_JoyAxisEvent     "Joystick axis motion"
    SDL_JOYBALLMOTION   _SDL_JoyBallEvent     "Joystick trackball motion"
    SDL_JOYHATMOTION    _SDL_JoyHatEvent      "Joystick hat position change"
    SDL_JOYBUTTONDOWN   _SDL_JoyButtonEvent   "Joystick button pressed"
    SDL_JOYBUTTONUP     _SDL_JoyButtonEvent   "Joystick button released"
    SDL_QUIT            _SDL_QuitEvent        "User-requested quit"
    SDL_SYSWMEVENT      #f                    "System specific event"
    SDL_EVENT_RESERVEDA #f                    "Reserved for future use.."
    SDL_EVENT_RESERVEDB #f                    "Reserved for future use.."
    SDL_VIDEORESIZE     _SDL_ResizeEvent      "User resized video mode"
    SDL_VIDEOEXPOSE     _SDL_ExposeEvent      "Screen needs to be redrawn"
    SDL_EVENT_RESERVED2 #f                    "Reserved for future use.."
    SDL_EVENT_RESERVED3 #f                    "Reserved for future use.."
    SDL_EVENT_RESERVED4 #f                    "Reserved for future use.."
    SDL_EVENT_RESERVED5 #f                    "Reserved for future use.."
    SDL_EVENT_RESERVED6 #f                    "Reserved for future use.."
    SDL_EVENT_RESERVED7 #f                    "Reserved for future use.."
    )))

(define-for-syntax SDLKey '(
  SDLK_BACKQUOTE 96
  SDLK_a   97
  SDLK_b   98
  SDLK_c   99
  SDLK_d   100
  SDLK_e   101
  SDLK_f   102
  SDLK_g   103
  SDLK_h   104
  SDLK_i   105
  SDLK_j   106
  SDLK_k   107
  SDLK_l   108
  SDLK_m   109
  SDLK_n   110
  SDLK_o   111
  SDLK_p   112
  SDLK_q   113
  SDLK_r   114
  SDLK_s   115
  SDLK_t   116
  SDLK_u   117
  SDLK_v   118
  SDLK_w   119
  SDLK_x   120
  SDLK_y   121
  SDLK_z   122
  SDLK_DELETE 127
  ; End of ASCII mapped keysyms

  ; International keyboard syms
  SDLK_WORLD_0 160  ; 0xA0
  SDLK_WORLD_1 161
  SDLK_WORLD_2 162
  SDLK_WORLD_3 163
  SDLK_WORLD_4 164
  SDLK_WORLD_5 165
  SDLK_WORLD_6 166
  SDLK_WORLD_7 167
  SDLK_WORLD_8 168
  SDLK_WORLD_9 169
  SDLK_WORLD_10 170
  SDLK_WORLD_11 171
  SDLK_WORLD_12 172
  SDLK_WORLD_13 173
  SDLK_WORLD_14 174
  SDLK_WORLD_15 175
  SDLK_WORLD_16 176
  SDLK_WORLD_17 177
  SDLK_WORLD_18 178
  SDLK_WORLD_19 179
  SDLK_WORLD_20 180
  SDLK_WORLD_21 181
  SDLK_WORLD_22 182
  SDLK_WORLD_23 183
  SDLK_WORLD_24 184
  SDLK_WORLD_25 185
  SDLK_WORLD_26 186
  SDLK_WORLD_27 187
  SDLK_WORLD_28 188
  SDLK_WORLD_29 189
  SDLK_WORLD_30 190
  SDLK_WORLD_31 191
  SDLK_WORLD_32 192
  SDLK_WORLD_33 193
  SDLK_WORLD_34 194
  SDLK_WORLD_35 195
  SDLK_WORLD_36 196
  SDLK_WORLD_37 197
  SDLK_WORLD_38 198
  SDLK_WORLD_39 199
  SDLK_WORLD_40 200
  SDLK_WORLD_41 201
  SDLK_WORLD_42 202
  SDLK_WORLD_43 203
  SDLK_WORLD_44 204
  SDLK_WORLD_45 205
  SDLK_WORLD_46 206
  SDLK_WORLD_47 207
  SDLK_WORLD_48 208
  SDLK_WORLD_49 209
  SDLK_WORLD_50 210
  SDLK_WORLD_51 211
  SDLK_WORLD_52 212
  SDLK_WORLD_53 213
  SDLK_WORLD_54 214
  SDLK_WORLD_55 215
  SDLK_WORLD_56 216
  SDLK_WORLD_57 217
  SDLK_WORLD_58 218
  SDLK_WORLD_59 219
  SDLK_WORLD_60 220
  SDLK_WORLD_61 221
  SDLK_WORLD_62 222
  SDLK_WORLD_63 223
  SDLK_WORLD_64 224
  SDLK_WORLD_65 225
  SDLK_WORLD_66 226
  SDLK_WORLD_67 227
  SDLK_WORLD_68 228
  SDLK_WORLD_69 229
  SDLK_WORLD_70 230
  SDLK_WORLD_71 231
  SDLK_WORLD_72 232
  SDLK_WORLD_73 233
  SDLK_WORLD_74 234
  SDLK_WORLD_75 235
  SDLK_WORLD_76 236
  SDLK_WORLD_77 237
  SDLK_WORLD_78 238
  SDLK_WORLD_79 239
  SDLK_WORLD_80 240
  SDLK_WORLD_81 241
  SDLK_WORLD_82 242
  SDLK_WORLD_83 243
  SDLK_WORLD_84 244
  SDLK_WORLD_85 245
  SDLK_WORLD_86 246
  SDLK_WORLD_87 247
  SDLK_WORLD_88 248
  SDLK_WORLD_89 249
  SDLK_WORLD_90 250
  SDLK_WORLD_91 251
  SDLK_WORLD_92 252
  SDLK_WORLD_93 253
  SDLK_WORLD_94 254
  SDLK_WORLD_95 255  ; 0xFF

  ; Numeric keypad
  SDLK_KP0 256
  SDLK_KP1 257
  SDLK_KP2 258
  SDLK_KP3 259
  SDLK_KP4 260
  SDLK_KP5 261
  SDLK_KP6 262
  SDLK_KP7 263
  SDLK_KP8 264
  SDLK_KP9 265
  SDLK_KP_PERIOD 266
  SDLK_KP_DIVIDE 267
  SDLK_KP_MULTIPLY 268
  SDLK_KP_MINUS 269
  SDLK_KP_PLUS 270
  SDLK_KP_ENTER 271
  SDLK_KP_EQUALS 272

  ; Arrows + Home/End pad
  SDLK_UP   273
  SDLK_DOWN 274
  SDLK_RIGHT 275
  SDLK_LEFT 276
  SDLK_INSERT 277
  SDLK_HOME 278
  SDLK_END 279
  SDLK_PAGEUP 280
  SDLK_PAGEDOWN 281

  ; Function keys
  SDLK_F1   282
  SDLK_F2   283
  SDLK_F3   284
  SDLK_F4   285
  SDLK_F5   286
  SDLK_F6   287
  SDLK_F7   288
  SDLK_F8   289
  SDLK_F9   290
  SDLK_F10 291
  SDLK_F11 292
  SDLK_F12 293
  SDLK_F13 294
  SDLK_F14 295
  SDLK_F15 296

  ; Key state modifier keys
  SDLK_NUMLOCK 300
  SDLK_CAPSLOCK 301
  SDLK_SCROLLOCK 302
  SDLK_RSHIFT 303
  SDLK_LSHIFT 304
  SDLK_RCTRL 305
  SDLK_LCTRL 306
  SDLK_RALT 307
  SDLK_LALT 308
  SDLK_RMETA 309
  SDLK_LMETA 310
  SDLK_LSUPER 311  ; Left "Windows" key
  SDLK_RSUPER 312  ; Right "Windows" key
  SDLK_MODE 313      ; "Alt Gr" key
  SDLK_COMPOSE 314  ; Multi-key compose key

  ; Miscellaneous function keys
  SDLK_HELP 315
  SDLK_PRINT 316
  SDLK_SYSREQ 317
  SDLK_BREAK 318
  SDLK_MENU 319
  SDLK_POWER 320  ; Power Macintosh power key
  SDLK_EURO 321  ; Some european keyboards
  SDLK_UNDO 322  ; Atari keyboard has Undo
  ))
  
(define-for-syntax SDLMod '(
  KMOD_NONE      #x0000
  KMOD_LSHIFT    #x0001
  KMOD_RSHIFT    #x0002
  KMOD_LCTRL     #x0040
  KMOD_RCTRL     #x0080
  KMOD_LALT      #x0100
  KMOD_RALT      #x0200
  KMOD_LMETA     #x0400
  KMOD_RMETA     #x0800
  KMOD_NUM       #x1000
  KMOD_CAPS      #x2000
  KMOD_MODE      #x4000
  KMOD_RESERVED  #x8000))

(define-xs SDLKey)
(define-xs SDLMod)
(define-const KMOD_CTRL (bitwise-ior KMOD_LCTRL KMOD_RCTRL))
(define-const KMOD_SHIFT (bitwise-ior KMOD_LSHIFT KMOD_RSHIFT))
(define-const KMOD_ALT (bitwise-ior KMOD_LALT KMOD_RALT))
(define-const KMOD_META (bitwise-ior KMOD_LMETA KMOD_RMETA))

#|  
(define-macro (sdl-keys-to-hash)
  `(define SLD_GetKeySym
     ,@(map (λ (args)
              (match args
                ((s n) `(define-const ,s ,n))))
            (group 2 SdlKeys))))
(sdl-expose-keys)|#
  
(define-cstruct _SDL_keysym
  ((type _uint8)       ; hardware specific scancode
   (sym _int)          ; SDL virtual keysym (SDLKey)
   (mod _int)          ; current key modifiers (SDLMod)
   (unicode _uint16))) ; translated character

(define-cstruct _SDL_ActiveEvent
  ((type _uint8)    ; SDL_ACTIVEEVENT
   (gain _uint8)    ; Whether given states were gained or lost (1/0) 
   (state _uint8))) ; A mask of the focus states

(define-cstruct _SDL_KeyboardEvent
  ((type _uint8)    ; SDL_KEYDOWN or SDL_KEYUP 
   (which _uint8)   ; The keyboard device index
   (state _uint8)   ; SDL_PRESSED or SDL_RELEASED
   (keysym _SDL_keysym)))

(define-cstruct _SDL_MouseMotionEvent
  ((type _uint8)     ; SDL_MOUSEMOTION
   (which _uint8)    ; The mouse device index 
   (state _uint8)    ; The current button state
   (x _uint16) (y _uint16) ; The X/Y coordinates of the mouse         
   (xrel _sint16)    ; The relative motion in the X direction
   (yrel _sint16)))  ; The relative motion in the Y direction

(define-cstruct _SDL_MouseButtonEvent
  ((type _uint8)     ; SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
   (which _uint8)    ; The mouse device index
   (button _uint8)   ; The mouse button index
   (state _uint8)    ; SDL_PRESSED or SDL_RELEASED 
   (x _uint16) (y _uint16))) ; The X/Y coordinates of the mouse at press time

(define-cstruct _SDL_JoyAxisEvent
  ((type _uint8)     ; SDL_JOYAXISMOTION
   (which _uint8)    ; The joystick device index
   (axis _uint8)     ; The joystick axis index
   (value _sint16))) ; The axis value (range: -32768 to 32767)

(define-cstruct _SDL_JoyBallEvent
  ((type _uint8)     ; SDL_JOYBALLMOTION
   (which _uint8)    ; The joystick device index
   (ball _uint8)     ; The joystick trackball index
   (xrel _sint16)    ; The relative motion in the X direction
   (yrel _sint16)))  ; The relative motion in the Y direction

(define-cstruct _SDL_JoyHatEvent
  ((type _uint8)     ; SDL_JOYHATMOTION
   (which _uint8)    ; The joystick device index
   (hat _uint8)      ; The joystick hat index
   (value _uint8)))  ; The hat position value:
                     ;   SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
                     ;   SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
                     ;   SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
                     ; Note that zero means the POV is centered.

(define-cstruct _SDL_JoyButtonEvent
  ((type _uint8)     ; SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
   (which _uint8)    ; The joystick device index
   (button _uint8)   ; The joystick button index
   (state _uint8)))  ; SDL_PRESSED or SDL_RELEASED

(define-cstruct _SDL_ResizeEvent
  ((type _uint8)    ; SDL_VIDEORESIZE
   (w _int)         ; New width
   (h _int)))       ; New height

(define-cstruct _SDL_ExposeEvent
  ((type _uint8)))    ; SDL_VIDEOEXPOSE

(define-cstruct _SDL_QuitEvent
  ((type _uint8)))    ; SDL_QUIT

(define-cstruct _SDL_UserEvent
  ((type _uint8)      ; SDL_USEREVENT through SDL_NUMEVENTS-1
   (code _int)        ; User defined event code
   (data1 _pointer)   ; User defined data pointer
   (data2 _pointer))) ; User defined data pointer

(define-cstruct _SDL_Rect
  ((x _int16)
   (y _int16)
   (w _uint16)
   (h _uint16)))

(define-cstruct _SDL_Color
  ((r _uint8)
   (g _uint8)
   (b _uint8)
   (unused _uint8)))

(define-cstruct _SDL_Palette
  ((ncolors _int)
   (colors _SDL_Color-pointer)))

(define-cstruct _SDL_PixelFormat
  ((palette _pointer)
   (BitsPerPixel _uint8)
   (BytesPerPixel _uint8)
   (Rloss _uint8)
   (Gloss _uint8)
   (Bloss _uint8)
   (Aloss _uint8)
   (Rshift _uint8)
   (Gshift _uint8)
   (Bshift _uint8)
   (Ashift _uint8)
   (Rmask _uint32)
   (Gmask _uint32)
   (Bmask _uint32)
   (Amask _uint32)
   (colorkey _uint32) ; RGB color key information 
   (alpha _uint8))) ;  Alpha value information (per-surface alpha)
  
(define-cstruct _SDL_Surface
  ((flags _uint32)
   (format _SDL_PixelFormat-pointer)   ; pixel format
   (w _int)                 ; width
   (h _int)                 ; height
   (pitch _uint16)
   (pixels _pointer)
   (offset _int)            ; private
   (hwdata _pointer)        ; private: hardware specific data
   (clip_rect _SDL_Rect)    ; current clipping
   (unused1 _uint32)        ; for binary compatibility
   (locked _uint32)         ; private
   (map _pointer)           ; private
   (format_version _uint32) ; private
   (refcount _int)          ; number of references to this surface
   ))

(define _SDL_Surface-gc-ptr
  (make-ctype _SDL_Surface-pointer #f
    (λ (x)
      (register-finalizer x SDL_FreeSurface)
      x)))

;;(define SDL_Surface (make-ctype _pointer #f #f))

(sdlfun SDL_Init (sdl_systems : _SdlSystems) -> _int)
(sdlfun SDL_Quit -> _void)
  
(sdlfun SDL_SetVideoMode (width : _int) (height : _int) (bpp : _int) (flags : _SdlFlags)
  -> _SDL_Surface-pointer)
(sdlfun SDL_GetVideoSurface -> _SDL_Surface-pointer)
(sdlfun SDL_LockSurface (screen : _SDL_Surface-pointer) -> _int)
(sdlfun SDL_UnlockSurface (screen : _SDL_Surface-pointer) -> _void)
(sdlfun SDL_Flip (screen : _SDL_Surface-pointer) -> _int)
(sdlfun SDL_CreateRGBSurface (flags : _SdlFlags) (width : _int) (height : _int) (depth : _int)
                             (Rmask : _uint32) (Gmask : _uint32) (Bmask : _uint32) (Amask : _uint32)
  -> _SDL_Surface-gc-ptr)
(sdlfun SDL_CreateRGBSurfaceFrom (pixels : _pointer) (flags : _SdlFlags)
        (width : _int) (height : _int) (depth : _int) (pitch : _int)
        (Rmask : _uint32) (Gmask : _uint32) (Bmask : _uint32) (Amask : _uint32)
  -> _SDL_Surface-gc-ptr)
(sdlfun SDL_FreeSurface (screen : _SDL_Surface-pointer) -> _void)

(sdlfun SDL_ConvertSurface (src : _SDL_Surface-pointer) (fmt : _SDL_PixelFormat)  (flags : _SdlFlags)
  -> _SDL_Surface-gc-ptr)

(sdlfun SDL_UpperBlit (src : _SDL_Surface-pointer) (srcrect : _pointer)
                      (dst : _SDL_Surface-pointer) (dstrect : _pointer)
  -> _int)

(define-macro (SDL_BlitSurface src srcrect dst dstrect)
  `(SDL_UpperBlit ,src ,srcrect ,dst ,dstrect))

(sdlfun SDL_PollEvent (event : _pointer) -> _int)

(sdlfun SDL_EnableUNICODE (enable : _int) -> _int)
(sdlfun SDL_GetKeyName (key : _int) -> _string)

(sdlfun SDL_FillRect (dst : _SDL_Surface-pointer) (dstrect : _pointer)
        (color : _uint32)
  -> _int)

(sdlfun SDL_GetTicks -> _uint32)
(sdlfun SDL_Delay (ms : _uint32) -> _void)

(sdlimgfun IMG_Load (file : _string) -> _SDL_Surface-gc-ptr)

(define-macro (event-case x)
  `(case [ptr-ref ,x _uint8]
     ,@(map (λ (args)
              (match args
                ((n sym struc _) `((,n) ,(if struc
                                             `(ptr-ref ,x ,struc)
                                             #f)))))
            (map (λ (x xs) (cons x xs))
                 (rng 0 (length SdlEvents)) SdlEvents))))

(define (SDL_GetEvent)
  (let ([event (malloc 128)])
    (cond
      [(= (SDL_PollEvent event) 0)  #f]
      [(event-case event) => (fn (it) it)]
      [else (SDL_GetEvent)])))


(define (SDL_GetEvents)
  (let ([r '()])
    (while (SDL_GetEvent) (push it r))
    (reverse! r)))

(define SDL_Screen #f)

(define (SDL_Resize w h)
  (set! SDL_Screen
        (SDL_SetVideoMode w h 32 '(SDL_ANYFORMAT SDL_HWSURFACE SDL_RESIZEABLE))))


(define-macro (with-unsafe . body)
  `(let ([+ unsafe-fx+]
         [- unsafe-fx-]
         [* unsafe-fx*]
         [/ unsafe-fxquotient]
         [remainder unsafe-fxremainder])
    ,@body))

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out racket/unsafe/ops))
(provide (all-from-out "snv.scm"))



#|
(define (set-point x y c)
  (let* ([p (SDL_Surface-pixels SDL_Screen)]
         [w (SDL_Surface-w SDL_Screen)]
         [h (SDL_Surface-h SDL_Screen)])
    (ptr-set! p _uint32 (unsafe-fx+ (unsafe-fx* y w) x) c)
    ))


(define (clear-screen c)
  (let* ([p (SDL_Surface-pixels SDL_Screen)]
         [w (SDL_Surface-w SDL_Screen)]
         [h (SDL_Surface-h SDL_Screen)]
         [e (* w h)]
         [a (ptr-ref p (_array _uint32 e))])
    (let loop ([i 0])
      (when (unsafe-fx< i e)
        ;;(ptr-set! p _uint32 i c)
        ;;(unsafe-array-set! a i c)
        (loop (unsafe-fx+ i 1))))))
|#

) ;; module sdl
