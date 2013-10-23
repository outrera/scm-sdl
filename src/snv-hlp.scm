(module snv-hlp racket/base
(require (except-in racket take drop any match))
(require mzlib/defmacro)
(require (only-in srfi/1 reverse!))
(require (only-in srfi/13 string-null?))
(require (only-in mzlib/match match))
;(require (lib "defmacro.ss"))
;(require (lib "list.ss"))
;(require (lib "string.ss"))

;(require (only (lib "../srfi/13.ss")
;               string-null?))

  
#|
(require mzlib/defmacro)

(defmacro aif (cond then else)
  `(let ((it ,cond))
     (if it ,then ,else)))

Or if that library didn't exist:

(define-syntax (defmac stx)
  (syntax-case stx ()
    ((defmacro <name> <args> . <body>)
     #`(define-syntax (<name> stx)
         (syntax-case stx ()
           ((<name> . <<args>>)
            (datum->syntax
             #'<name>
             (apply
              (lambda <args> . <body>)
              (syntax->datum #'<<args>>)))))))))
|#

(provide (all-from-out racket))
(provide reverse! string-null? match)

(provide define-macro)

  
(define-macro (provide-define-macro expr . body)
  `(begin (define-macro ,expr ,@body)
          (provide ,(car expr))))
  
(provide provide-define-macro)

(provide-define-macro (provide-define expr . body)
  `(begin (define ,expr ,@body)
          (provide ,(if (list? expr) (car expr) expr))))

(provide-define-macro (define-const sym expr)
  `(define-syntax (,sym stx-obj) #',expr))
  
(provide-define-macro (fn args . body)
  `(lambda ,args ,@body))

(provide-define-macro (aif test then . else)
  (if (not (null? else))
      `(let ((it ,test))
         (if it ,then ,else))
      `(let ((it ,test))
         (if it ,then #f))))

(provide-define-macro (push elem xs) `(set! ,xs (cons ,elem ,xs)))
(provide-define-macro (pop xs)
  (let ([r (gensym)])
    `(let ([,r (car xs)])
       (set! ,xs (cdr ,xs))
       ,r)))
(provide-define-macro (inc x) `(set! ,x (+ ,x 1)))
(provide-define-macro (dec x) `(set! ,x (- ,x 1)))

(provide-define-macro (while condition . body)
  (let ([loop (gensym)])
    `(letrec ([,loop (λ ()
                       (let ([it ,condition])
                         (when it ,@body (,loop))))])
       (,loop))))

(provide-define-macro (till condition . body)
  (let ([loop (gensym)]
        [c (gensym)])
    `(letrec ([,loop (λ ()
                       (let ([,c ,condition])
                         (when (not ,c) ,@body (,loop))
                         ,c))])
       (,loop))))

(provide-define-macro (times i n . body)
  (let* ([nn (gensym)])
    `(let ([,i 0])
       (while (< ,i ,n) ,@body (inc ,i)))))

(provide-define (rng low high)
  (cond
    [(< low high) (cons low (rng (+ low 1) high))]
    [(> low high) (cons low (rng (- low 1) high))]
    [else null]))

(provide-define (take n xs)
  (if (> n 0)
      (cons (car xs) (take (- n 1) (cdr xs)))
      null))

(provide-define (drop n xs)
  (if (> n 0)
      (drop (- n 1) (cdr xs))
      xs))

(provide-define-macro (block name . body)
  `(call/cc (lambda (,name) ,@body)))

(provide-define (all f xs)
  (block result
    (till (null? xs)
      (unless (f (pop xs)) (result #f)))
    #t))

(provide-define (any f xs)
  (block result
    (till (null? xs)
      (when (f (pop xs)) (result #t)))
    #f))

(provide-define (string-split sep xs)
  (let ([r '()]
        [s 0]
        [l (string-length xs)])
    (times i l
      (let ([x (string-ref xs i)])
        (when (char=? x sep)
          (push (substring xs s i) r)
          (set! s (+ 1 i)))))
    (push (substring xs s l) r)
    (reverse! r)))

(provide-define-macro (/= a b) `(not (= ,a ,b)))
) ;snv-hlp


