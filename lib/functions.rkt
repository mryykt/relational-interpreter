#lang racket
(require syntax/parse/define)
(require minikanren)
(require minikanren/numbers)
(require "big-step.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "type-inference.rkt")
(require "parse.rkt")

(provide (all-defined-out))

; util
(define all-functions-list '())
(define all-basic-functions '())
(define all-list-functions '())
(define all-advanced-list-functions '())

(define-syntax def-function
  (syntax-rules ()
    [(_ functions name expr)
     (begin
       (set! all-functions-list (cons (cons (symbol-trim-last 'name) expr) all-functions-list))
       (set! functions (cons (cons (symbol-trim-last 'name) expr) functions))
       (define name expr))]))

(define-syntax def-basic-function
  (syntax-rules ()
    [(_ name expr) (def-function all-basic-functions name expr)]))

(define-syntax def-list-function
  (syntax-rules ()
    [(_ name expr) (def-function all-list-functions name expr)]))

(define-syntax def-advanced-list-function
  (syntax-rules ()
    [(_ name expr) (def-function all-advanced-list-functions name expr)]))

; macro
(define (symbol-trim-last sym)
  (let ([s (symbol->string sym)]) (string->symbol (substring s 0 (max 0 (sub1 (string-length s)))))))

(define-syntax make-program
  (syntax-rules ()
    [(_ c) `c]
    [(_ f c)
     `(let ,(symbol-trim-last 'f)
        ,f
        c)]
    [(_ f1 f2 ...)
     `(let ,(symbol-trim-last 'f1)
        ,f1
        ,(make-program f2 ...))]))

(define-syntax apps
  (syntax-rules ()
    [(_ f) f]
    [(_ f x) `(app f x)]
    [(_ f x y ...) (apps (app f x) y ...)]))

(define (with-functions ls c)
  (if (eq? ls '())
      c
      `(let ,(caar ls)
         ,(cdar ls)
         ,(with-functions (cdr ls) c))))

(define (with-all-functions c)
  (with-functions all-functions-list c))

; basic function
(def-basic-function orf
                    '(lam x
                          (lam y
                               (if (var x)
                                   false
                                   ((var y) = true)))))

(def-basic-function andf
                    '(lam x
                          (lam y
                               (if (var x)
                                   ((var y) = true)
                                   false))))

(def-basic-function notf '(lam x (if (var x) false true)))

(def-basic-function consf '(lam x (lam y (cons (var x) (var y)))))

(def-basic-function ltf '(lam x (lam y ((var x) < (var y)))))

(def-basic-function addf '(lam x (lam y ((var x) + (var y)))))

(def-basic-function eqf (parser '(lambda (x y) (x = y))))

(def-basic-function neqf (parser '(lambda (x y) (if (x = y) #f #t))))

(def-basic-function maxf (parser '(lambda (x y) (if (x < y) y x))))

(def-basic-function minf (parser '(lambda (x y) (if (x < y) x y))))
; basic combinator

(def-basic-function flipf '(lam f (lam x (lam y (app (app (var f) (var y)) (var x))))))

(def-basic-function composef '(lam f (lam g (lam x (app (var f) (app (var g) (var x)))))))

(def-basic-function forkf (parser '(lambda (f g x y) (f x (g x y)))))

(def-basic-function constf (parser '(lambda (x y) x)))
; list functions
(def-list-function foldlf
                   (parser '(fix f
                                 (g acc xs)
                                 (if (xs = ())
                                     acc
                                     (f g (g acc (car xs)) (cdr xs))))))

(def-list-function foldrf
                   '(fix f
                         g
                         (lam init
                              (lam xs
                                   (if ((var xs) = ())
                                       (var init)
                                       (app (app (var g) (car (var xs)))
                                            (app (app (app (var f) (var g)) (var init))
                                                 (cdr (var xs)))))))))

(def-list-function foldr1f
                   (parser '(fix f
                                 (g xs)
                                 (if ((cdr xs) = ())
                                     (car xs)
                                     (g (car xs) (f g (cdr xs)))))))

(def-list-function mapf
                   '(fix f
                         g
                         (lam xs
                              (if ((var xs) = ())
                                  ()
                                  (cons (app (var g) (car (var xs)))
                                        (app (app (var f) (var g)) (cdr (var xs))))))))

(def-list-function scanlf
                   '(fix f
                         g
                         (lam acc
                              (lam xs
                                   (if ((var xs) = ())
                                       ()
                                       (let acc2 (app
                                                  [app
                                                   (var g)
                                                   (car (var xs))]
                                                  [var acc])
                                         (cons (var acc2)
                                               (app (app (app (var f) (var g)) (var acc2))
                                                    (cdr (var xs))))))))))

(define appendf
  `(let foldr
     ,foldrf
     (lam xs (lam ys ,(apps (var foldr) (lam x (lam y (cons (var x) (var y)))) (var ys) (var xs))))))

(define concatf (make-program foldlf appendf ,(apps (var foldl) (var append) ())))

(define lengthf
  '(fix f
        xs
        (if ((var xs) = ())
            (num ())
            ((num (1)) + (app (var f) (cdr (var xs)))))))

(def-list-function filterf
                   `(fix f
                         g
                         (lam xs
                              (if ((var xs) = ())
                                  ()
                                  (let tail ,(apps (var f) (var g) (cdr (var xs)))
                                    (if (app (var g) (car (var xs)))
                                        (cons (car (var xs)) (var tail))
                                        (var tail)))))))

; foldl g []

(def-advanced-list-function foldrEmptyf
                            (make-program foldrf ,(parser '(lambda (g xs) (foldr g () xs)))))

(def-advanced-list-function foldlEmptyf
                            (make-program foldlf ,(parser '(lambda (g xs) (foldl g () xs)))))

(def-advanced-list-function foldr0f (make-program foldrf ,(parser '(lambda (g xs) (foldr g 0 xs)))))

(def-advanced-list-function foldl0f (make-program foldlf ,(parser '(lambda (g xs) (foldl g 0 xs)))))

(define (run-test)
  (test
   "cons"
   (run 1 (q) (evalo (make-program consf ,(apps (var cons) (num ,(build-num 1)) ,(list-c 2 3))) q))
   `(,(list-v 1 2 3)))
  (test
   "compose"
   (run 1
        (q)
        (evalo (make-program
                composef
                ,(apps (var compose) (lam x (car (var x))) (lam x (cdr (var x))) ,(list-c 1 2 3)))
               q))
   `(,(build-num 2)))
  (test "foldl"
        (run 1
             (q)
             (evalo (make-program foldlf
                                  flipf
                                  consf
                                  ,(apps (var foldl) (app (var flip) (var cons)) () ,(list-c 1 2 3)))
                    q))
        `(,(list-v 3 2 1)))
  (test
   "foldr"
   (run 1 (q) (evalo (make-program foldrf consf ,(apps (var foldr) (var cons) () ,(list-c 1 2 3))) q))
   `(,(list-v 1 2 3)))
  (test "map"
        (run 1
             (q)
             (evalo (make-program mapf ,(apps (var map) (lam x (cons (var x) ())) ,(list-c 1 2 3)))
                    q))
        `(,(list-v '(1) '(2) '(3))))
  (test "scanl"
        (run 1
             (q)
             (evalo (make-program
                     scanlf
                     ,(apps (var scanl) (lam x (lam y ((var x) + (var y)))) (num ()) ,(list-c 1 2 3)))
                    q))
        `(,(list-v 1 3 6)))
  (test "append"
        (run 1 (q) (evalo (make-program appendf ,(apps (var append) ,(list-c 1 2) ,(list-c 3 4))) q))
        `(,(list-v 1 2 3 4)))
  (test "concat"
        (run 1 (q) (evalo (make-program concatf ,(apps (var concat) ,(list-c '(1) '(2)))) q))
        `(,(list-v 1 2)))
  (test "length"
        (run 1 (q) (evalo (make-program lengthf (app (var length) ,(list-c 1 2 3))) q))
        `(,(build-num 3)))
  (test "filter"
        (run 1
             (q)
             (evalo (make-program
                     filterf
                     ,(apps (var filter) (lam x ((var x) = (num ()))) ,(list-c 1 0 0 1 0 1 1 0)))
                    q))
        `(,(list-v 0 0 0 0)))
  (test "foldrEmpty"
        (run 1
             (q)
             (evalo (make-program
                     foldrEmptyf
                     ,(apps (var foldrEmpty) (lam x (lam y (cons (var x) (var y)))) ,(list-c 1 2)))
                    q))
        `(,(list-v 1 2))))
