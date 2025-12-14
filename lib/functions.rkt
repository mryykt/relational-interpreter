#lang racket
(require syntax/parse/define)
(require minikanren)
(require minikanren/numbers)
(require "big-step.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "type-inference.rkt")

(provide (all-defined-out))

; util
(define all-functions-list '())
(define all-basic-functions '())
(define all-list-functions '())
(define all-advanced-list-functions '())

(define-syntax def-basic-function
  (syntax-rules ()
    [(_ name expr)
     (begin
       (set! all-functions-list (cons (cons (symbol-trim-last 'name) expr) all-functions-list))
       (set! all-basic-functions (cons (cons (symbol-trim-last 'name) expr) all-basic-functions))
       (define name expr))]))

(define-syntax def-list-function
  (syntax-rules ()
    [(_ name expr)
     (begin
       (set! all-functions-list (cons (cons (symbol-trim-last 'name) expr) all-functions-list))
       (set! all-list-functions (cons (cons (symbol-trim-last 'name) expr) all-list-functions))
       (define name expr))]))

(define-syntax def-advanced-list-function
  (syntax-rules ()
    [(_ name expr)
     (begin
       (set! all-functions-list (cons (cons (symbol-trim-last 'name) expr) all-functions-list))
       (set! all-advanced-list-functions
             (cons (cons (symbol-trim-last 'name) expr) all-advanced-list-functions))
       (define name expr))]))

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
  (with-functions
   `((foldl . ,foldlf) (flip . ,flipf) (cons . ,consf) (concat . ,concatf) (append . ,appendf))
   c))

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
; basic combinator

(def-basic-function flipf '(lam f (lam x (lam y (app (app (var f) (var y)) (var x))))))

(def-basic-function composef '(lam f (lam g (lam x (app (var f) (app (var g) (var x)))))))

; list functions
(def-list-function foldlf
                   '(fix f
                         g
                         (lam acc
                              (lam xs
                                   (if ((var xs) = ())
                                       (var acc)
                                       (app (app (app (var f) (var g))
                                                 (app (app (var g) (var acc)) (car (var xs))))
                                            (cdr (var xs))))))))

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

(def-list-function
 appendf
 `(let foldr
    ,foldrf
    (lam xs (lam ys ,(apps (var foldr) (lam x (lam y (cons (var x) (var y)))) (var ys) (var xs))))))

(def-list-function concatf (make-program foldlf appendf ,(apps (var foldl) (var append) ())))

(def-list-function lengthf
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

; advanced list functions
(def-advanced-list-function
 mergef
 (make-program concatf
               mapf
               lengthf
               filterf
               notf
               (fix f
                    g
                    (lam xs
                         (if ((var xs) = ())
                             ()
                             (let temp ,(apps (var filter)
                                              (lam x (app (var not) ((var x) = ())))
                                              (app [var g] [var xs]))
                               (if ((app (var length) (var temp)) = (num (1)))
                                   (car (var temp))
                                   (app (var concat)
                                        ,(apps (var map) (app (var f) (var g)) (var temp))))))))))

(def-advanced-list-function
 fromHeadf
 `(fix f
       g
       (lam xs
            (if ((var xs) = ())
                ()
                ,(apps (var g) (car (var xs)) ,(apps (var f) (var g) (cdr (var xs))))))))

(def-advanced-list-function
 noEmptyf
 `(fix f
       g
       (lam x
            (lam xs
                 (if ((var xs) = ())
                     (cons (var x) ())
                     ,(apps (var g) (app (var f) (var g)) (var x) (car (var xs)) (cdr (var xs))))))))

(def-advanced-list-function
 sortHelperf
 `(lam g
       (lam h
            (lam x
                 (lam y
                      (lam ys
                           (if ,(apps (var g) (var x) (var y))
                               (cons (var x) (cons (var y) (var ys)))
                               (cons (var y) ,(apps (var h) (var x) (var ys))))))))))

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
  (test
   "merge"
   (run 1
        (q)
        (evalo (make-program mergef
                             ,(apps (var merge)
                                    (lam xs (cons (cons (car (var xs)) ()) (cons (cdr (var xs)) ())))
                                    ,(list-c 1 2 3)))
               q))
   `(,(list-v 1 2 3)))
  (test "qsort"
        (run 1
             (q)
             (evalo (make-program
                     mergef
                     notf
                     filterf
                     ,(apps (var merge)
                            (lam xs
                                 (cons ,(apps (var filter)
                                              (lam x (app (var not) ((car (var xs)) < (var x))))
                                              (cdr (var xs)))
                                       (cons (cons (car (var xs)) ())
                                             (cons ,(apps (var filter)
                                                          (lam x ((car (var xs)) < (var x)))
                                                          (cdr (var xs)))
                                                   ()))))
                            ,(list-c 3 1 2 6 7 4 5)))
                    q))
        `(,(list-v 1 2 3 4 5 6 7)))
  (test "fromHead"
        (run 1
             (q)
             (evalo (make-program
                     fromHeadf
                     ,(apps (var fromHead) (lam x (lam y (cons (var x) (var y)))) ,(list-c 1 2)))
                    q))
        `(,(list-v 1 2)))
  (test "insert_sort"
        (run 1
             (q)
             (evalo (make-program fromHeadf
                                  noEmptyf
                                  sortHelperf
                                  ltf
                                  ,(apps (var fromHead)
                                         (app (var noEmpty) (app (var sortHelper) (var lt)))
                                         ,(list-c 3 1 2 6 7 4 5)))
                    q))
        `(,(list-v 1 2 3 4 5 6 7))))
