#lang racket
(require syntax/parse/define)
(require minikanren)
(require minikanren/numbers)
(require "interpreter.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "type-inference.rkt")

(provide consf
         flipf
         composef
         foldlf
         foldrf
         mapf
         scanlf
         make-program
         run-test)

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

; basic function
(define orf '(lam x (lam y (ifz (var x) (num ()) (= (var y) (num ()))))))

(define andf '(lam x (lam y (ifz (var x) (= (var y) (num ())) (num (1))))))

(define notf '(lam x (ifz (var x) (num (1)) (num ()))))

(define consf '(lam x (lam y (cons (var x) (var y)))))
; basic combinator

(define flipf '(lam f (lam x (lam y (app (app (var f) (var y)) (var x))))))

(define composef '(lam f (lam g (lam x (app (var f) (app (var g) (var x)))))))

(define foldlf
  '(fix f
        g
        (lam acc
             (lam xs
                  (ifz (= (var xs) (list ()))
                       (var acc)
                       (app (app (app (var f) (var g)) (app (app (var g) (var acc)) (car (var xs))))
                            (cdr (var xs))))))))

(define foldrf
  '(fix f
        g
        (lam init
             (lam xs
                  (ifz (= (var xs) (list ()))
                       (var init)
                       (app (app (var g) (car (var xs)))
                            (app (app (app (var f) (var g)) (var init)) (cdr (var xs)))))))))

(define mapf
  '(fix f
        g
        (lam xs
             (ifz (= (var xs) (list ()))
                  (list ())
                  (cons (app (var g) (car (var xs))) (app (app (var f) (var g)) (cdr (var xs))))))))

(define scanlf
  '(fix f
        g
        (lam acc
             (lam xs
                  (ifz (= (var xs) (list ()))
                       (list ())
                       (let acc2 (app
                                  [app
                                   (var g)
                                   (car (var xs))]
                                  [var acc])
                         (cons (var acc2)
                               (app (app (app (var f) (var g)) (var acc2)) (cdr (var xs))))))))))

(define appendf
  `(let foldr
     ,foldrf
     (lam xs (lam ys ,(apps (var foldr) (lam x (lam y (cons (var x) (var y)))) (var ys) (var xs))))))

(define concatf (make-program foldlf appendf ,(apps (var foldl) (var append) (list ()))))

(define lengthf
  '(fix f xs (ifz (= (var xs) (list ())) (num ()) (+ (num (1)) (app (var f) (cdr (var xs)))))))

(define filterf
  `(fix
    f
    g
    (lam xs
         (ifz (= (var xs) (list ()))
              (list ())
              (let tail ,(apps (var f) (var g) (cdr (var xs)))
                (ifz (app (var g) (car (var xs))) (cons (car (var xs)) (var tail)) (var tail)))))))

; advanced combinator
(define mergef
  (make-program concatf
                mapf
                lengthf
                filterf
                notf
                (fix f
                     g
                     (lam xs
                          (ifz (= (var xs) (list ()))
                               (list ())
                               (let temp ,(apps (var filter)
                                                (lam x (app (var not) (= (var x) (list ()))))
                                                (app [var g] [var xs]))
                                 (ifz (= (app (var length) (var temp)) (num (1)))
                                      (car (var temp))
                                      (app (var concat)
                                           ,(apps (var map) (app (var f) (var g)) (var temp))))))))))

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
             (evalo
              (make-program foldlf
                            flipf
                            consf
                            ,(apps (var foldl) (app (var flip) (var cons)) (list ()) ,(list-c 1 2 3)))
              q))
        `(,(list-v 3 2 1)))
  (test
   "foldr"
   (run 1
        (q)
        (evalo (make-program foldrf consf ,(apps (var foldr) (var cons) (list ()) ,(list-c 1 2 3)))
               q))
   `(,(list-v 1 2 3)))
  (test "map"
        (run 1
             (q)
             (evalo (make-program mapf
                                  ,(apps (var map) (lam x (cons (var x) (list ()))) ,(list-c 1 2 3)))
                    q))
        `(,(list-v '(1) '(2) '(3))))
  (test "scanl"
        (run 1
             (q)
             (evalo (make-program
                     scanlf
                     ,(apps (var scanl) (lam x (lam y (+ (var x) (var y)))) (num ()) ,(list-c 1 2 3)))
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
                     ,(apps (var filter) (lam x (= (var x) (num ()))) ,(list-c 1 0 0 1 0 1 1 0)))
                    q))
        `(,(list-v 0 0 0 0)))
  (test "merge"
        (run 1
             (q)
             (evalo (make-program mergef
                                  ,(apps (var merge)
                                         (lam xs (list ((list ((car (var xs)))) (cdr (var xs)))))
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
                                 (list (,(apps (var filter)
                                               (lam x (app (var not) (< (car (var xs)) (var x))))
                                               (cdr (var xs)))
                                        (list ((car (var xs))))
                                        ,(apps (var filter)
                                               (lam x (< (car (var xs)) (var x)))
                                               (cdr (var xs))))))
                            ,(list-c 3 1 2 6 7 4 5)))
                    q))
        `(,(list-v 1 2 3 4 5 6 7))))
