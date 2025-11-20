#lang racket
(require syntax/parse/define)
(require minikanren)
(require minikanren/numbers)
(require "interpreter.rkt")
(require "test-check.rkt")
(require "helper.rkt")

(provide consf
         flipf
         composef
         foldlf
         foldrf
         mapf
         scanlf
         make-program
         run-test)

(define consf '(lam x (lam y (cons (var x) (var y)))))

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

(define (symbol-trim-last sym)
  (let* ([s (symbol->string sym)]
         [trimmed (substring s 0 (max 0 (sub1 (string-length s))))])
    (string->symbol trimmed)))

(define-syntax make-program
  (syntax-rules ()
    [(_ c) c]
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
        `(,(list-v 1 3 6))))
