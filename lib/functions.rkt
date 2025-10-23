#lang racket
(require minikanren)
(require minikanren/numbers)
(require "j.rkt")
(require "test-check.rkt")
(require "helper.rkt")

(define consf '(lam x (lam y (cons (var x) (var y)))))

(define flipf '(lam f (lam x (lam y (app (app (var f) (var y)) (var x))))))

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

(define (run-test)
  (test "cons"
        (run 1
             (q)
             (evalo `(let cons
                       ,consf
                       (app (app (var cons) (num ,(build-num 1))) ,(list-c 2 3)))
                    q))
        `(,(list-v 1 2 3)))
  (test "foldl"
        (run 1
             (q)
             (evalo `(let foldl
                       ,foldlf
                       (let flip
                         ,flipf
                         (app (app (app (var foldl)
                                        (app (var flip) (lam x (lam y (cons (var x) (var y))))))
                                   (list ()))
                              ,(list-c 1 2 3))))
                    q))
        `(,(list-v 3 2 1)))
  (test "foldr"
        (run 1
             (q)
             (evalo `(let foldr
                       ,foldrf

                       (app (app (app (var foldr) (lam x (lam y (cons (var x) (var y))))) (list ()))
                            ,(list-c 1 2 3)))
                    q))
        `(,(list-v 1 2 3))))
