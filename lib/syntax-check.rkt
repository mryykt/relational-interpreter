#lang racket
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide syntax-checko)

(defrel (numo n) (matche n [()] [(0 . ,n^) (=/= n^ '()) (numo n^)] [(1 . ,n^) (numo n^)]))

(defrel (charo c) (membero c (map integer->char (range 0 127))))

(defrel (syntax-checko exp)
        (matche exp
                [(var ,x) (symbolo x)]
                [(app ,f ,u) (fresh (a) (syntax-checko f) (syntax-checko u))]
                [(lam ,x ,u) (syntax-checko u)]
                [(fix ,f ,x ,u) (syntax-checko u)]
                [(num ,n) (numo n)]
                [(char ,c) (charo c)]
                [true]
                [false]
                [(,u + ,v) (syntax-checko u) (syntax-checko v)]
                [(,u - ,v) (syntax-checko u) (syntax-checko v)]
                [(,u * ,v) (syntax-checko u) (syntax-checko v)]
                [(,u = ,v) (fresh (a) (syntax-checko u) (syntax-checko v))]
                [(,u < ,v) (syntax-checko u) (syntax-checko v)]
                [()]
                [(cons ,ca ,cd) (syntax-checko ca) (syntax-checko cd)]
                [(car ,ls) (syntax-checko ls)]
                [(cdr ,ls) (syntax-checko ls)]
                [(if ,e ,u ,v) (syntax-checko e) (syntax-checko u) (syntax-checko v)]
                [(let ,x
                   ,e1
                   ,e2)
                 (syntax-checko e1)
                 (syntax-checko e2)]))
