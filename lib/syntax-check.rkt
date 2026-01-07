#lang racket
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide syntaxo)

(defrel (numo n) (matche n [()] [(0 . ,n^) (=/= n^ '()) (numo n^)] [(1 . ,n^) (numo n^)]))

(defrel (charo c) (membero c (map integer->char (range 0 128))))

(defrel (syntaxo exp)
        (matche exp
                [(num ,n) (numo n)]
                [(char ,c) (charo c)]
                [true]
                [false]
                [()]
                [(var ,x) (symbolo x)]
                [(lam ,x ,u) (symbolo x) (syntaxo u)]
                [(fix ,f ,x ,u) (symbolo f) (symbolo x) (syntaxo u)]
                [(app ,f ,u) (syntaxo f) (syntaxo u)]
                [(let ,x
                   ,e1
                   ,e2)
                 (syntaxo e1)
                 (syntaxo e2)]
                [(if ,e ,u ,v) (syntaxo e) (syntaxo u) (syntaxo v)]
                [(,u = ,v) (fresh (a) (syntaxo u) (syntaxo v))]
                [(,u < ,v) (syntaxo u) (syntaxo v)]
                [(,u + ,v) (syntaxo u) (syntaxo v)]
                [(,u - ,v) (syntaxo u) (syntaxo v)]
                [(,u * ,v) (syntaxo u) (syntaxo v)]
                [(cons ,ca ,cd) (syntaxo ca) (syntaxo cd)]
                [(car ,ls) (syntaxo ls)]
                [(cdr ,ls) (syntaxo ls)]))
