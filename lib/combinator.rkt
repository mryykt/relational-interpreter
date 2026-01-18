#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
(require "functions.rkt")
(require "helper.rkt")
(require "test-check.rkt")
(require "parse.rkt")
(require "example.rkt")

(provide translateo)

(defrel (translateo exp)
        (matche exp
                [(app ,e1 ,e2) (translateo e1) (translateo e2)]
                [(var ,v) (symbolo v)]
                [()]
                [(num ())]
                [(num (1))]))

(define (run-synthesis)
  (define (appi e is)
    (if (null? is)
        e
        (appi `(app ,e ,(car is)) (cdr is))))
  (define (f name e fs _ i o)
    (test
     name
     (let ([env (append fs all-basic-functions)])
       (map
        unparser
        (run 1
             (q)
             (fresh (exp) (== exp (appi q i)) (translateo q) (evalo (with-functions env exp) o)))))
     `(,e)))
  (for-each (lambda (example) (apply f example)) examples))
