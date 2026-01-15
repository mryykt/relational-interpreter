#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
(require "functions.rkt")
(require "helper.rkt")
; (require "type-inference.rkt")
(require "ml-type-inference.rkt")
(require "test-check.rkt")
(require "utils.rkt")
(require "parse.rkt")
(require "example.rkt")

(defrel (translateo exp)
        (matche exp
                [(app ,e1 ,e2) (translateo e1) (translateo e2)]
                [(var ,v) (symbolo v)]
                [()]
                [(num ())]
                [(num (1))]))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define (run-synthesis)
  (define (appi e is)
    (if (null? is)
        e
        (appi `(app ,e ,(car is)) (cdr is))))
  (define (f name e fs t i o)
    (test name
          (let ([env (append fs all-basic-functions)])
            (map unparser
                 (run 1
                      (q)
                      (fresh (tenv exp)
                             (mapo typed-helpero env tenv)
                             (== exp (appi q i))
                             (translateo q)
                             (typedo exp tenv t)
                             (evalo (with-functions env exp) o)))))
          `(,e)))
  (for-each (lambda (example) (apply f example)) examples))
