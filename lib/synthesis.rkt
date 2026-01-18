#lang racket
(require minikanren)
(require minikanren/matche)
(require "functions.rkt")
(require "constraint.rkt")
(require "test-check.rkt")
(require "parse.rkt")
(require (prefix-in simply: "type-inference.rkt"))
(require (prefix-in poly: "ml-type-inference.rkt"))
(require "combinator.rkt")
(require (prefix-in big-step: "big-step.rkt"))
(require (prefix-in small-step: "small-step.rkt"))
(require "example.rkt")
(require "utils.rkt")

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (simply:typedo body '() t) (== nt `(,name . ,t)))]))

(define (appi e is)
  (if (null? is)
      e
      (appi `(app ,e ,(car is)) (cdr is))))

(define (synthesis n evalo rules)
  (displayln n)
  (define (f name e fs t i o)
    (test name
          (let ([env (append fs all-basic-functions)])
            (map unparser
                 (run 1
                      (q)
                      (fresh (exp)
                             (== exp (appi q i))
                             (translateo q)
                             (rules exp env o t)
                             (evalo (with-functions env exp) o)))))
          `(,e)))
  (for-each (lambda (example) (apply f example)) examples))

(define (no-rules _exp _env _o _t)
  (== #t #t))

(define (with-types typedo)
  (lambda (exp env _o t) (fresh (tenv) (mapo typed-helpero env tenv) (typedo exp tenv t))))

(define (with-types-and-deduction typedo)
  (lambda (exp env o t)
    (fresh (tenv) (mapo typed-helpero env tenv) (constrainto exp tenv t o) (typedo exp tenv t))))

(define (run-synthesis)
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
                             (simply:typedo exp tenv t)
                             (constrainto exp tenv t o)
                             (big-step:evalo (with-functions env exp) o)))))
          `(,e)))
  (for-each (lambda (example) (apply f example)) examples))

(define (run-test)
  (synthesis "big-step" big-step:evalo no-rules)
  (synthesis "small-step" small-step:evalo no-rules)
  (synthesis "+simply type" big-step:evalo (with-types simply:typedo))
  (synthesis "+polymorphic type" big-step:evalo (with-types poly:typedo))
  (synthesis "+simply type + deduction rule" big-step:evalo (with-types-and-deduction simply:typedo)))
