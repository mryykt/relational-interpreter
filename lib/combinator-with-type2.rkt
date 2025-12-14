#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
(require "functions.rkt")
(require "helper.rkt")
(require "type-inference.rkt")
(require "test-check.rkt")
(require "utils.rkt")
(require "parse.rkt")

(defrel (combinator src env t)
        (matche src
                [(app ,u ,v) (fresh (t0) (combinator u env `(fun ,t0 ,t)) (combinator v env t0))]
                [(var ,u) (symbolo u) (lookupo u env t)]
                [() (fresh (et) (== t `(list ,et)))]))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) t (input ...) output)
     (map unparser
          (run n
               (q)
               (fresh (env tenv)
                      (mapo typed-helpero all-functions-list tenv)
                      (combinator q tenv t)
                      (evalo (with-all-functions (apps ,q input ...)) output))))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (map unparser
            (run n
                 (q)
                 (fresh (tenv)
                        (mapo typed-helpero env tenv)
                        (combinator q tenv t)
                        (evalo (with-functions env (apps ,q input ...)) output)))))]))

(define (run-test)
  (test
   "reverse"
   (synthesis 1 (q) '(fun (list int) (list int)) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
   '((foldl (flip cons) ())))
  (test "append"
        (synthesis 1
                   (q)
                   '(fun (list int) (fun (list int) (list int)))
                   (foldrf consf flipf)
                   (,(list-c 1 2) ,(list-c 3 4))
                   (list-v 1 2 3 4))
        '((flip (foldr cons))))
  (test "concat"
        (synthesis 1
                   (q)
                   '(fun (list (list int)) (list int))
                   (foldrf foldlf flipf consf)
                   (,(list-c '(1 2) '(3 4)))
                   (list-v 1 2 3 4))
        '((foldl (flip (foldr cons)) ())))
  (let ([+f '(lam x (lam y ((var x) + (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (synthesis 1 (q) '(fun (list int) int) (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
          '((foldl + (|0| ())))))
  (test "isort"
        (synthesis 1
                   (q)
                   '(fun (list int) (list int))
                   (noEmptyf sortHelperf ltf fromHeadf)
                   (,(list-c 3 1 2))
                   (list-v 1 2 3))
        '((fromHead (noEmpty (sortHelper lt))))))
