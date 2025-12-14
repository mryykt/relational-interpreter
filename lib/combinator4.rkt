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

(defrel (translateo exp)
        (matche exp [(app ,u ,v) (translateo u) (translateo v)] [(var ,u) (symbolo u)] [()]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) (input ...) output)
     (run n (q) (fresh (env) (translateo q) (evalo (with-all-functions (apps ,q input ...)) output)))]
    [(_ n (q) (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n (q) (translateo q) (evalo (with-functions env (apps ,q input ...)) output)))]))

(define (run-test)
  (test "reverse"
        (synthesis 1 (q) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
        `(,(parser '(foldl (flip cons) ()))))
  (test "append"
        (synthesis 1 (q) (foldrf consf flipf) (,(list-c 1 2) ,(list-c 3 4)) (list-v 1 2 3 4))
        `(,(parser '(flip (foldr cons)))))
  (test "concat"
        (synthesis 1 (q) (foldrf foldlf flipf consf) (,(list-c '(1 2) '(3 4))) (list-v 1 2 3 4))
        `(,(parser '(foldl (flip (foldr cons)) ()))))
  (let ([addf '(lam x (lam y ((var x) + (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (synthesis 1 (q) (foldlf addf 0f) (,(list-c 1 2 3)) (build-num 6))
          `(,(parser '(foldl add (|0| ()))))))
  (test "isort"
        (synthesis 1 (q) (noEmptyf sortHelperf ltf fromHeadf) (,(list-c 3 1 2)) (list-v 1 2 3))
        `(,(parser '(fromHead (noEmpty (sortHelper lt)))))))
