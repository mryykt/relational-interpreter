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

(defrel (translateo src dst)
        (matche src
                [(,u ,v) (fresh (a b) (translateo u a) (translateo v b) (== dst `(app ,a ,b)))]
                [,u (symbolo u) (== dst `(var ,u))]
                [() (== dst '(list ()))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) (input ...) output)
     (run
      n
      (q)
      (fresh (env dst) (translateo q dst) (evalo (with-all-functions (apps ,dst input ...)) output)))]
    [(_ n (q) (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (dst)
                   (translateo q dst)
                   (evalo (with-functions env (apps ,dst input ...)) output))))]))

(define (run-test)
  (test "reverse"
        (synthesis 1 (q) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
        '(((foldl (flip cons)) ())))
  (test "append"
        (synthesis 1 (q) (foldrf consf flipf) (,(list-c 1 2) ,(list-c 3 4)) (list-v 1 2 3 4))
        '((flip (foldr cons))))
  (test "concat"
        (synthesis 1 (q) (foldrf foldlf flipf consf) (,(list-c '(1 2) '(3 4))) (list-v 1 2 3 4))
        '(((foldl (flip (foldr cons))) ())))
  (let ([+f '(lam x (lam y ((var x) + (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (synthesis 1 (q) (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
          '(((foldl +) (|0| ())))))
  (test "isort"
        (synthesis 1 (q) (noEmptyf sortHelperf ltf fromHeadf) (,(list-c 3 1 2)) (list-v 1 2 3))
        '((fromHead (noEmpty (sortHelper lt))))))
