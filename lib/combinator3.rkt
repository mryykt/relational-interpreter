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

(provide synthesis
         run-test)

(defrel
 (combinator src env env^ t)
 (matche src
         [(app ,u ,v)
          (fresh (t0 env^^) (combinator u env env^^ `(fun ,t0 ,t)) (combinator v env^^ env^ t0))]
         [(var ,u) (symbolo u) (lookupo u env t) (rembero u env env^)]
         [(list ()) (fresh (et) (== t `(list ,et)) (== env env^))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) t (input ...) output)
     (run n
          (q)
          (fresh (env tenv res)
                 (mapo typed-helpero all-functions-list tenv)
                 (combinator q tenv res t)
                 (evalo (with-all-functions (apps ,q input ...)) output)))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (tenv res)
                   (mapo typed-helpero env tenv)
                   (combinator q tenv res t)
                   (evalo (with-functions env (apps ,q input ...)) output))))]))

(define (run-test)
  (test
   "reverse"
   (synthesis 1 (q) '(fun (list int) (list int)) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
   `(,(parser '(foldl (flip cons) (list)))))
  (test "append"
        (synthesis 1
                   (q)
                   '(fun (list int) (fun (list int) (list int)))
                   (foldrf consf flipf)
                   (,(list-c 1 2) ,(list-c 3 4))
                   (list-v 1 2 3 4))
        `(,(parser '(flip (foldr cons)))))
  (test "concat"
        (synthesis 1
                   (q)
                   '(fun (list (list int)) (list int))
                   (foldrf foldlf flipf consf)
                   (,(list-c '(1 2) '(3 4)))
                   (list-v 1 2 3 4))
        `(,(parser '(foldl (flip (foldr cons)) (list)))))
  (let ([addf '(lam x (lam y ((var x) + (var y))))]
        [0f '(num ())])
    (test "sum"
          (synthesis 1 (q) '(fun (list int) int) (foldlf addf 0f) (,(list-c 1 2 3)) (build-num 6))
          `(,(parser '(foldl add |0|)))))
  (test "isort"
        (synthesis 1
                   (q)
                   '(fun (list int) (list int))
                   (noEmptyf sortHelperf ltf fromHeadf)
                   (,(list-c 3 1 2))
                   (list-v 1 2 3))
        `(,(parser '(fromHead (noEmpty (sortHelper lt)))))))
