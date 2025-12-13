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

(provide synthesis
         run-test)

(defrel (translateo src dst env env^ t)
        (matche src
                [(,u ,v)
                 (fresh (a b t0 env^^)
                        (translateo u a env env^^ `(fun ,t0 ,t))
                        (translateo v b env^^ env^ t0)
                        (== dst `(app ,a ,b)))]
                [,u (symbolo u) (lookupo u env t) (== dst `(var ,u)) (rembero u env env^)]
                [() (fresh (et) (== t `(list ,et)) (== dst '(list ())) (== env env^))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) t (input ...) output)
     (run n
          (q)
          (fresh (env tenv dst res)
                 (mapo typed-helpero all-functions-list tenv)
                 (translateo q dst tenv res t)
                 (evalo (with-all-functions (apps ,dst input ...)) output)))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (tenv dst res)
                   (mapo typed-helpero env tenv)
                   (translateo q dst tenv res t)
                   (evalo (with-functions env (apps ,dst input ...)) output))))]))

(define (run-test)
  (test
   "reverse"
   (synthesis 1 (q) '(fun (list int) (list int)) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
   '(((foldl (flip cons)) ())))
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
        '(((foldl (flip (foldr cons))) ())))
  (let ([+f '(lam x (lam y ((var x) + (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (synthesis 1 (q) '(fun (list int) int) (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
          '(((foldl +) (|0| ())))))
  (test "isort"
        (synthesis 1
                   (q)
                   '(fun (list int) (list int))
                   (noEmptyf sortHelperf ltf fromHeadf)
                   (,(list-c 3 1 2))
                   (list-v 1 2 3))
        '((fromHead (noEmpty (sortHelper lt))))))
