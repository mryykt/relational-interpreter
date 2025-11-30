#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "interpreter.rkt")
(require "functions.rkt")
(require "helper.rkt")
(require "type-inference.rkt")
(require "test-check.rkt")
(require "utils.rkt")

(defrel
 (translateo src dst env t)
 (matche
  src
  [(,u ,v)
   (fresh (a b t0) (translateo u a env `(fun ,t0 ,t)) (translateo v b env t0) (== dst `(app ,a ,b)))]
  [,u (symbolo u) (lookupo u env t) (== dst `(var ,u))]
  [() (fresh (et) (== t `(list ,et)) (== dst '(list ())))]))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define-syntax combinator-helper
  (syntax-rules ()
    [(_ program (input)) `(app program input)]
    [(_ program (input1 input2 ...)) (combinator-helper (app program input1) (input2 ...))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) t (input ...) output)
     (run n
          (q)
          (fresh (env tenv dst)
                 (mapo typed-helpero all-functions-list tenv)
                 (translateo q dst tenv t)
                 (evalo (with-all-functions (combinator-helper ,dst (input ...))) output)))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (tenv dst)
                   (mapo typed-helpero env tenv)
                   (translateo q dst tenv t)
                   (evalo (with-functions env (combinator-helper ,dst (input ...))) output))))]))

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
  (let ([+f '(lam x (lam y (+ (var x) (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (synthesis 1 (q) '(fun (list int) int) (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
          '(((foldl +) (|0| ()))))))
