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

(defmatche (translateo src dst)
           [((,f ,x) (app ,f^ ,x^)) (translateo f f^) (translateo x x^)]
           [(,v (var ,v)) (symbolo v)]
           [(() (list ()))])

(define-syntax synthesis
  (syntax-rules ()
    [(_ n (q) t (input ...) output)
     (run n
          (q)
          (fresh (env tenv dst)
                 (mapo typed-helpero all-functions-list tenv)
                 (translateo q dst)
                 (typedo (apps ,dst input ...) tenv t)
                 (evalo (with-all-functions (apps ,dst input ...)) output)))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (tenv dst)
                   (mapo typed-helpero env tenv)
                   (translateo q dst)
                   (typedo (apps ,dst input ...) tenv t)
                   (evalo (with-functions env (apps ,dst input ...)) output))))]))

(define (run-test)
  (test "reverse"
        (synthesis 1 (q) '(list int) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1))
        '(((foldl (flip cons)) ())))
  (test
   "append"
   (synthesis 1 (q) '(list int) (foldrf consf flipf) (,(list-c 1 2) ,(list-c 3 4)) (list-v 1 2 3 4))
   '((flip (foldr cons))))
  (test "concat"
        (synthesis 1
                   (q)
                   '(list int)
                   (foldrf foldlf flipf consf)
                   (,(list-c '(1 2) '(3 4)))
                   (list-v 1 2 3 4))
        '(((foldl (flip (foldr cons))) ())))
  (let ([+f '(lam x (lam y (+ (var x) (var y))))]
        [0f '(num ())])
    (test "sum"
          (synthesis 1 (q) 'int (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
          '(((foldl +) |0|)))))
