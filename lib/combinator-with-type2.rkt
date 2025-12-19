#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
; (require "small-step.rkt")
(require "functions.rkt")
(require "helper.rkt")
(require "type-inference.rkt")
(require "test-check.rkt")
(require "utils.rkt")
(require "parse.rkt")
(require "combinator.rkt")

(defrel (limitedo src env t)
        (matche src
                [(app ,u ,v) (fresh (t0) (limitedo u env `(fun ,t0 ,t)) (limitedo v env t0))]
                [(var ,u) (symbolo u) (lookup-firsto u env t)]
                [(num ()) (== t 'int)]
                [(num (1)) (== t 'int)]
                [() (fresh (et) (== t `(list ,et)))]))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ n t (input ...) output)
     (map unparser
          (run n
               (q)
               (fresh (env tenv)
                      (mapo typed-helpero all-functions-list tenv)
                      (limitedo q tenv t)
                      (evalo (with-all-functions (apps ,q input ...)) output))))]
    [(_ n t (function ...) (input ...) output)
     (let ([env (append `((,(symbol-trim-last 'function) . ,function) ...) all-basic-functions)])
       (map unparser
            (run n
                 (q)
                 (fresh (tenv)
                        (mapo typed-helpero env tenv)
                        (limitedo q tenv t)
                        (evalo (with-functions env (apps ,q input ...)) output)))))]))

(define (run-test)
  (test "last"
        (synthesis 1 '(fun (list char) char) (foldr1f) (,(string-c "hello")) '(char #\o))
        '(foldr1 (flip const)))
  (test
   "reverse"
   (synthesis 1 '(fun (list char) (list char)) (foldlEmptyf) (,(string-c "hello")) (string-v "olleh"))
   '((foldlEmpty (flip cons))))
  (test "append"
        (synthesis 1
                   '(fun (list char) (fun (list char) (list char)))
                   (foldrf)
                   (,(string-c "hello ") ,(string-c "world"))
                   (string-v "hello world"))
        '((flip (foldr cons))))
  (test "concat"
        (synthesis 1
                   '(fun (list (list char)) (list char))
                   (foldrf foldrEmptyf)
                   (,(list-c "hello" " " "world"))
                   (string-v "hello world"))
        '((foldrEmpty (flip (foldr cons)))))
  (test "sum"
        (synthesis 1 '(fun (list int) int) (foldlf) (,(list-c 1 2 3)) (build-num 6))
        '((foldl add 0)))
  (test "isort"
        (synthesis 1
                   '(fun (list int) (list int))
                   (noEmptyf sortHelperf foldrEmptyf)
                   (,(list-c 3 1 2))
                   (list-v 1 2 3))
        '((foldrEmpty (noEmpty (sortHelper lt)))))
  (test "adds"
        (synthesis 1
                   '(fun int (fun (list int) (list int)))
                   (mapf)
                   ((num ,(build-num 5)) ,(list-c 1 2 3))
                   (list-v 6 7 8))
        '((compose map add)))
  (test "length"
        (synthesis 1 '(fun (list char) int) (foldr0f) (,(string-c "123")) (build-num 3))
        '((foldr0 (const (add 1)))))
  (test "rember"
        (synthesis 1
                   '(fun char (fun (list char) (list char)))
                   (filterf)
                   ((char #\o) ,(string-c "hello"))
                   (string-v "hell"))
        '((compose filter neq)))
  (test "maximize"
        (synthesis 1 '(fun (list int) int) (foldr1f) (,(list-c 1 2 3 2 1)) (build-num 3))
        '((foldr1 max)))
  (test "minimize"
        (synthesis 1 '(fun (list int) int) (foldr1f) (,(list-c 3 2 5 2 3)) (build-num 2))
        '((foldr1 min)))
  (test "uniq"
        (synthesis 1
                   '(fun (list char) (list char))
                   (foldrEmptyf filterf)
                   (,(string-c "aaabbc"))
                   (string-v "abc"))
        '()))
