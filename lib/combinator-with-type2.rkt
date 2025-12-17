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

(defrel (combinator src env t)
        (matche src
                [(app ,u ,v) (fresh (t0) (combinator u env `(fun ,t0 ,t)) (combinator v env t0))]
                [(var ,u) (symbolo u) (lookup2o u env t)]
                [(num ()) (== t 'int)]
                [(num (1)) (== t 'int)]
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
     (let ([env (append `((,(symbol-trim-last 'function) . ,function) ...) all-basic-functions)])
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
   (synthesis 1 (q) '(fun (list char) (list char)) (foldlf) (,(string-c "hello")) (string-v "olleh"))
   '((foldl (flip cons) ())))
  (test "append"
        (synthesis 1
                   (q)
                   '(fun (list char) (fun (list char) (list char)))
                   (foldrf)
                   (,(string-c "hello ") ,(string-c "world"))
                   (string-v "hello world"))
        '((flip (foldr cons))))
  ;   (test "concat"
  ;         (synthesis 1
  ;                    (q)
  ;                    '(fun (list (list int)) (list int))
  ;                    (foldrf foldlf)
  ;                    (,(list-c '(1 2) '(3 4)))
  ;                    (list-v 1 2 3 4))
  ;         '((foldl (flip (foldr cons)) ())))
  (test "sum"
        (synthesis 1 (q) '(fun (list int) int) (foldlf) (,(list-c 1 2 3)) (build-num 6))
        '((foldl add 0)))
  (test "isort"
        (synthesis 1
                   (q)
                   '(fun (list int) (list int))
                   (noEmptyf sortHelperf fromHeadf)
                   (,(list-c 3 1 2))
                   (list-v 1 2 3))
        '((fromHead (noEmpty (sortHelper lt)))))
  (test "adds"
        (synthesis 1
                   (q)
                   '(fun int (fun (list int) (list int)))
                   (mapf)
                   ((num ,(build-num 5)) ,(list-c 1 2 3))
                   (list-v 6 7 8))
        '((compose map add)))
  (test "length"
        (synthesis 1 (q) '(fun (list char) int) (foldrf) (,(string-c "123")) (build-num 3))
        '((foldr (const (add 1)) 0)))
  (test "rember"
        (synthesis 1
                   (q)
                   '(fun char (fun (list char) (list char)))
                   (filterf)
                   ((char #\o) ,(string-c "hello"))
                   (string-v "hell"))
        '((compose filter neq)))
  (test "maximize"
        (synthesis 1 (q) '(fun (list int) int) (foldr1f) (,(list-c 1 2 3 2 1)) (build-num 3))
        '((foldr1 max)))
  (test "minimize"
        (synthesis 1 (q) '(fun (list int) int) (foldr1f) (,(list-c 3 2 5 2 3)) (build-num 2))
        '((foldr1 min)))
  (test "uniq"
        (synthesis 1
                   (q)
                   '(fun (list char) (list char))
                   (foldrf filterf)
                   (,(string-c "aaabbc"))
                   (string-v "abc"))
        '()))
