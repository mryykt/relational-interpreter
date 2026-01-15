#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
(require "functions.rkt")
(require "helper.rkt")
; (require "type-inference.rkt")
(require "ml-type-inference.rkt")
(require "test-check.rkt")
(require "utils.rkt")
(require "parse.rkt")

(defrel (translateo exp)
        (matche exp
                [(app ,e1 ,e2) (translateo e1) (translateo e2)]
                [(var ,v) (symbolo v)]
                [()]
                [(num ())]
                [(num (1))]))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define-syntax synthesis
  (syntax-rules ()
    [(_ t (input ...) output)
     (map unparser
          (run 1
               (q)
               (fresh (env tenv exp)
                      (mapo typed-helpero all-functions-list tenv)
                      (== exp (apps ,q input ...))
                      (translateo q)
                      (typedo exp tenv t)
                      (evalo (with-all-functions exp) output))))]
    [(_ t (function ...) (input ...) output)
     (let ([env (append `((,(symbol-trim-last 'function) . ,function) ...) all-basic-functions)])
       (map unparser
            (run 1
                 (q)
                 (fresh (tenv exp)
                        (mapo typed-helpero env tenv)
                        (== exp (apps ,q input ...))
                        (translateo q)
                        (typedo exp tenv t)
                        (evalo (with-functions env exp) output)))))]))

(define (run-test)
  (test "reverse"
        (synthesis '(list char) (foldlEmptyf) (,(string-c "hello")) (string-v "olleh"))
        '((foldlEmpty (flip cons))))
  (test "append"
        (synthesis '(list char)
                   (foldrf)
                   (,(string-c "hello ") ,(string-c "world"))
                   (string-v "hello world"))
        '((flip (foldr cons))))
  (test "concat"
        (synthesis '(list char)
                   (foldrf foldrEmptyf)
                   (,(list-c "hello" " " "world"))
                   (string-v "hello world"))
        '((foldrEmpty (flip (foldr cons)))))
  (test "sum" (synthesis 'int (foldlf) (,(list-c 1 2 3)) (build-num 6)) '((foldl add 0)))
  (test "adds"
        (synthesis '(list int) (mapf) ((num ,(build-num 5)) ,(list-c 1 2 3)) (list-v 6 7 8))
        '((compose map add)))
  (test "length"
        (synthesis 'int (foldr0f) (,(string-c "123")) (build-num 3))
        '((foldr0 (const (add 1)))))
  (test "rember"
        (synthesis '(list char) (filterf) ((char #\o) ,(string-c "hello")) (string-v "hell"))
        '((compose filter neq)))
  (test "maximize" (synthesis 'int (foldr1f) (,(list-c 1 2 3 2 1)) (build-num 3)) '((foldr1 max)))
  (test "minimize" (synthesis 'int (foldr1f) (,(list-c 3 2 5 2 3)) (build-num 2)) '((foldr1 min)))
  (test "uniq"
        (synthesis '(list char) (foldrEmptyf filterf) (,(string-c "aaabbc")) (string-v "abc"))
        '((foldrEmpty (fork cons (compose filter neq)))))
  (test "last" (synthesis 'char (foldr1f) (,(string-c "hello")) '(char #\o)) '(foldr1 (flip const))))
