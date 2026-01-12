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
        (matche exp
                [(app ,e1 ,e2) (translateo e1) (translateo e2)]
                [(var ,v) (symbolo v)]
                [()]
                [(num ())]
                [(num (1))]))

(defrel (constrainto exp env t v)
        (matche (exp t)
                [(,_ int)]
                [(,_ bool)]
                [(,_ char)]
                [(,_ (,_a -> ,_b))]
                [((app (app (var cons) ,_a) ,d) (list ,_t))
                 (fresh (n n^ v^) (evalo d v^) (lengtho v^ n^) (lengtho v n) (inco n^ n))]
                [((app (app (app (var flip) ,f) ,x) ,y) (list ,_t))
                 (constrainto `(app (app ,f ,y) ,x) env t v)]
                [((app (app (app (app (var compose) ,f) ,g) ,x) ,l) (list ,_t))
                 (constrainto `(app (app ,f (app ,g ,x)) ,l) env t v)]
                [((app (app (app (var compose) ,f) ,g) ,x) ,t)
                 (fresh (t^)
                        (typedo `(app ,f ,x) env t^)
                        (conde [(== t t^)
                                (conde [(constrainto `(app ,g ,x) env t v)]
                                       [(constrainto `(app ,f ,x) env t v)])]
                               [(=/= t t^)]))]
                [((app (app (app (app (var fork) ,f) ,g) ,x) ,y) ,t)
                 (fresh (t^)
                        (typedo `(app (app ,g ,x) ,y) env t^)
                        (typedo y '() t^)
                        (conde [(== t t^)
                                (conde [(constrainto `(app (app ,g ,x) ,y) env t v)]
                                       [(constrainto `(app ,f ,y) env t v)])]
                               [(=/= t t^)]))]
                [((app (app (app (var foldl) ,f) ,acc) (cons ,a ,d)) (list ,t^))
                 (fresh (t^^) (typedo a '() t^^) (type-depth-leo t^^ t^))
                 (fresh (v^ v^^ l)
                        (evalo acc v^)
                        (lengtho v^ l)
                        (dropo l v v^^)
                        (constrainto `(app (app ,f ,d) ,a) env t v^^))]
                [((app (app (app (var foldr) ,f) ,acc) (cons ,a ,d)) (list ,t^))
                 (fresh (t^^) (typedo a '() t^^) (type-depth-leo t^^ t^))
                 (fresh (v^ v^^ l)
                        (evalo acc v^)
                        (lengtho v^ l)
                        (dropo l v v^^)
                        (constrainto `(app (app ,f ,a) ,d) env t v^^))]
                [((app (app (var map) ,_g) ,l) (list ,_t^))
                 (fresh (v^ n) (evalo l v^) (lengtho v^ n) (lengtho v n))]
                [((app (app (var filter) ,_p) ,l) (list ,_t^))
                 (fresh (v^ n m) (evalo l v^) (lengtho v^ n) (lengtho v m) (<o m n))]
                [((app (app (var foldlEmpty) ,f) (cons ,a ,d)) (list ,t^))
                 (fresh (t^^) (typedo a '() t^^) (type-depth-leo t^^ t^))
                 (constrainto `(app (app ,f ,d) ,a) env t v)]
                [((app (app (var foldlEmpty) ,_f) (cons ,a ,_d)) (list ,t^))
                 (symbolo t^)
                 (fresh (t^^) (typedo a '() `(list ,t^^)) (type-depth-leo t^^ t^))]
                [((app (app (var foldrEmpty) ,f) (cons ,a ,d)) (list ,t^))
                 (fresh (t^^) (typedo a '() t^^) (type-depth-leo t^^ t^))
                 (constrainto `(app (app ,f ,a) ,d) env t v)]
                [((app (app (var foldrEmpty) ,_f) (cons ,a ,_d)) (list ,t^))
                 (symbolo t^)
                 (fresh (t^^) (typedo a '() `(list ,t^^)) (type-depth-leo t^^ t^))]))

(defmatche (type-depth-leo t1 t2)
           [(,t1 ,t2) (membero t2 '(int char bool))]
           [((list ,t1^) (list ,t2^)) (type-depth-leo t1^ t2^)]
           [((,t11 -> ,t12) (,t21 -> ,t22)) (type-depth-leo t11 t21) (type-depth-leo t12 t22)])

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
                      (constrainto exp tenv t output)
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
                        (constrainto exp tenv t output)
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
