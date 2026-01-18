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
(require "example.rkt")

(provide constrainto)

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
                 (typedo a '() t^)
                 (fresh (v^ v^^ l)
                        (evalo acc v^)
                        (lengtho v^ l)
                        (dropo l v v^^)
                        (constrainto `(app (app ,f ,d) ,a) env t v^^))]
                [((app (app (app (var foldr) ,f) ,acc) (cons ,a ,d)) (list ,t^))
                 (typedo a '() t^)
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
                 (typedo a '() t^)
                 (constrainto `(app (app ,f ,d) ,a) env t v)]
                [((app (app (var foldlEmpty) ,_f) (cons ,a ,_d)) (list ,t^))
                 (symbolo t^)
                 (fresh (t^^) (typedo a '() `(list ,t^^)) (type-depth-leo t^^ t^))]
                [((app (app (var foldrEmpty) ,f) (cons ,a ,d)) (list ,t^))
                 (typedo a '() t^)
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

(define (run-synthesis)
  (define (appi e is)
    (if (null? is)
        e
        (appi `(app ,e ,(car is)) (cdr is))))
  (define (f name e fs t i o)
    (test name
          (let ([env (append fs all-basic-functions)])
            (map unparser
                 (run 1
                      (q)
                      (fresh (tenv exp)
                             (mapo typed-helpero env tenv)
                             (== exp (appi q i))
                             (translateo q)
                             (typedo exp tenv t)
                             (constrainto exp tenv t o)
                             (evalo (with-functions env exp) o)))))
          `(,e)))
  (for-each (lambda (example) (apply f example)) examples))
