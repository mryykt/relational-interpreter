#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "big-step.rkt")
(require "utils.rkt")
(require "type-inference.rkt")
(require "test-check.rkt")
(require "functions.rkt")
(require "helper.rkt")

(defrel (outsideo exp env t)
        (matche exp
                [(app ,u ,v) (fresh (a) (outsideo u env `(fun ,a ,t)) (inside1o v '() a))]
                [(var ,x) (symbolo x) (lookup-firsto x env t)]
                [(list ()) (fresh (et) (== t `(list ,et)))]
                [(num ()) (== t 'int)]))

(defrel (inside1o exp env t)
        (matche exp
                [(lam ,x ,u)
                 (fresh (a b)
                        (conde ((fresh (c d) (== b `(fun ,c ,d))) (inside1o u `((,x . ,a) . ,env) b))
                               ((inside2o u `((,x . ,a) . ,env) b)))
                        (== t `(fun ,a ,b)))]
                [(num ()) (== t 'int)]
                [(list ()) (fresh (et) (== t `(list ,et)))]))

(defrel (inside2o exp env t)
        (matche exp
                [(var ,x) (symbolo x) (lookup-firsto x env t)]
                [(cons ,ca ,cd)
                 (fresh (t^) (inside4o ca env t^) (inside4o cd env `(list ,t^)) (== t `(list ,t^)))]
                [(car ,ls) (inside4o ls env `(list ,t))]
                [(cdr ,ls) (inside4o ls env t) (caro 'list t)]
                [(if ,e ,u ,v) (inside3o e env 'bool) (inside4o u env t) (inside2o v env t)]))

(defrel (inside3o exp env t)
        (matche exp
                [(var ,x) (symbolo x) (lookup-firsto x env t)]
                [(num ()) (== t 'int)]
                [(= ,u ,v) (fresh (a) (inside3o u env a) (inside3o v env a)) (== t 'bool)]
                [(< ,u ,v) (inside3o u env 'int) (inside3o v env 'int) (== t 'bool)]
                [(list ()) (fresh (et) (== t `(list ,et)))]
                [(car ,ls) (inside3o ls env `(list ,t))]
                [(cdr ,ls) (inside3o ls env t) (caro 'list t)]))

(defrel (inside4o exp env t)
        (matche exp
                [(var ,x) (symbolo x) (lookup-firsto x env t)]
                [(num ()) (== t 'int)]
                [(list ()) (fresh (et) (== t `(list ,et)))]
                [(cons ,ca ,cd)
                 (fresh (t^) (inside4o ca env t^) (inside4o cd env `(list ,t^)) (== t `(list ,t^)))]
                [(car ,ls) (inside4o ls env `(list ,t))]
                [(cdr ,ls) (inside4o ls env t) (caro 'list t)]))

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
          (fresh (env tenv)
                 (mapo typed-helpero all-functions-list tenv)
                 (outsideo q tenv t)
                 (evalo (with-all-functions (combinator-helper ,q (input ...))) output)))]
    [(_ n (q) t (function ...) (input ...) output)
     (let ([env `((,(symbol-trim-last 'function) . ,function) ...)])
       (run n
            (q)
            (fresh (tenv)
                   (mapo typed-helpero env tenv)
                   (outsideo q tenv t)
                   (evalo (with-functions env (combinator-helper ,q (input ...))) output))))]))

(define (run-test)
  (test "reverse"
        (synthesis 1 (q) '(fun (list int) (list int)) (foldlf) (,(list-c 1 2 3)) (list-v 3 2 1))
        '(((app (app (var foldl) (lam _.0 (lam _.1 (cons (var _.1) (var _.0))))) (list ()))
           (=/= ((_.0 _.1)))
           (sym _.0 _.1))))
  ;   (test "append"
  ;         (synthesis 1
  ;                    (q)
  ;                    '(fun (list int) (fun (list int) (list int)))
  ;                    (foldrf consf flipf)
  ;                    (,(list-c 1 2) ,(list-c 3 4))
  ;                    (list-v 1 2 3 4))
  ;         '((flip (foldr cons))))
  ;   (test "concat"
  ;         (synthesis 1
  ;                    (q)
  ;                    '(fun (list (list int)) (list int))
  ;                    (foldrf foldlf flipf consf)
  ;                    (,(list-c '(1 2) '(3 4)))
  ;                    (list-v 1 2 3 4))
  ;         '(((foldl (flip (foldr cons))) ())))
  ;   (let ([+f '(lam x (lam y (+ (var x) (var y))))]
  ;         [0f '(lam x (num ()))])
  ;     (test "sum"
  ;           (synthesis 1 (q) '(fun (list int) int) (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6))
  ;           '(((foldl +) (|0| ())))))
  ;   (test "isort"
  ;         (synthesis 1
  ;                    (q)
  ;                    '(fun (list int) (list int))
  ;                    (noEmptyf sortHelperf ltf fromHeadf)
  ;                    (,(list-c 3 1 2))
  ;                    (list-v 1 2 3))
  ;         '((fromHead (noEmpty (sortHelper lt)))))
  )
