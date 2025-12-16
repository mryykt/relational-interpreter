#lang racket
(require "compile-vm.rkt")
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require minikanren/numbers)

(provide typedo
         typed-evalo)

(defrel
 (typedo exp env t)
 (matche exp
         [(var ,x) (symbolo x) (lookup-firsto x env t)]
         [(app ,f ,u) (fresh (a) (typedo f env `(fun ,a ,t)) (typedo u env a))]
         [(lam ,x ,u) (fresh (a b) (typedo u `((,x . ,a) . ,env) b) (== t `(fun ,a ,b)))]
         [(fix ,f ,x ,u)
          (fresh (a b) (typedo u `((,f . (fun ,a ,b)) . ((,x . ,a) . ,env)) b) (== t `(fun ,a ,b)))]
         [(num ,n) (== t 'int)]
         [(char ,_c) (== t 'char)]
         [true (== t 'bool)]
         [false (== t 'bool)]
         [(,u + ,v) (typedo u env 'int) (typedo v env 'int) (== t 'int)]
         [(,u - ,v) (typedo u env 'int) (typedo v env 'int) (== t 'int)]
         [(,u * ,v) (typedo u env 'int) (typedo v env 'int) (== t 'int)]
         [(,u = ,v) (fresh (a) (typedo u env a) (typedo v env a)) (== t 'bool)]
         [(,u < ,v) (typedo u env 'int) (typedo v env 'int) (== t 'bool)]
         [() (fresh (t^) (== t `(list ,t^)))]
         [(cons ,ca ,cd)
          (fresh (t^) (typedo ca env t^) (typedo cd env `(list ,t^)) (== t `(list ,t^)))]
         [(car ,ls) (typedo ls env `(list ,t))]
         [(cdr ,ls) (typedo ls env t) (caro 'list t)]
         [(if ,e ,u ,v) (typedo e env 'bool) (typedo u env t) (typedo v env t)]
         [(let ,x
            ,e1
            ,e2)
          (fresh (te1) (typedo e1 env te1) (typedo e2 `((,x . ,te1) . ,env) t))]))

(defrel (typed-evalo exp t result) (typedo exp '() t) (evalo exp result))

(define (run-test)
  (test "test-num" (run 1 (q) (typedo `(num ,(build-num 1)) '() q)) '(int))
  (test "test-fun" (run 1 (q) (typedo '(app (lam x (var x)) (num ())) '() q)) '(int))
  (test "test-fix"
        (run 1
             (q)
             (typedo `(app (fix f
                                n
                                (if ((var n) = (num ()))
                                    (num ,(build-num 1))
                                    ((var n) * (app (var f) ((var n) - (num ,(build-num 1)))))))
                           (num ,(build-num 4)))
                     '()
                     q))
        '(int))
  (test "test-arithmetic"
        (run 1
             (q)
             (typedo `((num ,(build-num 2))
                       +
                       ((num ,(build-num 10)) - ((num ,(build-num 3)) * (num ,(build-num 3)))))
                     '()
                     q))
        '(int))
  (test "test-list"
        (run 1 (q) (typedo `(app (lam x (cons (car (var x)) (cdr (var x)))) ,(list-c 1)) '() q))
        '((list int)))
  (test "test-let"
        (run 1
             (q)
             (typedo `(let x (num
                              ,(build-num 1))
                        (let y (num
                                ,(build-num 1))
                          ((var x) + (var y))))
                     '()
                     q))
        '(int)))
