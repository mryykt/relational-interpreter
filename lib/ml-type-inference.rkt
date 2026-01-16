#lang racket
(require "compile-vm.rkt")
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "parse.rkt")
(require minikanren/numbers)

(provide typedo)

(defrel
 (typedo exp env t)
 (matche
  exp
  [(num ,_n) (== t 'int)]
  [(char ,_c) (== t 'char)]
  [true (== t 'bool)]
  [false (== t 'bool)]
  [() (fresh (t^) (== t `(list ,t^)))]
  [(var ,x) (symbolo x) (lookup-firsto x env t)]
  [(lam ,x ,e) (fresh (a b) (typedo e `((,x . ,a) . ,env) b) (== t `(,a -> ,b)))]
  [(fix ,f ,x ,e)
   (fresh (a b) (typedo e `((,f . (,a -> ,b)) . ((,x . ,a) . ,env)) b) (== t `(,a -> ,b)))]
  [(app ,e1 ,e2) (fresh (a) (typedo e1 env `(,a -> ,t)) (typedo e2 env a))]
  [(if ,e1 ,e2 ,e3) (typedo e1 env 'bool) (typedo e2 env t) (typedo e3 env t)]
  [(let ,x
     ,e1
     ,e2)
   (fresh (e2^ te1) (typedo e1 env te1) (replaceo x e1 e2 e2^) (typedo e2^ `((,x . ,te1) . ,env) t))]
  [(,e1 = ,e2) (fresh (a) (typedo e1 env a) (typedo e2 env a)) (== t 'bool)]
  [(,e1 < ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'bool)]
  [(,e1 + ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
  [(,e1 - ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
  [(,e1 * ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
  [(cons ,e1 ,e2) (fresh (t^) (typedo e1 env t^) (typedo e2 env `(list ,t^)) (== t `(list ,t^)))]
  [(car ,e) (typedo e env `(list ,t))]
  [(cdr ,e) (typedo e env t) (caro 'list t)]))

(defrel (replaceo x r _e _e^)
        (matche (_e _e^)
                [((num ,n) (num ,n))]
                [((char ,c) (char ,c))]
                [(true true)]
                [(false false)]
                [(() ())]
                [((var ,x^) ,r^) (== x x^) (== r r^)]
                [((var ,y) (var ,y)) (=/= x y)]
                [((lam ,x ,e) (lam ,x ,e))]
                [((lam ,y ,e) (lam ,y ,e^)) (=/= x y) (replaceo x r e e^)]
                [((fix ,f ,x ,e) (fix ,f ,x ,e))]
                [((fix ,x^ ,y ,e) (fix ,x^ ,y ,e)) (== x x^)]
                [((fix ,f ,y ,e^) (fix ,f ,y ,e^)) (=/= x f) (=/= x y) (replaceo x r e^ e^)]
                [((app ,e1 ,e2) (app ,e1^ ,e2^)) (replaceo x r e1 e1^) (replaceo x r e2 e2^)]
                [((let ,x^
                    ,e1
                    ,e2)
                  (let ,x^
                    ,e1^
                    ,e2))
                 (== x x^)
                 (replaceo x r e1 e1^)]
                [((let ,y
                    ,e1
                    ,e2)
                  (let ,y
                    ,e1^
                    ,e2^))
                 (=/= x y)
                 (replaceo x r e1 e1^)
                 (replaceo x r e2 e2^)]
                [((if ,e1 ,e2 ,e3) (if ,e1^ ,e2^ ,e3^))
                 (replaceo x r e1 e1^)
                 (replaceo x r e2 e2^)
                 (replaceo x r e3 e3^)]
                [((,e1 ,op ,e2) (,e1^ ,op ,e2^))
                 (membero op '(= < + - *))
                 (replaceo x r e1 e1^)
                 (replaceo x r e2 e2^)]
                [((cons ,e1 ,e2) (cons ,e1^ ,e2^)) (replaceo x r e1 e1^) (replaceo x r e2 e2^)]
                [((car ,e) (car ,e^)) (replaceo x r e e^)]
                [((cdr ,e) (cdr ,e^)) (replaceo x r e e^)]))

(define (run-test)
  (test "test-num" (run 1 (q) (typedo (parser 1) '() q)) '(int))
  (test "test-fun" (run 1 (q) (typedo (parser '((lambda (x) x) 0)) '() q)) '(int))
  (test "test-fix"
        (run 1
             (q)
             (typedo (parser '((fix f
                                    (n)
                                    (if (n = 0)
                                        1
                                        (n * (f (n - 1)))))
                               4))
                     '()
                     q))
        '(int))
  (test "test-arithmetic" (run 1 (q) (typedo (parser '(2 + (10 - (3 * 3)))) '() q)) '(int))
  (test "test-list"
        (run 1 (q) (typedo (parser '((lambda (x) (cons (car x) (cdr x))) (list 1))) '() q))
        '((list int)))
  (test "test-let"
        (run 1
             (q)
             (typedo (parser '(let x
                                1
                                (let y
                                  1
                                  (x + y))))
                     '()
                     q))
        '(int))
  (test "test-polymorphic"
        (run 1
             (q)
             (typedo (parser '(let id (lambda
                                       [x]
                                       x)
                                (id id)))
                     '()
                     q))
        '((_.0 -> _.0))))
