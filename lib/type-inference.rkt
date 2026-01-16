#lang racket
(require "compile-vm.rkt")
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "parse.rkt")
(require minikanren/numbers)

(provide typedo
         typed-evalo)

(defrel
 (typedo exp env t)
 (matche exp
         [(var ,x) (symbolo x) (lookup-firsto x env t)]
         [(app ,e1 ,e2) (fresh (a) (typedo e1 env `(,a -> ,t)) (typedo e2 env a))]
         [(lam ,x ,e) (fresh (a b) (typedo e `((,x . ,a) . ,env) b) (== t `(,a -> ,b)))]
         [(fix ,f ,x ,e)
          (fresh (a b) (typedo e `((,f . (,a -> ,b)) . ((,x . ,a) . ,env)) b) (== t `(,a -> ,b)))]
         [(num ,_n) (== t 'int)]
         [(char ,_c) (== t 'char)]
         [true (== t 'bool)]
         [false (== t 'bool)]
         [(,e1 + ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
         [(,e1 - ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
         [(,e1 * ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'int)]
         [(,e1 = ,e2) (fresh (a) (typedo e1 env a) (typedo e2 env a)) (== t 'bool)]
         [(,e1 < ,e2) (typedo e1 env 'int) (typedo e2 env 'int) (== t 'bool)]
         [() (fresh (t^) (== t `(list ,t^)))]
         [(cons ,e1 ,e2)
          (fresh (t^) (typedo e1 env t^) (typedo e2 env `(list ,t^)) (== t `(list ,t^)))]
         [(car ,e) (typedo e env `(list ,t))]
         [(cdr ,e) (typedo e env t) (caro 'list t)]
         [(if ,e1 ,e2 ,e3) (typedo e1 env 'bool) (typedo e2 env t) (typedo e3 env t)]
         [(let ,x
            ,e1
            ,e2)
          (fresh (te1) (typedo e1 env te1) (typedo e2 `((,x . ,te1) . ,env) t))]))

(defrel (typed-evalo exp t result) (typedo exp '() t) (evalo exp result))

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
        '()))
