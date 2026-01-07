#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "parse.rkt")

(provide evalo
         eval-expo)

(defrel (eval-expo exp env _v)
        (matche (exp _v)
                [((num ,n) ,n)]
                [((char ,c) ,c)]
                [(true true)]
                [(false false)]
                [(() ())]
                [((var ,x) ,v) (symbolo x) (lookup-firsto x env v)]
                [((lam ,x ,e) ((0 ,x ,e) . ,env^)) (== env env^)]
                [((fix ,f ,x ,e) ((,f ,x ,e) . ,env^)) (== env env^)]
                [((app ,e1 ,e2) ,v)
                 (fresh (f x e env^ v^)
                        (eval-expo e1 env `((,f ,x ,e) . ,env^))
                        (eval-expo e2 env v^)
                        (eval-expo e `((,f . ((,f ,x ,e) . ,env^)) (,x . ,v^) . ,env^) v))]
                [((let ,x
                    ,e1
                    ,e2)
                  ,v)
                 (fresh (v^) (eval-expo e1 env v^) (eval-expo e2 `((,x . ,v^) . ,env) v))]
                [((if ,e1 ,e2 ,e3) ,v)
                 (fresh (tv)
                        (eval-expo e1 env tv)
                        (conde [(== tv 'true) (eval-expo e2 env v)]
                               [(== tv 'false) (eval-expo e3 env v)]))]
                [((,e1 = ,e2) ,v) (binary-op e1 e2 env eqo v)]
                [((,e1 < ,e2) ,v)
                 (fresh (v1 v2)
                        (eval-expo e1 env v1)
                        (eval-expo e2 env v2)
                        (conde [(<o v1 v2) (== v 'true)] [(<=o v2 v1) (== v 'false)]))]
                [((,e1 + ,e2) ,v) (binary-op e1 e2 env pluso v)]
                [((,e1 - ,e2) ,v) (binary-op e1 e2 env minuso v)]
                [((,e1 * ,e2) ,v) (binary-op e1 e2 env *o v)]
                [((cons ,e1 ,e2) ,v) (binary-op e1 e2 env conso v)]
                [((car ,e) ,a) (fresh (d) (eval-expo e env `(,a . ,d)))]
                [((cdr ,e) ,d) (fresh (a) (eval-expo e env `(,a . ,d)))]))

(defrel (eqo l r v) (conde [(== l r) (== v 'true)] [(=/= l r) (== v 'false)]))

(define-syntax binary-op
  (syntax-rules ()
    [(_ l r env op v) (fresh (__l __r) (eval-expo l env __l) (eval-expo r env __r) (op __l __r v))]))

(defrel (evalo exp v) (eval-expo exp '() v))

(define (run-test)
  (test "test-fun" (run 1 (q) (evalo (parser '((lambda (x) x) 1)) q)) '((1)))
  (test "test-fun-2" (run 1 (q) (evalo (parser '((lambda (x y) y) 1 2)) q)) `(,(build-num 2)))
  (test "test-if-1" (run 1 (q) (evalo (parser '(if #t 1 2)) q)) `(,(build-num 1)))
  (test "test-if-2" (run 1 (q) (evalo (parser '(if #f 1 2)) q)) `(,(build-num 2)))
  (test "test-arithmetic" (run 1 (q) (evalo (parser '(2 + (10 - (3 * 3)))) q)) `(,(build-num 3)))
  (test "test-fix"
        (run 1
             (q)
             (evalo (parser '((fix f
                                   (n)
                                   (if (n = 0)
                                       1
                                       (n * (f (n - 1)))))
                              4))
                    q))
        `(,(build-num 24)))
  (test "test-let-1"
        (run 1
             (q)
             (evalo (parser '(let x
                               1
                               x))
                    q))
        `(,(build-num 1)))
  (test "test-let-2"
        (run 1
             (q)
             (evalo (parser '(let f (lambda
                                     [x]
                                     x)
                               (f 1)))
                    q))
        `(,(build-num 1)))
  (test "test-eq-1" (run 1 (q) (evalo (parser '(10 = 11)) q)) '(false))
  (test "test-eq-2" (run 1 (q) (evalo (parser '(10 = 10)) q)) '(true))
  (test "test-list" (run 1 (q) (evalo (parser '(list 1 2 3)) q)) `(,(list-v 1 2 3)))
  (test "test-car" (run 1 (q) (evalo (parser '(car (list 1 2 3))) q)) `(,(build-num 1)))
  (test "test-cdr" (run 1 (q) (evalo (parser '(cdr (list 1 2 3))) q)) `(,(list-v 2 3)))
  (test "test-list-length"
        (run 1
             (q)
             (evalo (parser '((fix f
                                   (xs)
                                   (if (xs = ())
                                       0
                                       (1 + (f (cdr xs)))))
                              (list 1 2 3)))
                    q))
        `(,(build-num 3)))
  (test "test-list-append"
        (run 1
             (q)
             (evalo (parser '((fix f
                                   (xs ys)
                                   (if (xs = ())
                                       ys
                                       (cons (car xs) (f (cdr xs) ys))))
                              (list 1 2)
                              (list 3)))
                    q))
        `(,(list-v 1 2 3)))
  (test "test- <" (run 1 (q) (evalo (parser '(1 < 2)) q)) '(true))
  (test "test- <" (run 1 (q) (evalo (parser '(2 < 1)) q)) '(false))
  (test "fill-1" (run 1 (q) (evalo (parser `(,q + 2)) (build-num 3))) '((num (1))))
  (test "fill-2" (run 2 (q) (evalo (parser `(,q < 2)) 'true)) '((num ()) (num (1))))
  (test "fill-3" (run 20 (q) (evalo (parser `((lambda (x) ,q) 999)) (build-num 1000))) '()))
