#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
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
  (test "test-fun" (run 1 (q) (evalo `(app (lam x (var x)) (num ,(build-num 1))) q)) '((1)))
  (test "test-fun-2"
        (run 1
             (q)
             (evalo `(app (app (lam x (lam y (var y))) (num ,(build-num 1))) (num ,(build-num 2))) q))
        `(,(build-num 2)))
  (test "test-if-1"
        (run 1
             (q)
             (evalo `(if true
                         (num ,(build-num 1))
                         (num ,(build-num 2)))
                    q))
        `(,(build-num 1)))
  (test "test-if-2"
        (run 1
             (q)
             (evalo `(if false
                         (num ,(build-num 1))
                         (num ,(build-num 2)))
                    q))
        `(,(build-num 2)))
  (test "test-arithmetic"
        (run 1
             (q)
             (evalo `((num ,(build-num 2))
                      +
                      ((num ,(build-num 10)) - ((num ,(build-num 3)) * (num ,(build-num 3)))))
                    q))
        `(,(build-num 3)))
  (test "test-fix"
        (run 1
             (q)
             (evalo `(app (fix f
                               n
                               (if ((var n) = (num ()))
                                   (num ,(build-num 1))
                                   ((var n) * (app (var f) ((var n) - (num ,(build-num 1)))))))
                          (num ,(build-num 4)))
                    q))
        `(,(build-num 24)))
  (test "test-let-1"
        (run 1
             (q)
             (evalo `(let x (num
                             ,(build-num 1))
                       (var x))
                    q))
        `(,(build-num 1)))
  (test "test-let-2"
        (run 1
             (q)
             (evalo `(let f (lam
                             x
                             [var x])
                       (app (var f) (num ,(build-num 1))))
                    q))
        `(,(build-num 1)))
  (test "test-eq-1" (run 1 (q) (evalo `((num ,(build-num 10)) = (num ,(build-num 11))) q)) '(false))
  (test "test-eq-2" (run 1 (q) (evalo `((num ,(build-num 10)) = (num ,(build-num 10))) q)) '(true))
  (test "test-list" (run 1 (q) (evalo (list-c 1 2 3) q)) `(,(list-v 1 2 3)))
  (test "test-car" (run 1 (q) (evalo `(car ,(list-c 1 2 3)) q)) `(,(build-num 1)))
  (test "test-cdr" (run 1 (q) (evalo `(cdr ,(list-c 1 2 3)) q)) `(,(list-v 2 3)))
  (test "test-list-length"
        (run 1
             (q)
             (evalo `(app (fix f
                               x
                               (if ((var x) = ())
                                   (num ,(build-num 0))
                                   ((num ,(build-num 1)) + (app (var f) (cdr (var x))))))
                          ,(list-c 1 2 3))
                    q))
        `(,(build-num 3)))
  (test "test-list-append"
        (run 1
             (q)
             (evalo `(app (app (fix f
                                    x
                                    (lam y
                                         (if ((var x) = ())
                                             (var y)
                                             (cons (car (var x))
                                                   (app (app (var f) (cdr (var x))) (var y))))))
                               ,(list-c 1 2))
                          ,(list-c 3))
                    q))
        `(,(list-v 1 2 3)))
  (test "test- <" (run 1 (q) (evalo `((num ,(build-num 1)) < (num ,(build-num 2))) q)) '(true))
  (test "test- <" (run 1 (q) (evalo `((num ,(build-num 2)) < (num ,(build-num 1))) q)) '(false)))
