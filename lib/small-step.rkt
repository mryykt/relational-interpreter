#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")
(require "functions.rkt")
(require "parse.rkt")

(provide evalo)

(defrel (evalo exp val) (eval-expo exp '() val))

(defrel (eval-expo exp env val)
        (fresh (exp^)
               (stepo exp env exp^)
               (conde [(fresh (v) (== exp^ `(v ,v)) (== val v))] [(eval-expo exp^ env val)])))

(defrel (eval-stepo exp val) (eval-step-expo exp '() val))

(defrel (eval-step-expo exp env val)
        (conde [(stepo exp env val)]
               [(fresh (exp^) (stepo exp env exp^) (eval-step-expo exp^ env val))]))

(defmatche (stepo _l _e1 _r)
           [((num ,n) ,_env (v ,n))]
           [((char ,c) ,_env (v ,c))]
           [(true ,_env (v true))]
           [(false ,_env (v false))]
           [(() ,_env (v ()))]
           [((var ,x) ,env ,v) (symbolo x) (lookup-firsto x env v)]
           [((lam ,x ,e) ,env (v (closure 0 ,x ,e ,env)))]
           [((fix ,f ,x ,e) ,env (v (closure ,f ,x ,e ,env)))]
           [((app (v (closure ,f ,x ,e ,env^)) (v ,v))
             ,_env
             (scope ,e ((,f . (v (closure ,f ,x ,e ,env^))) . ((,x . (v ,v)) . ,env^))))]
           [((app ,e1 ,e2) ,env (app ,u^ ,e2)) (stepo e1 env u^)]
           [((app (v ,v) ,e) ,env (app (v ,v) ,v^)) (stepo e env v^)]
           [((let ,x
               (v ,v)
               ,e)
             ,env
             (scope ,e ((,x . (v ,v)) . ,env)))]
           [((let ,x
               ,e1
               ,e2)
             ,env
             (let ,x
               ,e1^
               ,e2))
            (stepo e1 env e1^)]
           [((if (v true) ,e1 ,_e2) ,_env ,e1)]
           [((if (v false) ,_e1 ,e2) ,_env ,e2)]
           [((if ,e1 ,e2 ,e3) ,env (if ,e1^ ,e2 ,e3)) (stepo e1 env e1^)]
           [(((v ,v1) + (v ,v2)) ,_env (v ,v)) (pluso v1 v2 v)]
           [(((v ,v1) - (v ,v2)) ,_env (v ,v)) (minuso v1 v2 v)]
           [(((v ,v1) * (v ,v2)) ,_env (v ,v)) (*o v1 v2 v)]
           [(((v ,v1) = (v ,v2)) ,_env (v true)) (== v1 v2)]
           [(((v ,v1) = (v ,v2)) ,_env (v false)) (=/= v1 v2)]
           [(((v ,v1) < (v ,v2)) ,_env (v true)) (<o v1 v2)]
           [(((v ,v1) < (v ,v2)) ,_env (v false)) (conde [(<o v2 v1)] [(== v1 v2)])]
           [((,e1 ,op ,e2) ,env (,e1^ ,op ,e2)) (membero op '(+ - * = <)) (stepo e1 env e1^)]
           [(((v ,v) ,op ,e) ,env ((v ,v) ,op ,e^)) (membero op '(+ - * = <)) (stepo e env e^)]
           [((cons (v ,v1) (v ,v2)) ,_env (v (,v1 . ,v2)))]
           [((cons ,e1 ,e2) ,env (cons ,e1^ ,e2)) (stepo e1 env e1^)]
           [((cons (v ,v) ,e) ,env (cons (v ,v) ,e^)) (stepo e env e^)]
           [((car (v (,v1 . ,_v2))) ,_env (v ,v1))]
           [((car ,e) ,env (car ,e^)) (stepo e env e^)]
           [((cdr (v (,_v1 . ,v2))) ,_env (v ,v2))]
           [((cdr ,e) ,env (cdr ,e^)) (stepo e env e^)]
           [((scope (v ,v) ,_env^) ,_env (v ,v))]
           [((scope ,e ,env^) ,_env (scope ,e^ ,env^)) (stepo e env^ e^)])

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
