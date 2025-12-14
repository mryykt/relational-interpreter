#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "utils.rkt")
(require "test-check.rkt")
(require "helper.rkt")

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
           [(true ,_env (v true))]
           [(false ,_env (v false))]
           [(() ,_env (v ()))]
           [((var ,x) ,env ,v) (symbolo x) (lookupo x env v)]
           [((lam ,x ,u) ,env (v (closure 0 ,x ,u ,env)))]
           [((fix ,f ,x ,u) ,env (v (closure ,f ,x ,u ,env)))]
           [((app (v (closure ,f ,x ,u ,env^)) (v ,v))
             ,_env
             (scope ,u ((,f . (v (closure ,f ,x ,u ,env^))) . ((,x . (v ,v)) . ,env^))))]
           [((app ,u ,v) ,env (app ,u^ ,v)) (stepo u env u^)]
           [((app (v ,u) ,v) ,env (app (v ,u) ,v^)) (stepo v env v^)]
           [((let ,x
               (v ,u)
               ,v)
             ,env
             (scope ,v ((,x . (v ,u)) . ,env)))]
           [((let ,x
               ,u
               ,v)
             ,env
             (let ,x
               ,u^
               ,v))
            (stepo u env u^)]
           [((if (v true) ,u ,_v) ,_env ,u)]
           [((if (v false) ,_u ,v) ,_env ,v)]
           [((if ,c ,u ,v) ,env (if ,c^ ,u ,v)) (stepo c env c^)]
           [(((v ,u) + (v ,v)) ,_env (v ,w)) (pluso u v w)]
           [(((v ,u) - (v ,v)) ,_env (v ,w)) (minuso u v w)]
           [(((v ,u) * (v ,v)) ,_env (v ,w)) (*o u v w)]
           [(((v ,u) = (v ,v)) ,_env (v true)) (== u v)]
           [(((v ,u) = (v ,v)) ,_env (v false)) (=/= u v)]
           [(((v ,u) < (v ,v)) ,_env (v true)) (<o u v)]
           [(((v ,u) < (v ,v)) ,_env (v false)) (conde [(<o v u)] [(== u v)])]
           [((,u ,op ,v) ,env (,u^ ,op ,v)) (membero op '(+ - * = <)) (stepo u env u^)]
           [(((v ,u) ,op ,v) ,env ((v ,u) ,op ,v^)) (membero op '(+ - * = <)) (stepo v env v^)]
           [((cons (v ,a) (v ,d)) ,_env (v (,a . ,d)))]
           [((cons ,a ,d) ,env (cons ,a^ ,d)) (stepo a env a^)]
           [((cons (v ,a) ,d) ,env (cons (v ,a) ,d^)) (stepo d env d^)]
           [((car (v (,a . ,_d))) ,_env (v ,a))]
           [((car ,c) ,env (car ,c^)) (stepo c env c^)]
           [((cdr (v (,_a . ,d))) ,_env (v ,d))]
           [((cdr ,c) ,env (cdr ,c^)) (stepo c env c^)]
           [((scope (v ,v) ,_env^) ,_env (v ,v))]
           [((scope ,u ,env^) ,_env (scope ,u^ ,env^)) (stepo u env^ u^)])

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
