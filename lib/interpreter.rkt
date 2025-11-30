#lang racket

(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "utils.rkt")
(require "test-check.rkt")
(provide evalo
         evalo^)

(defrel (evalo^ exp env v)
        (matche exp
                [(var ,x) (symbolo x) (lookupo x env v)]
                [(app ,f ,u)
                 (fresh (x t env^ uv g)
                        (evalo^ f env `((,g ,x ,t) . ,env^))
                        (evalo^ u env uv)
                        (evalo^ t `((,g . ((,g ,x ,t) . ,env^)) (,x . ,uv) . ,env^) v))]
                [(lam ,x ,t) (== v `((0 ,x ,t) . ,env))]
                [(fix ,f ,x ,t) (evalo^ `(var ,f) `((,f . ((,f ,x ,t) . ,env)) . ,env) v)]
                [(num ,n) (== v n)]
                [true (== v 'true)]
                [false (== v 'false)]
                [(+ ,l ,r) (binary-op l r env pluso v)]
                [(- ,l ,r) (binary-op l r env minuso v)]
                [(* ,l ,r) (binary-op l r env *o v)]
                [(= ,l ,r) (binary-op l r env eqo v)]
                [(< ,l ,r)
                 (fresh (lv rv)
                        (evalo^ l env lv)
                        (evalo^ r env rv)
                        (conde [(<o lv rv) (== v 'true)] [(<=o rv lv) (== v 'false)]))]
                [(list ,ls)
                 (matche ls [() (== v '())] [(,ca . ,cd) (binary-op ca `(list ,cd) env conso v)])]
                [(cons ,ca ,cd) (binary-op ca cd env conso v)]
                [(car ,ls) (fresh (lsv) (evalo^ ls env lsv) (caro v lsv))]
                [(cdr ,ls) (fresh (lsv) (evalo^ ls env lsv) (cdro v lsv))]
                [(if ,t ,e1 ,e2)
                 (fresh (tv)
                        (evalo^ t env tv)
                        (conde [(== tv 'true) (evalo^ e1 env v)] [(== tv 'false) (evalo^ e2 env v)]))]
                [(let ,x
                   ,t
                   ,e)
                 (fresh (tv ce env^ tl) (evalo^ t env tv) (evalo^ e `((,x . ,tv) . ,env) v))]))

(defrel (eqo l r v) (conde [(== l r) (== v 'true)] [(=/= l r) (== v 'false)]))

(define-syntax binary-op
  (syntax-rules ()
    [(_ l r env op v) (fresh (__l __r) (evalo^ l env __l) (evalo^ r env __r) (op __l __r v))]))

(defrel (evalo exp v) (evalo^ exp '() v))

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
             (evalo `(+ (num ,(build-num 2))
                        (- (num ,(build-num 10)) (* (num ,(build-num 3)) (num ,(build-num 3)))))
                    q))
        `(,(build-num 3)))
  (test "test-fix"
        (run 1
             (q)
             (evalo `(app (fix f
                               n
                               (if (= (var n) (num ()))
                                   (num ,(build-num 1))
                                   (* (var n) (app (var f) (- (var n) (num ,(build-num 1)))))))
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
  (test "test-eq-1" (run 1 (q) (evalo `(= (num ,(build-num 10)) (num ,(build-num 11))) q)) '(false))
  (test "test-eq-2" (run 1 (q) (evalo `(= (num ,(build-num 10)) (num ,(build-num 10))) q)) '(true))
  (test "test-list"
        (run 1 (q) (evalo `(list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))) q))
        `((,(build-num 1) ,(build-num 2) ,(build-num 3))))
  (test "test-car"
        (run 1
             (q)
             (evalo `(car (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3))))) q))
        `(,(build-num 1)))
  (test "test-cdr"
        (run 1
             (q)
             (evalo `(cdr (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3))))) q))
        `((,(build-num 2) ,(build-num 3))))
  (test "test-list-length"
        (run 1
             (q)
             (evalo `(app (fix f
                               x
                               (if (= (var x) (list ()))
                                   (num ,(build-num 0))
                                   (+ (num ,(build-num 1)) (app (var f) (cdr (var x))))))
                          (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))))
                    q))
        `(,(build-num 3)))
  (test "test-list-append"
        (run 1
             (q)
             (evalo `(app (app (fix f
                                    x
                                    (lam y
                                         (if (= (var x) (list ()))
                                             (var y)
                                             (cons (car (var x))
                                                   (app (app (var f) (cdr (var x))) (var y))))))
                               (list ((num ,(build-num 1)) (num ,(build-num 2)))))
                          (list ((num ,(build-num 3)))))
                    q))
        `((,(build-num 1) ,(build-num 2) ,(build-num 3))))
  (test "test- <" (run 1 (q) (evalo `(< (num ,(build-num 1)) (num ,(build-num 2))) q)) '(true))
  (test "test- <" (run 1 (q) (evalo `(< (num ,(build-num 2)) (num ,(build-num 1))) q)) '(false)))
