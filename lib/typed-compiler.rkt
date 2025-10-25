#lang racket
(require "compile-vm.rkt")
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require minikanren/numbers)

(provide typed-compileo
         typed-evalo)

(defrel
 (typed-compileo exp env t vm)
 (matche
  exp
  [(var ,x) (fresh (n) (symbolo x) (ntho n env `(,x . ,t)) (== vm `((search ,n))))]
  [(app ,f ,u)
   (fresh (a cf cu tl tl2)
          (typed-compileo f env `(fun ,a ,t) cf)
          (typed-compileo u env a cu)
          (== vm `(pushenv . ,tl))
          (appendo cu tl2 tl)
          (appendo `(push . ,cf) `(apply popenv) tl2))]
  [(lam ,x ,u)
   (fresh (a b cu env^)
          (typed-compileo u env^ b cu)
          (appendo env `((0 . none) (,x . ,a)) env^)
          (== vm `((mkclos ,cu)))
          (== t `(fun ,a ,b)))]
  [(fix ,f ,x ,u)
   (fresh (a b cu env^)
          (typed-compileo u env^ b cu)
          (appendo env `((,f . ,t) (,x . ,a)) env^)
          (== vm `((mkclos ,cu)))
          (== t `(fun ,a ,b)))]
  [(num ,n) (== t 'int) (== vm `((ldi ,n)))]
  [(+ ,u ,v) (binary-opo u v env 'add 'int 'int vm) (== t 'int)]
  [(- ,u ,v) (binary-opo u v env 'sub 'int 'int vm) (== t 'int)]
  [(* ,u ,v) (binary-opo u v env 'mul 'int 'int vm) (== t 'int)]
  [(= ,u ,v) (fresh (t^) (binary-opo u v env 'eq t^ t^ vm)) (== t 'int)]
  [(list ,ls)
   (fresh (t^)
          (matche
           ls
           [() (== t `(list ,t^)) (== vm '(nil))]
           [(,ca . ,cd) (binary-opo ca `(list ,cd) env 'cons t^ `(list ,t^) vm) (== t `(list ,t^))]))]
  [(cons ,ca ,cd) (fresh (t^) (binary-opo ca cd env 'cons t^ `(list ,t^) vm) (== t `(list ,t^)))]
  [(car ,ls) (fresh (cls) (typed-compileo ls env `(list ,t) cls) (appendo cls '(car) vm))]
  [(cdr ,ls) (fresh (cls) (typed-compileo ls env t cls) (appendo cls '(cdr) vm)) (caro 'list t)]
  [(ifz ,e ,u ,v)
   (fresh (ce cu cv)
          (typed-compileo e env 'int ce)
          (typed-compileo u env t cu)
          (typed-compileo v env t cv)
          (appendo ce `((test ,cu ,cv)) vm))]
  [(let ,x
     ,e1
     ,e2)
   (fresh (te1 ce1 ce2 env^ tl)
          (typed-compileo e1 env te1 ce1)
          (typed-compileo e2 env^ t ce2)
          (appendo env `((,x . ,te1)) env^)
          (appendo `(pushenv . ,ce1) `(extend . ,tl) vm)
          (appendo ce2 '(popenv) tl))]))

(defrel (binary-opo l r env op tl tr vm)
        (fresh (cl cr c)
               (typed-compileo l env tl cl)
               (typed-compileo r env tr cr)
               (appendo cr `(push . ,c) vm)
               (appendo cl `(,op) c)))

(defrel (typed-evalo exp t result) (fresh (c) (typed-compileo exp '() t c) (vmo c result)))

(defrel (helper exp ans) (fresh (t result) (typed-evalo exp t result) (== ans `(,t ,result))))

(define (run-test)
  (test "test-fun" (run 1 (q) (helper `(app (lam x (var x)) (num ,(build-num 1))) q)) '((int (1))))
  (test "test-fun-2"
        (run 1
             (q)
             (helper `(app (app (lam x (lam y (var y))) (num ,(build-num 1))) (num ,(build-num 2)))
                     q))
        `((int ,(build-num 2))))
  (test "test-ifz-1"
        (run 1 (q) (helper `(ifz (num ,(build-num 0)) (num ,(build-num 1)) (num ,(build-num 2))) q))
        `((int ,(build-num 1))))
  (test "test-ifz-2"
        (run 1 (q) (helper `(ifz (num ,(build-num 1)) (num ,(build-num 1)) (num ,(build-num 2))) q))
        `((int ,(build-num 2))))
  (test "test-arithmetic"
        (run 1
             (q)
             (helper `(+ (num ,(build-num 2))
                         (- (num ,(build-num 10)) (* (num ,(build-num 3)) (num ,(build-num 3)))))
                     q))
        `((int ,(build-num 3))))
  (test "test-fix"
        (run 1
             (q)
             (helper `(app (fix f
                                n
                                (ifz (var n)
                                     (num ,(build-num 1))
                                     (* (var n) (app (var f) (- (var n) (num ,(build-num 1)))))))
                           (num ,(build-num 4)))
                     q))
        `((int ,(build-num 24))))
  (test "test-let"
        (run 1
             (q)
             (helper `(let x (num
                              ,(build-num 1))
                        (var x))
                     q))
        `((int ,(build-num 1))))
  (test "test-eq-1"
        (run 1 (q) (helper `(= (num ,(build-num 10)) (num ,(build-num 11))) q))
        '((int (1))))
  (test "test-eq-2"
        (run 1 (q) (helper `(= (num ,(build-num 10)) (num ,(build-num 10))) q))
        '((int ())))
  (test
   "test-list"
   (run 1 (q) (helper `(list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))) q))
   `(((list int) (,(build-num 1) ,(build-num 2) ,(build-num 3)))))
  (test "test-car"
        (run 1
             (q)
             (helper `(car (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))))
                     q))
        `((int ,(build-num 1))))
  (test "test-cdr"
        (run 1
             (q)
             (helper `(cdr (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))))
                     q))
        `(((list int) (,(build-num 2) ,(build-num 3)))))
  (test "test-list-length"
        (run 1
             (q)
             (helper `(app (fix f
                                x
                                (ifz (= (var x) (list ()))
                                     (num ,(build-num 0))
                                     (+ (num ,(build-num 1)) (app (var f) (cdr (var x))))))
                           (list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)))))
                     q))
        `((int ,(build-num 3))))
  (test "test-list-append"
        (run 1
             (q)
             (helper `(app (app (fix f
                                     x
                                     (lam y
                                          (ifz (= (var x) (list ()))
                                               (var y)
                                               (cons (car (var x))
                                                     (app (app (var f) (cdr (var x))) (var y))))))
                                (list ((num ,(build-num 1)) (num ,(build-num 2)))))
                           (list ((num ,(build-num 3)))))
                     q))
        `(((list int) (,(build-num 1) ,(build-num 2) ,(build-num 3))))))
