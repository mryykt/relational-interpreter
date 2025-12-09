#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "test-check.rkt")
(require "type-inference.rkt")
(provide list-c
         list-v
         typed-helpero)

(define (list-c . xs)
  (define (helper ys)
    (if (null? ys)
        '()
        (cond
          [(number? (car ys)) (cons `(num ,(build-num (car ys))) (helper (cdr ys)))]
          [(list? (car ys)) (cons `(list ,(helper (car ys))) (helper (cdr ys)))])))
  `(list ,(helper xs)))

(define (list-v . xs)
  (define (helper ys)
    (if (null? ys)
        '()
        (cond
          [(number? (car ys)) (cons (build-num (car ys)) (helper (cdr ys)))]
          [(list? (car ys)) (cons (helper (car ys)) (helper (cdr ys)))])))
  (helper xs))

(defrel (typed-helpero ne nt)
        (matche ne [(,name . ,body) (fresh (t) (typedo body '() t) (== nt `(,name . ,t)))]))

(define (run-test)
  (test "list-c-1"
        (list-c 1 2 3 4)
        `(list ((num ,(build-num 1)) (num ,(build-num 2)) (num ,(build-num 3)) (num ,(build-num 4)))))
  (test "list-c-2"
        (list-c '(1 2) '(3 4))
        `(list ((list ((num ,(build-num 1)) (num ,(build-num 2))))
                (list ((num ,(build-num 3)) (num ,(build-num 4)))))))
  (test "list-v-1" (list-v 1 2 3 4) `(,(build-num 1) ,(build-num 2) ,(build-num 3) ,(build-num 4)))
  (test "list-v-2"
        (list-v '(1 2) '(3 4))
        `((,(build-num 1) ,(build-num 2)) (,(build-num 3) ,(build-num 4)))))
