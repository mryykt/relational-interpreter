#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "test-check.rkt")
(provide list-c
         string-c
         list-v
         string-v)

(define (list-c . xs)
  (define (helper ys)
    (if (null? ys)
        '()
        (cond
          [(char? (car ys)) `(cons (char ,(car ys)) ,(helper (cdr ys)))]
          [(number? (car ys)) `(cons (num ,(build-num (car ys))) ,(helper (cdr ys)))]
          [(list? (car ys)) `(cons ,(helper (car ys)) ,(helper (cdr ys)))])))
  (helper xs))

(define (string-c str)
  (apply list-c (string->list str)))

(define (list-v . xs)
  (define (helper ys)
    (if (null? ys)
        '()
        (cond
          [(char? (car ys)) (cons (car ys) (helper (cdr ys)))]
          [(number? (car ys)) (cons (build-num (car ys)) (helper (cdr ys)))]
          [(list? (car ys)) (cons (helper (car ys)) (helper (cdr ys)))])))
  (helper xs))

(define (string-v str)
  (apply list-v (string->list str)))

(define (run-test)
  (test "list-c-1"
        (list-c 1 2 3 4)
        `(cons (num ,(build-num 1))
               (cons (num ,(build-num 2))
                     (cons (num ,(build-num 3)) (cons (num ,(build-num 4)) ())))))
  (test "list-c-2"
        (list-c '(1 2) '(3 4))
        `(cons (cons (num ,(build-num 1)) (cons (num ,(build-num 2)) ()))
               (cons (cons (num ,(build-num 3)) (cons (num ,(build-num 4)) ())) ())))
  (test "list-v-1" (list-v 1 2 3 4) `(,(build-num 1) ,(build-num 2) ,(build-num 3) ,(build-num 4)))
  (test "list-v-2"
        (list-v '(1 2) '(3 4))
        `((,(build-num 1) ,(build-num 2)) (,(build-num 3) ,(build-num 4)))))
