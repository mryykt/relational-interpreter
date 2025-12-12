#lang racket
(require (except-in racket/match ==))
(require minikanren)
(require minikanren/numbers)
(require "syntax-check.rkt")
(provide parser
         unparser
         run-test)

(define (app es)
  (match es
    [(list e1 e2) `(app ,e1 ,e2)]
    [(list e1 e2 e3 ...) (app `((app ,e1 ,e2) . ,e3))]))

(define (parser exp)
  (match exp
    [#t 'true]
    [#f 'false]
    [(list l x ...)
     #:when (equal? l 'list)
     `(list ,(map parser x))]
    [(list l x e)
     #:when (equal? l 'lambda)
     `(lam ,x ,(parser e))]
    [(list l f x e)
     #:when (equal? l 'fix)
     `(fix ,f ,x ,(parser e))]
    [(list l x e1 e2)
     #:when (equal? l 'let)
     `(let ,x
        ,(parser e1)
        ,(parser e2))]
    [(list i e1 e2 e3)
     #:when (equal? i 'if)
     `(if ,(parser e1)
          ,(parser e2)
          ,(parser e3))]
    [(list e1 op e2)
     #:when (member op '(= < + - * /))
     `(,(parser e1) ,op ,(parser e2))]
    [(list c a d)
     #:when (equal? c 'cons)
     `(cons ,(parser a) ,(parser d))]
    [(list c l)
     #:when (equal? c 'car)
     `(car ,(parser l))]
    [(list c l)
     #:when (equal? c 'cdr)
     `(cdr ,(parser l))]
    [(list e ...) (app (map parser e))]
    [_
     (cond
       [(symbol? exp) `(var ,exp)]
       [(number? exp) `(num ,(build-num exp))])]))

(define (unbuild-num n)
  (letrec ([f (lambda (x acc w)
                (if (pair? x)
                    (f (cdr x) (+ (* w (car x)) acc) (* w 2))
                    acc))])
    (f n 0 1)))

(define (unparser exp)
  (match exp
    ['true #t]
    ['false #f]
    [(list n x)
     #:when (equal? n 'num)
     (unbuild-num x)]
    [(list l x)
     #:when (equal? l 'list)
     `(list . ,(map unparser x))]
    [(list v x)
     #:when (equal? v 'var)
     x]
    [(list l x e)
     #:when (equal? l 'lam)
     `(lambda ,x ,(unparser e))]
    [(list l f x e)
     #:when (equal? l 'fix)
     `(fix ,f ,x ,(unparser e))]
    [(list l x e1 e2)
     #:when (equal? l 'let)
     `(let ,x
        ,(unparser e1)
        ,(unparser e2))]
    [(list i e1 e2 e3)
     #:when (equal? i 'if)
     `(if ,(unparser e1)
          ,(unparser e2)
          ,(unparser e3))]
    [(list a u v)
     #:when (equal? a 'app)
     (if (and (pair? u) (equal? (car u) 'app))
         (append (unparser u) `(,(unparser v)))
         `(,(unparser u) ,(unparser v)))]
    [(list e1 op e2)
     #:when (member op '(= < + - * /))
     `(,(unparser e1) ,op ,(unparser e2))]
    [(list c a d)
     #:when (equal? c 'cons)
     `(cons ,(unparser a) ,(unparser d))]
    [(list c l)
     #:when (equal? c 'car)
     `(car ,(unparser l))]
    [(list c l)
     #:when (equal? c 'cdr)
     `(cdr ,(unparser l))]))

(define (check exp)
  (let ([f (lambda (e)
             (if (equal? e (parser (unparser e)))
                 #t
                 (begin
                   (displayln
                    (format "ng: ~a\n   => ~a\n   => ~a" e (unparser e) (parser (unparser e))))
                   #f)))])
    (if (and (list? exp) (pair? (car exp)))
        (f (car exp))
        (f exp))))

(define (run-test)
  (count check (run 500 (q) (syntax-checko q))))
