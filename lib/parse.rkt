#lang racket
(require (except-in racket/match ==))
(require minikanren)
(require minikanren/numbers)
(require "syntax-check.rkt")
(provide parser
         unparser
         unbuild-num)

(define (app es)
  (match es
    [(list e1 e2) `(app ,e1 ,e2)]
    [(list e1 e2 e3 ...) (app `((app ,e1 ,e2) . ,e3))]))

(define (parser exp)
  (match exp
    [#t 'true]
    [#f 'false]
    [(list) '()]
    [(list 'lambda xs e)
     (if (pair? xs)
         `(lam ,(car xs) ,(parser `(lambda ,(cdr xs) ,e)))
         (parser e))]
    [(list 'fix f xs e)
     (if (pair? xs)
         `(fix ,f ,(car xs) ,(parser `(lambda ,(cdr xs) ,e)))
         (parser e))]
    [(list 'let x e1 e2)
     `(let ,x
        ,(parser e1)
        ,(parser e2))]
    [(list 'if e1 e2 e3)
     `(if ,(parser e1)
          ,(parser e2)
          ,(parser e3))]
    [(list e1 op e2)
     #:when (member op '(= < + - * /))
     `(,(parser e1) ,op ,(parser e2))]
    [(list 'cons a d) `(cons ,(parser a) ,(parser d))]
    [(list 'car l) `(car ,(parser l))]
    [(list 'cdr l) `(cdr ,(parser l))]
    [(list 'list e ...) (foldr (lambda (x acc) `(cons ,x ,acc)) '() (map parser e))]
    [(list e ...) (app (map parser e))]
    [(vector (list 'unbound) _ _) exp]
    [_
     (cond
       [(symbol? exp) `(var ,exp)]
       [(number? exp) `(num ,(build-num exp))]
       [(char? exp) `(char ,exp)])]))

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
    [(list 'num x) (unbuild-num x)]
    [(list 'char x) x]
    [(list) '()]
    [(list 'var x) x]
    [(list 'lam x e)
     (if (and (pair? e) (equal? (car e) 'lam))
         (match (unparser e)
           [(list _ xs e) `(lambda (,x . ,xs) ,e)])
         `(lambda (,x) ,(unparser e)))]
    [(list 'fix f x e)
     (if (and (pair? e) (equal? (car e) 'lam))
         (match (unparser e)
           [(list _ xs e) `(fix ,f (,x . ,xs) ,e)])
         `(fix ,f (,x) ,(unparser e)))]
    [(list 'let x e1 e2)
     `(let ,x
        ,(unparser e1)
        ,(unparser e2))]
    [(list 'if e1 e2 e3)
     `(if ,(unparser e1)
          ,(unparser e2)
          ,(unparser e3))]
    [(list 'app u v)
     (if (and (pair? u) (equal? (car u) 'app))
         (append (unparser u) `(,(unparser v)))
         `(,(unparser u) ,(unparser v)))]
    [(list e1 op e2)
     #:when (member op '(= < + - * /))
     `(,(unparser e1) ,op ,(unparser e2))]
    [(list 'cons a d)
     (if (constant-list? exp)
         (cons 'list (cons->list exp))
         `(cons ,(unparser a) ,(unparser d)))]
    [(list 'car l) `(car ,(unparser l))]
    [(list 'cdr l) `(cdr ,(unparser l))]))

(define (cons->list xs)
  (match xs
    ['() '()]
    [(list 'cons a d) (cons (unparser a) (cons->list d))]))

(define (constant? x)
  (match x
    [(list 'num _) #t]
    [(list 'char _) #t]
    ['() #t]
    [(list 'cons y ys) (and (constant? y) (constant-list? ys))]))

(define (constant-list? xs)
  (match xs
    ['() #t]
    [(list 'cons y ys) (and (constant? y) (constant-list? ys))]))

(define (check exp)
  (let ([f (lambda (e)
             (if (equal? e (parser (unparser e)))
                 #t
                 (begin
                   (displayln
                    (format "ng: ~a\n   => ~a\n   => ~a" e (unparser e) (parser (unparser e))))
                   #f)))])
    (if (and (list? exp) (not (null? exp)) (pair? (car exp)))
        (f (car exp))
        (f exp))))

(define (run-test)
  (count check (run 500 (q) (syntaxo q))))
