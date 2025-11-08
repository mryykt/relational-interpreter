#lang racket
(require minikanren)
(require minikanren/matche)
(require minikanren/numbers)
(require "interpreter.rkt")
(require "functions.rkt")
(require "helper.rkt")
(require "type-inference.rkt")
(require "test-check.rkt")

(defmatche (translateo src dst)
           [((,f ,x) (app ,f^ ,x^)) (translateo f f^) (translateo x x^)]
           [(,v (var ,v)) (symbolo v)]
           [(() (list ()))])

(define-syntax app-macro
  (syntax-rules ()
    [(_ f (input)) `(app f input)]
    [(_ f (input1 input2 ...)) (app-macro `(app f input1) (input2 ...))]))

(define-syntax combinator-helper
  (syntax-rules ()
    [(_ program (input)) `(app program input)]
    [(_ program (input1 input2 ...)) (combinator-helper (app program input1) (input2 ...))]))

(define-syntax combinator
  (syntax-rules ()
    [(_ src t (function ...))
     (fresh (__dst __program __dummy)
            (translateo src __dst)
            (== __program (make-program function ... ,__dst))
            (typedo __program '() t)
            (evalo __program __dummy))]
    [(_ src t (function ...) (input ...) output)
     (fresh (__dst __program)
            (translateo src __dst)
            (== __program (make-program function ... ,(combinator-helper ,__dst (input ...))))
            (typedo __program '() t)
            (evalo __program output))]))

(define (run-test)
  (test "reverse"
        (run 1 (q) (combinator q '(list int) (foldlf flipf consf) (,(list-c 1 2)) (list-v 2 1)))
        '(((foldl (flip cons)) ())))
  (test
   "append"
   (run
    1
    (q)
    (combinator q '(list int) (foldrf consf flipf) (,(list-c 1 2) ,(list-c 3 4)) (list-v 1 2 3 4)))
   '((flip (foldr cons))))
  (test
   "concat"
   (run
    1
    (q)
    (combinator q '(list int) (foldrf foldlf flipf consf) (,(list-c '(1 2) '(3 4))) (list-v 1 2 3 4)))
   '(((foldl (flip (foldr cons))) ())))
  (test "concat-type"
        (run 2 (q) (combinator q `(fun (list (list int)) (list int)) (foldrf foldlf flipf consf)))
        '(((foldl (foldr cons)) ()) ((foldl (foldr (flip cons))) ())))
  (let ([+f '(lam x (lam y (+ (var x) (var y))))]
        [0f '(lam x (num ()))])
    (test "sum"
          (run 1 (q) (combinator q 'int (foldlf +f 0f) (,(list-c 1 2 3)) (build-num 6)))
          '(((foldl +) (|0| +)))))
  (test "map"
        (run 1
             (q)
             (combinator q
                         `(fun (fun int (list int)) (fun (list int) (list (list int))))
                         (foldrf composef flipf consf)))
        '()))
