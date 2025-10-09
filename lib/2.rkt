#lang racket
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")

(defrel (minio exp env val)
        (conde [(numbero exp) (== val exp)]
               [(symbolo exp) (lookupo exp env val)]
               [(caro 'list exp) (== exp val)]
               [(fresh (ca cd ca^ cd^)
                       (== exp `(cons ,ca ,cd))
                       (minio ca env ca^)
                       (minio cd env cd^)
                       (matche cd [(list ,xs) (== val `(list (,ca . ,xs)))]))]
               [(caro 'lambda exp) (== exp val)]
               [(fresh (f e f^ e^ x body env^)
                       (== exp `(app ,f ,e))
                       (minio f env f^)
                       (minio e env e^)
                       (== f^ `(lambda ,x ,body))
                       (conso `(,x . ,e^) env env^)
                       (minio body env^ val))]
               [(fresh (x e1 e2 e1^ env^)
                       (== exp
                           `(let ,x
                              ,e1
                              ,e2))
                       (minio e1 env e1^)
                       (conso `(,x . ,e1^) env env^)
                       (minio e2 env^ val))]
               [(fresh (xs e1 e2 ca cd xs^)
                       (== exp
                           `(case ,xs
                              ,e1
                              ,e2))
                       (minio xs env xs^)
                       (matche xs^
                               [(list ()) (minio e1 env val)]
                               [(list (,ca . ,cd))
                                (minio `(app (app ,e2 ,ca) (list ,cd)) env val)]))]))

(define (run-test)
  (test "cons" (run 2 (q) (minio '(cons 1 (list ())) '() q)) '((list (1))))
  (test "function" (run 1 (q) (minio '(app (lambda x x) 1) '() q)) '(1))
  (test "case-list"
        (run 1
             (q)
             (minio '(case (list (1 2 3))
                       [list ()]
                       [lambda
                        ca
                        (lambda cd cd)])
                    '()
                    q))
        '((list (2 3))))
  (test "case-list-null"
        (run 1
             (q)
             (minio '(case (list ())
                       1
                       [lambda
                        ca
                        (lambda cd cd)])
                    '()
                    q))
        '(1))
  (test "let"
        (run 1
             (q)
             (minio '(let x
                       1
                       x)
                    '()
                    q))
        '(1))
  (test "integration"
        (run 1
             (q)
             (minio '(let f (lambda
                             xs
                             [case
                              xs
                              (list ())
                              (lambda ca (lambda cd (cons (cons ca (list ())) (app f cd))))])
                       (app f (list (1 2 3))))
                    '()
                    q))
        '((list ((list (1)) (list (2)) (list (3)))))))
