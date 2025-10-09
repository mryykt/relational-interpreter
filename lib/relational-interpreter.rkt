#lang racket
(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")

(defmatche (wro exp val) [(,m ,m)] [(,l ,n) (fresh (m) (stepo l m) (wro m n))])

(defmatche (stepo exp val)
           [(,m ,n) (contractiono m n)]
           [((,m ,n) (,m1 ,n)) (stepo m m1)]
           [((,m ,n) (,m ,n1)) (stepo n n1)])

(defmatche (contractiono exp val)
           [((((foldl ,f) ,acc) ((cons ,x) ,xs)) (((foldl ,f) ((,f ,x) ,acc)) ,xs)) (=/= xs 'nil)]
           [((((foldl ,f) ,acc) ((cons ,x) nil)) ((,f ,x) ,acc))]
           [((((foldr ,f) ,init) ((cons ,x) ,xs)) ((,f ,x) (((foldr ,f) ,init) ,xs))) (=/= xs 'nil)]
           [((((foldr ,f) ,init) ((cons ,x) nil)) ((,f ,x) ,init))]
           [(((map ,f) ((cons ,x) ,xs)) ((cons (,f ,x)) ((map ,f) ,xs))) (=/= xs 'nil)]
           [(((map ,f) ((cons ,x) nil)) ((cons (,f ,x)) nil))]
           [((((flip ,f) ,x) ,y) ((,f ,y) ,x))]
           [((((compose ,f) ,g) ,x) (,f (,g ,x)))])

(define (lst . elms)
  (if (null? elms)
      'nil
      `((cons ,(car elms)) ,(apply lst (cdr elms)))))

(defrel
 (evalo exp val)
 (conde [(fresh (f acc acc^ xs xs^ acc2)
                (== exp `(((foldl ,f) ,acc) ,xs))
                (evalo acc acc^)
                (evalo xs xs^)
                (matche xs^
                        [(list ()) (== val acc^)]
                        [(list (,ca . ,cd))
                         (evalo `((,f ,ca) ,acc^) acc2)
                         (evalo `(((foldl ,f) ,acc2) (list ,cd)) val)]))]
        [(fresh (f init init^ xs xs^ acc)
                (== exp `(((foldr ,f) ,init) ,xs))
                (evalo init init^)
                (evalo xs xs^)
                (matche xs^
                        [(list ()) (== val init^)]
                        [(list (,ca . ,cd))
                         (evalo `(((foldr ,f) ,init^) (list ,cd)) acc)
                         (evalo `((,f ,ca) ,acc) val)]))]
        [(fresh (f x y x^ y^)
                (== exp `(((flip ,f) ,x) ,y))
                (evalo x x^)
                (evalo y y^)
                (evalo `((,f ,y^) ,x^) val))]
        [(fresh (f g x x^) (== exp `(((compose ,f) ,g) ,x)) (evalo x x^) (evalo `(,f (,g ,x^)) val))]
        [(fresh (ca cd ca^ cd^)
                (== exp `((cons ,ca) ,cd))
                (evalo ca ca^)
                (evalo cd cd^)
                (matche cd^ [(list ,xs) (== val `(list (,ca^ . ,xs)))]))]
        [(caro 'list exp) (== val exp)]
        [(numbero exp) (== val exp)]
        [(symbolo exp) (== val exp)]))

(define-syntax fn
  (syntax-rules ()
    [(_ f) (quasiquote (f))]
    [(_ f a) (quasiquote (f a))]
    [(_ f a b ...) (fn (f a) b ...)]))

(define (run-test)
  (begin
    (test "foldl" (run* (q) (evalo (fn foldl cons (list ()) (list (1 2 3))) q)) '((list (3 2 1))))
    (test "foldr" (run* (q) (evalo (fn foldr cons (list ()) (list (1 2 3))) q)) '((list (1 2 3))))
    (test "flip" (run* (q) (evalo (fn flip cons (list ()) 1) q)) '((list (1))))
    ; synthesis test
    (test "reverse"
          (run 1 (q) (evalo `(,q (list (1 2 3))) '(list (3 2 1))))
          `(,(fn foldl cons (list ()))))
    (test "append"
          (run 1 (q) (evalo `((,q (list (1 2))) (list (3 4))) '(list (1 2 3 4))))
          `(,(fn flip (foldr cons))))
    ; 無理
    ; (test "concat"
    ;   (run 1 (q) (evalo `(,q (list (list (1 2)) (list (3 4)))) '(list (1 2 3 4))))
    ;   '(((foldl (flip (foldr cons))) (list ()))))
    ))

(run 1 (q) (evalo (fn foldr (flip (foldr cons)) (list ()) (list (list (1 2)) (list (3 4)))) q))
