#lang racket
(require minikanren)
(require minikanren/numbers)
(require minikanren/matche)

(provide debug-trace
         conso
         caro
         cdro
         membero
         rembero
         appendo
         lengtho
         lookup-firsto
         lookup2o
         mapo
         allo
         inco
         deco
         ntho)

(define-syntax debug-trace
  (syntax-rules ()
    [(_ str a ...)
     (project (a ...)
              (begin
                (display str)
                (display (format " ~a" a)) ...
                (display "\n")
                (== #t #t)))]))

(define (conso ca cd ls)
  (== `(,ca . ,cd) ls))

(define (caro ca ls)
  (fresh (x) (conso ca x ls)))

(define (cdro cd ls)
  (fresh (x) (conso x cd ls)))

(define (membero x xs)
  (fresh (ca cd) (conso ca cd xs) (conde ((== x ca)) ((membero x cd)))))

(define (rembero x l out)
  (matche l
          [() (== out '())]
          [(,h . ,t)
           (fresh (res)
                  (rembero x t res)
                  (conde [(== x h) (== out res)] [(=/= x h) (== out `(,h . ,res))]))]))

(define (appendo l s out)
  (conde ((== l '()) (== s out))
         ((fresh (ca cd tl) (conso ca cd l) (conso ca tl out) (appendo cd s tl)))))

(defmatche (lengtho xs n) [(() ())] [((,_a . ,d) ,n) (fresh (n^) (lengtho d n^) (inco n^ n))])

(defmatche (lookup-firsto _x _env _v)
           [(,x ((,x . ,v) . ,_rest) ,v)]
           [(,x ((,y . ,_v) . ,rest) ,v) (=/= x y) (lookup-firsto x rest v)])

(define (lookup2o x env t)
  (matche env [((,y . ,v) . ,rest) (== x y) (== t v)] [((,_y . ,_v) . ,rest) (lookup2o x rest t)]))

(define (mapo p xs ys)
  (conde [(== xs '()) (== ys '())]
         [(fresh (h l h^ l^) (== xs `(,h . ,l)) (== ys `(,h^ . ,l^)) (p h h^) (mapo p l l^))]))

(define (allo p xs)
  (conde [(== xs '())] [(fresh (h l) (== xs `(,h . ,l)) (p h) (allo p l))]))

(define (inco n m)
  (pluso n (build-num 1) m))

(define (deco n m)
  (inco m n))

(define (ntho n xs v)
  (nth-helpero '() n xs v))

(define (nth-helpero n m xs v)
  (conde [(== n m) (caro v xs)]
         [(fresh (n^ xs^) (inco n n^) (cdro xs^ xs) (nth-helpero n^ m xs^ v))]))
