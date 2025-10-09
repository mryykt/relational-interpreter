(load "miniKanren/mk-vicare.scm")
(load "miniKanren/mk.scm")
(load "miniKanren/numbers.scm")


(define (conso ca cd ls) (== `(,ca . ,cd) ls))

(define (caro ca ls) (fresh (x) (conso ca x ls)))

(define (cdro cd ls) (fresh (x) (conso x cd ls)))

(define (membero x xs)
  (fresh (ca cd)
    (conso ca cd xs)
    (conde
      ((== x ca))
      ((membero x cd)))))

(define (appendo l s out)
  (conde
    ((== l '()) (== s out))
    ((fresh (ca cd tl)
        (conso ca cd l)
        (conso ca tl out)
        (appendo cd s tl)))))

(defrel (lookupo x env t)
  (conde
    ((fresh (y v rest)
        (== `((,y . ,v) . ,rest) env) (== y x)
        (== v t)))
    ((fresh (y v rest)
        (== `((,y . ,v) . ,rest) env) (=/= y x)
        (lookupo x rest t)))))

(defrel (inco n m)
  (pluso n (build-num 1) m))

(defrel (deco n m)
  (inco m n))

(defrel (ntho n xs v)
  (conde
    [ (zeroo n) (caro v xs) ]
    [ (fresh (n^ xs^) (deco n n^) (cdro xs^ xs) (ntho n^ xs^ v)) ]))
