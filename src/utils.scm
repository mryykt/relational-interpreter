(load "miniKanren/mk.scm")

(define (conso ca cd ls) (== (cons ca cd) ls))

(define (caro ca ls) (fresh (x) (conso ca x ls)))

(define (cdro cd ls) (fresh (x) (conso x cd ls)))

(define (membero x xs)
  (fresh (ca cd)
    (conso ca cd xs)
    (conde
      ((== x ca))
      ((membero x cd)))))
