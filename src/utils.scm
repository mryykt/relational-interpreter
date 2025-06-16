(load "miniKanren/mk-vicare.scm")
(load "miniKanren/mk.scm")

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
