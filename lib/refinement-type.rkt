#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require (prefix-in r: "refinement.rkt"))
(require "parse.rkt")

(provide typedo
         typed-expo)

(defrel (typedo exp t) (typed-expo exp '() t))

; env⊢exp:t
(defrel
 (typed-expo _exp env _t)
 (matche
  (_exp _t)
  [((num ,_n) ,t) (base-typeo t 'int)]
  [((char ,_c) ,t) (base-typeo t 'char)]
  [(true ,t) (base-typeo t 'bool)]
  [(false ,t) (base-typeo t 'bool)]
  [(() ,t) (fresh (t^) (base-typeo t `(list ,t^)))]
  [((var ,x) ,t) (lookup-firsto x env t)]
  [((lam ,x ,e) (,x ,t1 -> ,t2)) (well-formed-typeo t1 env) (typed-expo e `((,x . ,t1) . ,env) t2)]
  [((fix ,f ,x ,e) (,x ,t1 -> ,t2))
   (well-formed-typeo t1 env)
   (typed-expo e `((,f . (,x ,t1 -> ,t2)) (,x . ,t1) . ,env) t2)]
  [((app ,e1 ,e2) ,t^)
   (fresh (x s t) (typed-expo e1 env `(,x ,s -> ,t)) (typed-expo e2 env s) (substitutiono x e2 t t^))]
  [((let ,x
      ,e1
      ,e2)
    ,t)
   (fresh (s) (typed-expo e1 env s) (typed-expo e2 `((,x . ,s) . ,env) t) (well-formed-typeo t env))]
  [((if ,e1 ,e2 ,e3) ,t)
   (fresh (t1)
          (typed-expo e1 env t1)
          (base-typeo t1 'bool)
          (typed-expo e2 env t)
          (typed-expo e3 env t)
          (well-formed-typeo t env))]
  [((,e1 = ,e2) ,t) (base-typeo t 'bool) (fresh (s) (typed-expo e1 env s) (typed-expo e2 env s))]
  [((,e1 < ,e2) ,t)
   (base-typeo t 'bool)
   (fresh (s) (base-typeo s 'int) (typed-expo e1 env s) (typed-expo e2 env s))]
  [((,e1 ,op ,e2) ,t)
   (base-typeo t 'int)
   (membero op '(+ * -))
   (fresh (s) (typed-expo e1 env s) (typed-expo e2 env s) (base-typeo s 'int))]
  [((cons ,e1 ,e2) ,t)
   (fresh (s) (base-typeo t `(list ,s)) (typed-expo e1 env s) (typed-expo e2 env `(list ,s)))]
  [((car ,e) ,t) (fresh (t^) (typed-expo e env t^) (base-typeo t^ `(list ,t)))]
  [((cdr ,e) ,t) (fresh (t^) (typed-expo e env t) (base-typeo t `(list ,t^)))]
  [(,exp ,t) (fresh (s) (typed-expo exp env s) (subtypingo t s env) (well-formed-typeo t env))]))

(defrel (base-typeo t b) (fresh (x) (== t `(,x ,b ⊤))))

(defrel (substitutiono x e t t^)
        (matche e
                [(num ,n) (fresh (n^) (project (n) (== n^ (unbuild-num n))) (replacemento x n^ t t^))]
                [true (replacemento x '⊤ t t^)]
                [false (replacemento x '⊥ t t^)]
                [(cons ,_a ,_d)
                 (fresh (l n)
                        (const-listo e l)
                        (project (l) (== n (unbuild-num l)) (replacemento '(len ,x) n t t^)))]
                [,_e (not-consto e) (== t t^)]))

; consで始まる式は定数以外ないものとして扱う
(defrel (const-listo xs l)
        (matche xs [()] [(cons ,_a ,d) (fresh (l^) (const-listo d l^) (inco l^ l))]))

(defrel (not-consto e)
        (fresh (x y)
               (=/= e `(num ,x))
               (=/= e `(char ,x))
               (=/= e 'true)
               (=/= e 'false)
               (=/= e `(cons ,x ,y))))

(defrel
 (replacemento x e t t^)
 (matche (t t^)
         [(,b ,b) (symbolo b)]
         [((list ,s) (list ,s^)) (replacemento x e s s^)]
         [((,y ,b ,r) (,y ,b^ ,r^)) (replacemento x e b b^) (r:substitutiono x e r r^)]
         [((,y ,s1 -> ,s2) (,y ,s1^ -> ,s2^)) (replacemento x e s1 s1^) (replacemento x e s2 s2^)]))

; env⊢t
(defrel (well-formed-typeo _t env)
        (matche _t
                [(,x ,b ,exp) (symbolo b) (r:typedo exp `((,x . ,b) . ,env) 'bool)]
                [(,x (list ,s) ,exp)
                 (r:typedo exp `((,x . (list ,s)) . ,env) 'bool)
                 (well-formed-typeo s env)]
                [(,x ,s -> ,t) (well-formed-typeo s env) (well-formed-typeo t `((,x . ,s) . ,env))]))

; env⊢s<:t
(defmatche (subtypingo _t _s _env)
           [((,x ,t1 -> ,t2) (,x ,s1 -> ,s2) ,env)
            (subtypingo t1 s1 env)
            (subtypingo t2 s2 `((,x . ,t1) . ,env))]
           [((list ,t) (list ,s) ,env) (subtypingo t s env)]
           [((,x ,b ,e1) (,x ,b ,e2) ,env) (r:impo e1 e2 `((,x . ,b) . ,env))])

(define (run-test)
  (test "well-formed-type-int" (run 1 (_q) (well-formed-typeo '(x int ⊤) '())) '(_.0))
  (test "well-formed-type-list" (run 1 (_q) (well-formed-typeo '(x (list (y char ⊤)) ⊤) '())) '(_.0))
  (test "well-formed-type-fun"
        (run 1 (_q) (well-formed-typeo '(x (y int ⊤) -> (z bool ⊤)) '()))
        '(_.0))
  (test "well-formed-type-with-refinement"
        (run 1 (_q) (well-formed-typeo '(x int (x <= 0)) '()))
        '(_.0))
  (test "well-formed-type-with-refinement"
        (run 1
             (_q)
             (well-formed-typeo
              '(xs (_1 (list (_2 int ⊤)) ⊤) -> (ys (list (_3 int ⊤)) ((len ys) <= (len xs))))
              '()))
        '(_.0)))
