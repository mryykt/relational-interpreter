#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require (prefix-in r: "refinement.rkt"))

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
   (fresh (x s t)
          (substitutiono x e2 t env t^)
          (typed-expo e1 env `(,x ,s -> ,t))
          (typed-expo e2 env s))] ;TODO
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

(defrel
 (substitutiono x e t env t^)
 (matche (t t^)
         [((,y ,t1 -> ,t2) (,y ,t1^ -> ,t2^))
          (conde [(== y x) (== t1 t1^) (== t2 t2^)]
                 [(=/= y x) (substitutiono x e t1 env t1^) (substitutiono x e t2 env t2^)])])) ;TODO

; env⊢t
(defmatche (well-formed-typeo _t _env)
           [((,x ,b ,exp) ,env) (r:typedo exp `((,x . ,b) . ,env) 'bool)]
           [((,x ,s -> ,t) ,env) (well-formed-typeo s env) (well-formed-typeo t `((,x . ,s) . ,env))])

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
        '(_.0)))
