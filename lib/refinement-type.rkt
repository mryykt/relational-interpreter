#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require (prefix-in c: "constraint.rkt"))

(defrel (typedo exp t) (typed-expo exp '() t))

; env⊢exp:t
(defmatche
 (typed-expo _exp _env _t)
 [((num ,_n) ,_e ,t) (base-typeo t 'int)]
 [((char ,_c) ,_e ,t) (base-typeo t 'char)]
 [(true ,_e ,t) (base-typeo t 'bool)]
 [(false ,_e ,t) (base-typeo t 'bool)]
 [(() ,_e ,t) (fresh (t^) (base-typeo t `(list ,t^)))]
 [((var ,x) ,env ,t) (lookup-firsto x env t)]
 [((lam ,x ,e) ,env (,x ,t1 -> ,t2))
  (well-formed-typeo t1 env)
  (typed-expo e `((,x . ,t1) . ,env) t2)]
 [((fix ,f ,x ,e) ,env (,x ,t1 -> ,t2))
  (well-formed-typeo t1 env)
  (typed-expo e `((,f . (,x ,t1 -> ,t2)) (,x . ,t1) . ,env) t2)]
 [((app ,e1 ,e2) ,env ,t) (fresh (s) (typed-expo e1 env `(x ,s -> ,t)) (typed-expo e2 env s))] ;TODO
 [((let ,x
     ,e1
     ,e2)
   ,env
   ,t)
  (fresh (s) (typed-expo e1 env s) (typed-expo e2 `((,x . ,s) . ,env) t) (well-formed-typeo t env))]
 [((if ,e1 ,e2 ,e3) ,env ,t)
  (fresh (t1)
         (typed-expo e1 env t1)
         (base-typeo t1 'bool)
         (typed-expo e2 env t)
         (typed-expo e3 env t)
         (well-formed-typeo t env))]
 [((,e1 = ,e2) ,env ,t) (base-typeo t 'bool) (fresh (s) (typed-expo e1 env s) (typed-expo e2 env s))]
 [((,e1 < ,e2) ,env ,t)
  (base-typeo t 'bool)
  (fresh (s) (base-typeo s 'int) (typed-expo e1 env s) (typed-expo e2 env s))]
 [((,e1 ,op ,e2) ,env ,t)
  (base-typeo t 'int)
  (membero op '(+ * -))
  (fresh (s) (typed-expo e1 env s) (typed-expo e2 env s) (base-typeo s 'int))]
 [((cons ,e1 ,e2) ,env ,t)
  (fresh (s) (base-typeo t `(list ,s)) (typed-expo e1 env s) (typed-expo e2 env `(list ,s)))]
 [((car ,e) ,env ,t) (fresh (t^) (typed-expo e env t^) (base-typeo t^ `(list ,t)))]
 [((cdr ,e) ,env ,t) (fresh (t^) (typed-expo e env t) (base-typeo t `(list ,t^)))]
 [(,exp ,env ,t) (fresh (s) (typed-expo exp env s) (subtypingo t s env) (well-formed-typeo t env))])

(defmatche (base-typeo t b) [((,_x ,b ,_exp) ,b)])

; env⊢t
(defmatche (well-formed-typeo _t _env)
           [((,x ,b ,exp) ,env) (c:typedo exp `((,x . ,b) . ,env) 'bool)]
           [((,x ,s -> ,t) ,env) (well-formed-typeo s env) (well-formed-typeo t `((,x . ,s) . ,env))])

; env⊢s<:t
(defmatche (subtypingo _t _s _env)
           [((,x ,t1 -> ,t2) (,x ,s1 -> ,s2) ,env)
            (subtypingo t1 s1 env)
            (subtypingo t2 s2 `((,x . ,t1) . ,env))]
           [((list ,t) (list ,s) ,env) (subtypingo t s env)]
           [((,x ,b ,e1) (,x ,b ,e2) ,env) (c:impo e1 e2 `((,x . ,b) . ,env))])
