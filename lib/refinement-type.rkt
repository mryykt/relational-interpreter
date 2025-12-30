#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")

; env⊢exp:t
(defmatche
 (typedo _exp _env _t)
 [((num ,_n) ,_e ,t) (base-typeo t 'int)]
 [((char ,_c) ,_e ,t) (base-typeo t 'char)]
 [(true ,_e ,t) (base-typeo t 'bool)]
 [(false ,_e ,t) (base-typeo t 'bool)]
 [(() ,_e (list ,t))]
 [((var ,x) ,env ,t) (lookup-firsto x env t)]
 [((lam ,x ,e) ,env (,x ,s -> ,t)) (well-formed-typeo s env) (typedo e `((,x . ,s) . ,env) t)]
 [((fix ,f ,x ,e) ,env (,x ,s -> ,t))
  (well-formed-typeo s env)
  (typedo e `((,f . (,x ,s -> ,t)) (,x . ,s) . ,env) t)]
 [((app ,e1 ,e2) ,env ,t) (fresh (s) (typedo e1 env `(x ,s -> ,t)) (typedo e2 env s))] ;TODO
 [((let ,x
     ,e1
     ,e2)
   ,env
   ,t)
  (fresh (s) (typedo e1 env s) (typedo e2 `((,x . ,s) . ,env) t) (well-formed-typeo t env))]
 [((if ,e1 ,e2 ,e3) ,env ,t)
  (fresh (t1)
         (typedo e1 env t1)
         (base-typeo t1 'bool)
         (typedo e2 env t)
         (typedo e3 env t)
         (well-formed-typeo t env))]
 [((,e1 = ,e2) ,env ,t) (base-typeo t 'bool) (fresh (s) (typedo e1 env s) (typedo e2 env s))]
 [((,e1 < ,e2) ,env ,t)
  (base-typeo t 'bool)
  (fresh (s) (base-typeo s 'int) (typedo e1 env s) (typedo e2 env s))]
 [((,e1 ,op ,e2) ,env ,t)
  (base-typeo t 'int)
  (membero op '(+ * -))
  (fresh (s) (typedo e1 env s) (typedo e2 env s) (base-typeo s 'int))]
 [((cons ,e1 ,e2) ,env ,t) (fresh (s) (== t `(list ,s)) (typedo e1 env s) (typedo e2 env `(list ,s)))]
 [((car ,e) ,env ,t) (typedo e env `(list ,t))]
 [((cdr ,e) ,env (list ,t)) (typedo e env `(list ,t))]
 [(,exp ,env ,t) (fresh (s) (typedo exp env s) (subtypingo t s env) (well-formed-typeo t env))])

(defmatche (base-typeo t b) [((,_x ,b ,_exp) ,b) (membero b '(int char bool))])

; env⊢t
(defmatche (well-formed-typeo _t _env)
           [((,x ,b ,exp) ,env) (simply-typedo exp `((,x . ,b) . ,env) 'bool)]
           [((list ,t) ,env) (well-formed-typeo t env)]
           [((,x ,s -> ,t) ,env) (well-formed-typeo s env) (well-formed-typeo t `((,x . ,s) . ,env))])

; env⊢s<:t
(defrel (subtypingo t s env) (== t s))

; env⊢ s=>t
(defrel (impo t s env) (== t s))

; ⊢s:env
(defrel (closing-substitutiono s env) (== 1 1))

; 制約式の単純型付け
(defrel (simply-typedo exp env t) (== t 'bool))
