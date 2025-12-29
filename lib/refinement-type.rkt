#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

; env⊢exp:t
(defmatche (typedo _exp _env _t)
           [((num ,_n) ,_e int)]
           [((char ,_c) ,_e char)]
           [(true ,_e bool)]
           [(false ,_e bool)]
           [(() ,_e (list ,t))]
           [((var ,x) ,env ,t) (lookup-firsto x env t)]
           [((lam ,x ,e) ,env ,t)]
           [((fix ,f ,x ,e) ,env ,t)]
           [((app ,e1 ,e2) ,env ,t)]
           [((let ,x
               ,e1
               ,e2)
             ,env
             ,t)]
           [((if ,e1 ,e2 ,e3) ,env ,t)]
           [((,e1 = ,e2) ,env ,t)]
           [((,e1 < ,e2) ,env ,t)]
           [((,e1 ,op ,e2) ,env ,t) (membero op '(+ * -))]
           [((cons ,e1 ,e2) ,env ,t)]
           [((car ,e) ,env ,t)]
           [((cdr ,e) ,env ,t)])

; env⊢t
(defmatche (well-formed-typeo _t _env)
           [((,x ,b ,exp) ,env) (simply-typedo exp `((,x . ,b) . ,env) 'bool)]
           [((,x ,s -> ,t) ,env) (well-formed-typeo s env) (well-formed-typeo t `((,x . ,s) . ,env))])

; env⊢s<:t
(defrel (subtypingo t s env) (== 1 1))

; env⊢ s=>t
(defrel (impo t s env) (== 1 1))

; ⊢s:env
(defrel (closing-substitutiono s env) (== 1 1))

; 制約式の単純型付け
(defrel (simply-typedo exp env t) (== 1 1))
