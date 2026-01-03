#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide typedo
         substitutiono
         impo)

(defrel (typedo _exp env _t)
        (matche (_exp _t)
                [(⊤ bool)]
                [(⊥ bool)]
                [((¬ ,e) bool) (typedo e env 'bool)]
                [((,e1 ∧ ,e2) bool) (typedo e1 env 'bool) (typedo e2 env 'bool)]
                [((,e1 = ,e2) bool) (fresh (t) (typedo e1 env t) (typedo e2 env t))]
                [((,e1 <= ,e2) bool) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 + ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 - ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 * ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((len ,e) int) (fresh (t) (typedo e env `(list ,t)))]
                [(,n int) (numbero n)]
                [(,x ,t) (symbolo x) (fresh (r) (lookup-firsto x env r) (refinement-plaino r t))]))

(defrel
 (refinement-plaino r t)
 (matche (r t)
         [(,s ,s) (membero s '(int char bool))]
         [((list ,r^) (list ,t^)) (refinement-plaino r^ t^)]
         [((,_x ,b ,_r) ,b^) (refinement-plaino b b^)]
         [((,_x ,r1 -> ,r2) (,t1 -> ,t2)) (refinement-plaino r1 t1) (refinement-plaino r2 t2)]))

(defrel (substitutiono x e _exp _exp^)
        (matche (_exp _exp^)
                [(,y ,y^) (== y x) (literalo y) (== y^ e)]
                [(,y ,y) (=/= y x) (literalo y)]
                [((¬ ,exp) (¬ ,exp^)) (substitutiono x e exp exp^)]
                [((,e1 ,op ,e2) (,e1^ ,op ,e2^))
                 (membero op '(∧ = <= + - *))
                 (substitutiono x e e1 e1^)
                 (substitutiono x e e2 e2^)]))

(defrel (literalo e) (matche e [,_e (symbolo e)] [,_e (numbero e)] [(len ,xs) (symbolo xs)]))

(defrel (impo _e1 _e2 env) (matche (_e1 _e2) [(,_e1 ⊤)]))

(defrel
 (evalo _exp env _v)
 (matche (_exp _v)
         [(⊤ ⊤)]
         [(⊥ ⊥)]
         [((¬ ,e) ⊥) (evalo e env '⊤)]
         [((¬ ,e) ⊤) (evalo e env '⊥)]
         [((,e1 ∧ ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (matche (v1 v2 v) [(⊤ ⊤ ⊤)] [(⊤ ⊥ ⊥)] [(⊥ ⊤ ⊥)] [(⊥ ⊥ ⊥)]))]
         [((,e1 = ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (conde [(== v1 v2) (== v '⊤)] [(=/= v1 v2) (== v '⊥)]))]
         [((,e1 <= ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (project (v1 v2)
                          (conde [(== (< v1 v2) #t) (== v '⊤)]
                                 [(== (< v1 v2) #f) (== v '⊥)])))] ;<=を勝手に<に変える^o^
         [((,e1 + ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (+ v1 v2))))]
         [((,e1 - ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (- v1 v2))))]
         [((,e1 * ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (* v1 v2))))]
         [(,n ,n) (numbero n)]
         [(,x ,v) (symbolo x) (lookup-firsto x env v)]))
