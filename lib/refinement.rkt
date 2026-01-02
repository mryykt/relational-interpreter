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
                [(,y ,y^) (== y x) (== y^ e)]
                [(,y ,y) (=/= y x)]
                [((¬ ,exp) (¬ ,exp^)) (substitutiono x e exp exp^)]
                [((,e1 ,op ,e2) (,e1^ ,op ,e2^))
                 (membero op '(∧ = <= + - *))
                 (substitutiono x e e1 e1^)
                 (substitutiono x e e2 e2^)]
                [((len ,xs) (len ,ys)) (== xs x) (== ys e)]))

(defrel (impo _e1 _e2 env) (matche (_e1 _e2) [(,_e1 ⊤)]))
