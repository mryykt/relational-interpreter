#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide typedo
         substitutiono
         impo)

(defrel
 (typedo _exp env _t)
 (matche (_exp _t)
         [(⊤ bool)]
         [(⊥ bool)]
         [((¬ ,e) bool) (typedo e env 'bool)]
         [((,e1 ∧ ,e2) bool) (typedo e1 env 'bool) (typedo e2 env 'bool)]
         [((,e1 = ,e2) bool) (literalo e1) (fresh (t) (typedo e1 env t) (typedo e2 env t))]
         [((,e1 <= ,e2) bool) (literalo e1) (typedo e1 env 'int) (typedo e2 env 'int)]
         [((,e1 + ,e2) int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
         [((,e1 - ,e2) int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
         [((,e1 * ,e2) int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
         [((len ,xs) int) (fresh (t) (symbolo xs) (lookup-firsto xs env `(list ,t)))]
         [(,n int) (numbero n)]
         [(,x ,t) (symbolo x) (lookup-firsto x env t)]))

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

(defrel (literalo exp) (matche exp [(len ,e) (symbolo e)] [,n (numbero n)] [,x (symbolo x)]))

(defrel (impo _e1 _e2 env) (matche (_e1 _e2) [(,_e1 ⊤)]))
