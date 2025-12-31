#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide typedo
         impo)

(defrel
 (typedo _exp env _t)
 (matche (_exp _t)
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

(defrel (literalo exp) (matche exp [(len ,e) (symbolo e)] [,n (numbero n)] [,x (symbolo x)]))

(defrel (impo e1 e2 env) (== e1 e2))
