#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")

(provide typedo
         impo)

(defmatche
 (typedo _exp _env _t)
 [((¬ ,e) ,env bool) (typedo e env 'bool)]
 [((,e1 ∧ ,e2) ,env bool) (typedo e1 env 'bool) (typedo e2 env 'bool)]
 [((,e1 = ,e2) ,env bool) (literalo e1) (fresh (t) (typedo e1 env t) (typedo e2 env t))]
 [((,e1 <= ,e2) ,env bool) (literalo e1) (typedo e1 env 'int) (typedo e2 env 'int)]
 [((,e1 + ,e2) ,env int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
 [((,e1 - ,e2) ,env int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
 [((,e1 * ,e2) ,env int) (literalo e1) (literalo e2) (typedo e1 env 'int) (typedo e2 env 'int)]
 [((len ,xs) ,env int) (fresh (t) (symbolo xs) (lookup-firsto xs env `(list ,t)))]
 [(,n ,_e int) (numbero n)]
 [(,x ,env ,t) (symbolo x) (lookup-firsto x env t)])

(defrel (literalo exp) (matche exp [(len ,e) (symbolo e)] [,n (numbero n)] [,x (symbolo x)]))

(defrel (impo e1 e2 env) (== e1 e2))
