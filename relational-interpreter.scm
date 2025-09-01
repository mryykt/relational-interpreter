(load "miniKanren.scm")

(defmatche (wro exp val)
  [ (,m ,m) ]
  [ (,l ,n) (fresh (m) (stepo l m) (wro m n)) ])

(defmatche (stepo exp val)
  [ (,m ,n) (contractiono m n) ]
  [ ((,m ,n) (,m1 ,n)) (stepo m m1) ]
  [ ((,m ,n) (,m ,n1)) (stepo n n1) ])

(defmatche (contractiono exp val)
  [ ((((foldl ,f) ,acc) ((cons ,x) ,xs)) (((foldl ,f) ((,f ,x) ,acc)) ,xs)) (=/= xs 'nil) ]
  [ ((((foldl ,f) ,acc) ((cons ,x) nil)) ((,f ,x) ,acc)) ]
  [ ((((foldr ,f) ,init) ((cons ,x) ,xs)) ((,f ,x) (((foldr ,f) ,init) ,xs))) (=/= xs 'nil) ]
  [ ((((foldr ,f) ,init) ((cons ,x) nil)) ((,f ,x) ,init)) ]
  [ ((((flip ,f) ,x) ,y) ((,f ,y) ,x)) ]
  [ ((((compose ,f) ,g) ,x) (,f (,g ,x))) ])


(defrel (evalo exp val)
  (conde
    [ (fresh (f f-val acc acc-val xs xs-val)
      (== exp `(((foldl ,f) ,acc) ,xs))
      (evalo f f-val) (evalo acc acc-val) (evalo xs xs-val)
      (foldlo f-val acc-val xs-val val)) ]
    [ (fresh (ca cd val-ca)
      (== exp `((cons ,ca) ,cd))
      (evalo ca val-ca)
      (== val `((cons ,val-ca) ,cd))) ]
    [ (== exp 'nil) (== val 'nil) ]
    [ (numbero exp) (== val exp) ]
    [ (symbolo exp) (== val exp) ]))

(defrel (foldlo f acc xs res)
  (matche xs
    [nil (== acc res) ]
    [ ((cons ,ca) ,cd) (fresh (acc^) (evalo `((,f ,ca) ,acc) acc^) (foldlo f acc^ cd res)) ]))

(define (lst . elms)
  (if
    (null? elms) 'nil
    `((cons ,(car elms)) ,(apply lst (cdr elms)))))
