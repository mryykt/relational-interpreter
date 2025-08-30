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
    [ (fresh (f f-val acc val-acc xs xs-val acc^)
      (== exp `(((foldl ,f) ,acc) ,xs))
      (evalo f f-val) (evalo acc acc-val) (evalo xs xs-val)
      (conde
        [ (fresh (ca cd)
          (== xs-val `((cons ,ca) ,cd))
          (evalo `((,f-val ,ca) ,val-acc) acc^)
          (evalo `(((foldl ,f-val) ,acc^) ,cd) val)) ]
        [ (== xs-val 'nil) (== val val-acc) ])) ]
    [ (fresh (ca cd val-ca)
      (== exp `((cons ,ca) ,cd))
      (evalo ca val-ca)
      (== val `((cons ,val-ca) ,cd))) ]
    [ (== exp 'nil) (== val 'nil) ]
    [ (numbero exp) (== val exp) ]
    [ (symbolo exp) (== val exp) ]))

(define (lst . elms)
  (if
    (null? elms) 'nil
    `((cons ,(car elms)) ,(apply lst (cdr elms)))))
