(load "miniKanren.scm")

(defmatche (wro exp val)
  [ (,m ,m) ]
  [ (,l ,n) (fresh (m) (stepo l m) (wro m n)) ])

(defmatche (stepo exp val)
  [ (,m ,n) (contractiono m n) ]
  [ ((,m ,n) (,m1 ,n)) (stepo m m1) ]
  [ ((,m ,n) (,m ,n1)) (stepo n n1) ])

(defmatche (contractiono exp val)
  [ ((((foldl ,f) ,acc) ((cons ,x) ,xs)) (((foldl ,f) ((,f ,x) ,acc)) ,xs)) ]
  [ ((((foldl ,f) ,acc) nil) ,acc) ]
  [ ((((foldr ,f) ,init) ((cons ,x) ,xs)) ((,f ,x) (((foldr ,f) ,init) ,xs))) ]
  [ ((((foldr ,f) ,init) nil) ,init) ]
  [ ((((flip ,f) ,x) ,y) ((,f ,y) ,x)) ])
