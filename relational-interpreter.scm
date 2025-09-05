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
  [ (((map ,f) ((cons ,x) ,xs)) ((cons (,f ,x)) ((map ,f) ,xs))) (=/= xs 'nil) ]
  [ (((map ,f) ((cons ,x) nil)) ((cons (,f ,x)) nil)) ]
  [ ((((flip ,f) ,x) ,y) ((,f ,y) ,x)) ]
  [ ((((compose ,f) ,g) ,x) (,f (,g ,x))) ])


(defrel (evalo exp val)
  (conde
    [ (fresh (f f^ acc acc^ xs xs^ acc2)
      (== exp `(((foldl ,f) ,acc) ,xs))
      (evalo f f^) (evalo acc acc^) (evalo xs xs^)
      (matche xs^
        [ (list ()) (== val acc^) ]
        [ (list (,ca . ,cd))
        (evalo `((,f^ ,ca) ,acc^) acc2)
        (evalo `(((foldl ,f^) ,acc2) (list ,cd)) val) ])) ]
    [ (fresh (ca cd ca^ cd^)
      (== exp `((cons ,ca) ,cd))
      (evalo ca ca^)
      (evalo cd cd^)
      (matche cd^
        [ (list ,xs) (== val `(list (,ca^ . ,xs))) ])) ]
    [ (caro 'list exp) (== val exp) ]
    [ (numbero exp) (== val exp) ]
    [ (symbolo exp) (== val exp) ]))

(define (lst . elms)
  (if
    (null? elms) 'nil
    `((cons ,(car elms)) ,(apply lst (cdr elms)))))
