(load "miniKanren.scm")
(load "miniKanren/test-check.scm")

(defrel (minio exp env val)
  (conde
    [ (numbero exp) (== val exp) ]
    [ (symbolo exp) (lookupo exp env val) ]
    [ (caro 'list exp) (== exp val) ]
    [ (fresh (ca cd ca^ cd^)
      (== exp `(cons ,ca ,cd))
      (minio ca env ca^) (minio cd env cd^)
      (matche cd
        [ (list ,xs) (== val `(list (,ca . ,xs))) ])) ]
    [ (caro 'lambda exp) (== exp val) ]
    [ (fresh (f e f^ e^ x body env^)
      (== exp `(app ,f ,e))
      (minio f env f^) (minio e env e^)
      (== f^ `(lambda ,x ,body))
      (conso `(,x . ,e^) env env^)
      (minio body env^ val)) ]
    [ (fresh (x e1 e2 e1^ env^)
      (== exp `(let ,x ,e1 ,e2))
      (minio e1 env e1^)
      (conso `(,x . ,e1^) env env^)
      (minio e2 env^ val)) ]
    [ (fresh (xs e1 e2 ca cd xs^)
      (== exp `(case ,xs ,e1 ,e2))
      (minio xs env xs^)
      (matche xs^
        [ (list ()) (minio e1 env val) ]
        [ (list (,ca . ,cd)) (minio `(app (app ,e2 ,ca) (list ,cd)) env val) ])) ]))
