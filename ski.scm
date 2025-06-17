(load "miniKanren.scm")

(defmatche (wro exp val)
  ((,m ,m))
  ((,l ,n) (fresh (m) (stepo l m) (wro m n))))

(defmatche (stepo exp val)
  ((,m ,n) (contractiono m n))
  (((,m ,n) (,m1 ,n)) (stepo m m1))
  (((,m ,n) (,m ,n1)) (stepo n n1)))

(defmatche (contractiono exp val)
  (((i ,x) ,x))
  ((((k ,x) ,y) ,x))
  (((((s ,x) ,y) ,z) ((,x ,z) (,y ,z)))))
