(load "miniKanren.scm")
(load "miniKanren/numbers.scm")
(load "miniKanren/test-check.scm")

(defrel (vmo i v)
  (vm-helpo (build-num 0) '() '() i v))

(defrel (vm-helpo a s e c v)
  (fresh (a^ s^ e^ c^)
    (vm-stepo a s e c a^ s^ e^ c^)
    (matche c^
      [ () (== v a^) ]
      [ (,hd . ,tl) (vm-helpo a^ s^ e^ c^ v) ])))

(defmatche (vm-stepo accum stack env code accum^ stack^ env^ code^)
  [ (,a ,s ,e ((mkclos ,i) . ,c) (,i . ,e) ,s ,e ,c) ]
  [ (,a ,s ,e (push . ,c) ,a (,a . ,s) ,e ,c) ]
  [ (,a ,s ,e (extend . ,c) ,a ,s ,e^ ,c) (appendo e `(,a) e^) ]
  [ (,a ,s ,e ((search ,n) . ,c) ,v ,s ,e ,c) (ntho n e v) ]
  [ (,a ,s ,e (pushenv . ,c) ,a (,e . ,s) ,e ,c) ]
  [ (,a (,e^ . ,s) ,e (popenv . ,c) ,a ,s ,e^ ,c) ]
  [ ((,i . ,e^) (,w . ,s) ,e (apply . ,c) (,i . ,e^) ,s ,e^^ ,c^) (appendo i c c^) (appendo e `((,i . ,e^) ,w) e^^) ]
  [ (,a ,s ,e ((ldi ,n) . ,c) ,n ,s ,e ,c) ])

(defrel (compileo pcf env vm)
  (matche pcf
    [ (var ,x) (fresh (n) (== vm `((search ,n))) (nth n env x)) ]
    [ (app ,f ,v) (fresh (tl cf cv tl2) (== vm `(pushenv . ,tl))
      (compileo f env cf)
      (compileo v env cv)
      (appendo `(push . ,cf) `(apply popenv) tl2)
      (appendo cv tl2 tl)) ]
    [ (lam ,x ,t) (fresh (ct env^) (== vm `((mkclos ,ct))) (appendo env `(0 ,x) env^) (compileo t env^ ct)) ]
    [ (num ,n) (== vm `((ldi ,n))) ]))

(defrel (evalo exp v)
  (fresh (vm) (compileo exp '() vm) (vmo vm v)))
