#lang racket
(require "utils.rkt")
(require minikanren)
(require minikanren/numbers)
(require minikanren/matche)
(require "test-check.rkt")
(provide vmo
         compileo
         evalo
         traceo
         run-test)

(defmatche (vm-stepo accum stack env code accum^ stack^ env^ code^)
           [(,a ,s ,e ((mkclos ,i) . ,c) (,i . ,e) ,s ,e ,c)]
           [(,a ,s ,e (push . ,c) ,a (,a . ,s) ,e ,c)]
           [(,a ,s ,e (extend . ,c) ,a ,s ,e^ ,c) (appendo e `(,a) e^)]
           [(,a ,s ,e ((search ,n) . ,c) ,v ,s ,e ,c) (ntho n e v)]
           [(,a ,s ,e (pushenv . ,c) ,a (,e . ,s) ,e ,c)]
           [(,a (,e^ . ,s) ,e (popenv . ,c) ,a ,s ,e^ ,c)]
           [((,i . ,e^) (,w . ,s) ,e (apply . ,c) (,i . ,e^) ,s ,e^^ ,c^)
            (appendo i c c^)
            (appendo e^ `((,i . ,e^) ,w) e^^)]
           [(,a ,s ,e ((ldi ,n) . ,c) ,n ,s ,e ,c)]
           [(,n (,m . ,s) ,e (add . ,c) ,l ,s ,e ,c) (pluso n m l)]
           [(,n (,m . ,s) ,e (sub . ,c) ,l ,s ,e ,c) (minuso n m l)]
           [(,n (,m . ,s) ,e (mul . ,c) ,l ,s ,e ,c) (*o n m l)]
           [(() ,s ,e ((test ,i ,j) . ,c) () ,s ,e ,c^) (appendo i c c^)]
           [(,n ,s ,e ((test ,i ,j) . ,c) ,n ,s ,e ,c^) (=/= n '()) (appendo j c c^)])

(defrel (vmo i v) (vm-helpo (build-num 0) '() '() i v))

(defrel (vm-helpo a s e c v)
        (fresh (a^ s^ e^ c^)
               (vm-stepo a s e c a^ s^ e^ c^)
               (matche c^ [() (== v a^)] [(,hd . ,tl) (vm-helpo a^ s^ e^ c^ v)])))

(defrel (trace-vmo i v l) (trace-helpo (build-num 0) '() '() i v l))

(defrel (trace-helpo a s e c v l)
        (fresh (a^ s^ e^ c^ l^)
               (vm-stepo a s e c a^ s^ e^ c^)
               (== l `((,a ,s ,e ,c) . ,l^))
               (matche c^
                       [() (== v a^) (== l^ `((,a^ ,s^ ,e^ ,c^)))]
                       [(,hd . ,tl) (trace-helpo a^ s^ e^ c^ v l^)])))

(defrel
 (compileo pcf env vm)
 (matche pcf
         [(var ,x) (fresh (n) (symbolo x) (== vm `((search ,n))) (ntho n env x))]
         [(app ,f ,v)
          (fresh (tl cf cv tl2)
                 (compileo f env cf)
                 (compileo v env cv)
                 (== vm `(pushenv . ,tl))
                 (appendo cv tl2 tl)
                 (appendo `(push . ,cf) `(apply popenv) tl2))]
         [(lam ,x ,t)
          (fresh (ct env^) (== vm `((mkclos ,ct))) (appendo env `(0 ,x) env^) (compileo t env^ ct))]
         [(fix ,f ,x ,t)
          (fresh (ct env^) (== vm `((mkclos ,ct))) (appendo env `(,f ,x) env^) (compileo t env^ ct))]
         [(num ,n) (== vm `((ldi ,n)))]
         [(+ ,u ,v)
          (fresh (cu cv tl)
                 (compileo u env cu)
                 (compileo v env cv)
                 (appendo cv `(push . ,tl) vm)
                 (appendo cu `(add) tl))]
         [(- ,u ,v)
          (fresh (cu cv tl)
                 (compileo u env cu)
                 (compileo v env cv)
                 (appendo cv `(push . ,tl) vm)
                 (appendo cu `(sub) tl))]
         [(* ,u ,v)
          (fresh (cu cv tl)
                 (compileo u env cu)
                 (compileo v env cv)
                 (appendo cv `(push . ,tl) vm)
                 (appendo cu `(mul) tl))]
         [(ifz ,t ,u ,v)
          (fresh (ct cu cv)
                 (compileo t env ct)
                 (compileo u env cu)
                 (compileo v env cv)
                 (appendo ct `((test ,cu ,cv)) vm))]))

(defrel (evalo exp v) (fresh (vm) (compileo exp '() vm) (vmo vm v)))

(defrel (traceo exp v l) (fresh (vm) (compileo exp '() vm) (trace-vmo vm v l)))

(define (run-test)
  (test "test-fun" (run 1 (q) (evalo `(app (lam x (var x)) (num ,(build-num 1))) q)) '((1)))
  (test "test-fun-2"
        (run 1
             (q)
             (evalo `(app (app (lam x (lam y (var y))) (num ,(build-num 1))) (num ,(build-num 2))) q))
        `(,(build-num 2)))
  (test "test-ifz-1"
        (run 1 (q) (evalo `(ifz (num ,(build-num 0)) (num ,(build-num 1)) (num ,(build-num 2))) q))
        `(,(build-num 1)))
  (test "test-ifz-2"
        (run 1 (q) (evalo `(ifz (num ,(build-num 1)) (num ,(build-num 1)) (num ,(build-num 2))) q))
        `(,(build-num 2)))
  (test "test-arithmetic"
        (run 1
             (q)
             (evalo `(+ (num ,(build-num 2))
                        (- (num ,(build-num 10)) (* (num ,(build-num 3)) (num ,(build-num 3)))))
                    q))
        `(,(build-num 3)))
  (test "test-fix"
        (run 1
             (q)
             (evalo `(app (fix f
                               n
                               (ifz (var n)
                                    (num ,(build-num 1))
                                    (* (var n) (app (var f) (- (var n) (num ,(build-num 1)))))))
                          (num ,(build-num 4)))
                    q))
        `(,(build-num 24))))
