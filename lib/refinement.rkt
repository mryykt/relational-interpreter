#lang racket

(require minikanren)
(require minikanren/matche)
(require "utils.rkt")
(require "test-check.rkt")
(require racket/generator)

(provide typedo
         substitutiono
         impo)

(defrel (typedo _exp env _t)
        (matche (_exp _t)
                [(⊤ bool)]
                [(⊥ bool)]
                [((¬ ,e) bool) (typedo e env 'bool)]
                [((,e1 ∧ ,e2) bool) (typedo e1 env 'bool) (typedo e2 env 'bool)]
                [((,e1 = ,e2) bool) (fresh (t) (typedo e1 env t) (typedo e2 env t))]
                [((,e1 <= ,e2) bool) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 + ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 - ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((,e1 * ,e2) int) (typedo e1 env 'int) (typedo e2 env 'int)]
                [((len ,e) int) (fresh (t) (typedo e env `(list ,t)))]
                [(,n int) (numbero n)]
                [(,x ,t) (symbolo x) (fresh (r) (lookup-firsto x env r) (refinement-plaino r t))]))

(defrel
 (refinement-plaino r t)
 (matche (r t)
         [(,s ,s) (membero s '(int char bool))]
         [((list ,r^) (list ,t^)) (refinement-plaino r^ t^)]
         [((,_x ,b ,_r) ,b^) (refinement-plaino b b^)]
         [((,_x ,r1 -> ,r2) (,t1 -> ,t2)) (refinement-plaino r1 t1) (refinement-plaino r2 t2)]))

(defrel (substitutiono x e _exp _exp^)
        (matche (_exp _exp^)
                [(,y ,y^) (== y x) (literalo y) (== y^ e)]
                [(,y ,y) (=/= y x) (literalo y)]
                [((¬ ,exp) (¬ ,exp^)) (substitutiono x e exp exp^)]
                [((,e1 ,op ,e2) (,e1^ ,op ,e2^))
                 (membero op '(∧ = <= + - *))
                 (substitutiono x e e1 e1^)
                 (substitutiono x e e2 e2^)]))

(defrel (literalo e) (matche e [,_e (symbolo e)] [,_e (numbero e)] [(len ,xs) (symbolo xs)]))

(defrel
 (impo _e1 _e2 env)
 (matche (_e1 _e2) [(,_e1 ⊤)] [(,e1 ,e2) (=/= '⊤ e2) (project (e1 e2 env) (== #t (imp e1 e2 env)))]))

(define (imp e1 e2 env)
  (let* ([vars (remove-duplicates (get-vars `(,e1 ∧ ,e2)))]
         [tenv (map (lambda (x) (cons x (lookup x env))) vars)])
    (for/and ([venv (env-generator tenv)])
      (if (equal? '⊥ (eval e1 venv))
          #t
          (equal? '⊤ (eval e2 venv))))))

(define (get-model exp env)
  (let ([vars (remove-duplicates (get-vars exp))]) '()))

(define (type-domain ty)
  (match ty
    [(list _ 'bool _) '(⊥ ⊤)]
    [(list _ 'int _) '(0 1 2)]
    [(list _ (list 'list _) _) '(0 1 2)]))

(define (typed-env->domains tenv)
  (for/list ([p tenv])
    (cons (car p) (type-domain (cdr p)))))

(define (env-generator typed-env)
  (define domains (typed-env->domains typed-env))
  (define vars (map car domains))
  (define vals (map cdr domains))
  (define bases (map length vals))
  (define max (apply * bases))
  (in-generator
   (for ([i (in-range max)])
     (define env
       (for/list ([v vars]
                  [ds vals]
                  [k (in-naturals)])
         (cons v (list-ref ds (remainder (quotient i (apply * (take bases k))) (length ds))))))
     (yield env))))

(define (get-vars exp)
  (match exp
    ['⊤ '()]
    ['⊥ '()]
    [(list '¬ e) (get-vars e)]
    [(list e1 op e2)
     #:when (member op '(∧ = <= + - *))
     (append (get-vars e1) (get-vars e2))]
    [(list 'len xs) `(,xs)]
    [_
     (cond
       [(number? exp) '()]
       [(symbol? exp) `(,exp)])]))

(define (eval exp env)
  (match exp
    ['⊤ '⊤]
    ['⊥ '⊥]
    [(list '¬ e) (if (equal? '⊤ (eval e env)) '⊥ '⊤)]
    [(list e1 '∧ e2)
     (let ([v1 (eval e1 env)]
           [v2 (eval e2 env)])
       (if (and (equal? '⊤ v1) (equal? '⊤ v2)) '⊤ '⊥))]
    [(list e1 '= e2)
     (let ([v1 (eval e1 env)]
           [v2 (eval e2 env)])
       (if (equal? v1 v2) '⊤ '⊥))]
    [(list e1 '<= e2)
     (let ([v1 (eval e1 env)]
           [v2 (eval e2 env)])
       (if (< v1 v2) '⊤ '⊥))]
    [(list e1 '+ e2) (+ (eval e1 env) (eval e2 env))]
    [(list e1 '- e2) (- (eval e1 env) (eval e2 env))]
    [(list e1 '* e2) (* (eval e1 env) (eval e2 env))]
    [(list 'len xs)
     #:when (symbol? xs)
     (lookup xs env)]
    [_
     (cond
       [(number? exp) exp]
       [(symbol? exp) (lookup exp env)])]))

(define (lookup x env)
  (match env
    ['() '⊥]
    [(list (cons y v) rest ...)
     (if (equal? x y)
         v
         (lookup x rest))]))

(defrel
 (evalo _exp env _v)
 (matche (_exp _v)
         [(⊤ ⊤)]
         [(⊥ ⊥)]
         [((¬ ,e) ⊥) (evalo e env '⊤)]
         [((¬ ,e) ⊤) (evalo e env '⊥)]
         [((,e1 ∧ ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (matche (v1 v2 v) [(⊤ ⊤ ⊤)] [(⊤ ⊥ ⊥)] [(⊥ ⊤ ⊥)] [(⊥ ⊥ ⊥)]))]
         [((,e1 = ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (conde [(== v1 v2) (== v '⊤)] [(=/= v1 v2) (== v '⊥)]))]
         [((,e1 <= ,e2) ,v)
          (fresh (v1 v2)
                 (evalo e1 env v1)
                 (evalo e2 env v2)
                 (project (v1 v2)
                          (conde [(== (< v1 v2) #t) (== v '⊤)]
                                 [(== (< v1 v2) #f) (== v '⊥)])))] ;<=を勝手に<に変える^o^
         [((,e1 + ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (+ v1 v2))))]
         [((,e1 - ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (- v1 v2))))]
         [((,e1 * ,e2) ,v)
          (fresh (v1 v2) (evalo e1 env v1) (evalo e2 env v2) (project (v1 v2) (== v (* v1 v2))))]
         [(,n ,n) (numbero n)]
         [(,x ,v) (symbolo x) (lookup-firsto x env v)]))

(define (run-test)
  (test "get-vars" (get-vars '((len zs) <= ((len xs) + (len ys)))) '(zs xs ys))
  (test "imp-1"
        (imp '((x <= y) ∧ (y <= z)) '(x <= z) '((x . (_ int ⊤)) (y . (_ int ⊤)) (z . (_ int ⊤))))
        #t)
  (test "imp-2"
        (imp '((x <= y) ∧ (y <= z)) '(z <= x) '((x . (_ int ⊤)) (y . (_ int ⊤)) (z . (_ int ⊤))))
        #f)
  (test "imp-2"
        (imp '((x <= y) ∧ (y <= z)) '(z <= x) '((x . (_ int ⊤)) (y . (_ int ⊤)) (z . (_ int ⊤))))
        #f))
