(load "relational-interpreter.scm")
(load "miniKanren/test-check.scm")

(test "reverse"
  (run 1 (q) (wro `(,q ,(lst 1 2 3)) (lst 3 2 1)))
  '(((foldl cons) nil)))

(test "append"
  (run 1 (q) (wro `((,q ,(lst 1 2)) ,(lst 3 4)) (lst 1 2 3 4)))
  '((flip (foldr cons))))
