#lang racket
(provide test)

(define test-failed (make-parameter #f))

(define-syntax test
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ([expected expected-result]
              [produced tested-expression])
         (or (equal? expected produced)
             (begin
               (test-failed #t)
               (printf "Failed: ~s~%Expected: ~s~%Computed: ~s~%"
                       'tested-expression
                       expected
                       produced)))))]))
