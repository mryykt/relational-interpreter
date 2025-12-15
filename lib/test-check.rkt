#lang racket
(provide test)
(require racket/sandbox)

(define test-failed (make-parameter #f))

(define timeout-second 300)

(define-syntax test
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ([expected expected-result]
              [produced (with-handlers ([exn:fail:resource?
                                         (λ (e) (displayln (format "Timeout in ~s" timeout-second)))])
                          (call-with-limits timeout-second #f (λ () (time tested-expression))))])
         (or (equal? expected produced)
             (begin
               (test-failed #t)
               (printf "Failed: ~s~%Expected: ~s~%Computed: ~s~%"
                       'tested-expression
                       expected
                       produced)))))]))
