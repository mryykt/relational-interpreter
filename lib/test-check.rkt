#lang racket
(provide test)
(require racket/sandbox)

(define test-failed (make-parameter #f))

(define timeout-second 300)

(define-syntax test
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s " title)
       (flush-output)
       (let* ([expected expected-result]
              [produced
               (with-handlers ([exn:fail:resource? (λ (e) (display (format "~s s " timeout-second)))])
                 (call-with-limits timeout-second
                                   #f
                                   (λ ()
                                     (begin
                                       (define start (current-milliseconds))
                                       (define ret tested-expression)
                                       (define end (current-milliseconds))
                                       (printf "~s ms " (- end start))
                                       ret))))])
         (cond
           [(void? produced) (printf "\e[31m×\e[0m\n")]
           [(equal? expected produced) (printf "\e[32m✔\e[0m\n")]
           [else
            (begin
              (test-failed #t)
              (printf "\e[31m×\e[0m Expected: ~s~%Computed:" expected produced))])))]))
