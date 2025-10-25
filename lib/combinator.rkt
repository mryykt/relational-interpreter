#lang racket
(require minikanren)
(require minikanren/matche)
(require "compile-vm.rkt")

(defmatche (translateo src dst)
           [((,f ,x) (app ,f^ ,x^)) (translateo f f^) (translateo x x^)]
           [(,v (var ,v)) (symbolo v)])
