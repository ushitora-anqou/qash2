#lang racket

(require "parser.rkt")

(provide (rename-out [qash2-read read]
                     [qash2-read-syntax read-syntax]))

(define (qash2-read in)
  (syntax->datum
    (qash2-read-syntax #f in)))

(define (qash2-read-syntax path port)
  (datum->syntax
    #f
    `(module rpcalc-mod racket
       ,(parse port))))

; cf. https://docs.racket-lang.org/guide/Module_Syntax.html?version=9.3#%28part._main-and-test%29
(module+ test
  (require rackunit)
  (check-eq? (last (qash2-read (open-input-string "2 3 7 + *"))) 20))
