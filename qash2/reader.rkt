#lang racket

(require syntax/strip-context)

(provide (rename-out [qash2-read read]
                     [qash2-read-syntax read-syntax]))

(define (qash2-read in)
  (syntax->datum
    (qash2-read-syntax #f in)))

(define (qash2-read-syntax src in)
  (define _ (port->string in))
  (stirp-context
    (with-syntax ([str #'(list "2026-09-20" "マクドナルド" "食費" 1000 "現金" -1000)])
      #'(module anything racket
          (provide data)
          (define data str)))))
