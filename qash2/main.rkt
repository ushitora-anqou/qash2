#lang racket

(module reader racket
  (require syntax/strip-context)

  (provide (rename-out [qash2-read read]
                       [qash2-read-syntax read-syntax]))

  (define (qash2-read in)
    (syntax->datum
      (qash2-read-syntax #f in)))

  (define (qash2-read-syntax src in)
    (with-syntax ([str (port->string in)])
      (strip-context ; read-syntax 関数は lexical context を持たない syntax object を返さなければならない。
        #'(module anything racket
            (provide data2)
            (define data2 'str))))))
