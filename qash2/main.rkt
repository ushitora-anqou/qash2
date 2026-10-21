#lang racket

(require "parser.rkt")

(define (run filename)
  (call-with-input-file filename
    (lambda (in)
      (displayln (parse in)))))

; cf. https://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29
(module+ main
  (define args (current-command-line-arguments))
  (cond
    [(not (= (vector-length args) 1))
     (raise-user-error 'qash2 "usage: racket main.rkt <file>")]
    [else (run (vector-ref args 0))]))
