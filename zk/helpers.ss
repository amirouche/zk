(library (zk helpers)
  (export
   pp
   define-syntax-rule
   pk
   )
  (import (chezscheme))

  ;; helpers

  (define (pk . args)
    (format (current-error-port) ";; ~a\n" args)
    (car (reverse args)))

  (define (pp . args)
    (pretty-print args)
    (car (reverse args)))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      [(define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           [(keyword args ...) body]))]))

  )
