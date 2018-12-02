(library (zk helpers)
  (export
   pp
   define-syntax-rule
   pk
   compose
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

  (define (compose . procs)
    (let ((procs (reverse procs)))
      (lambda args
        (let loop ((procs (cdr procs))
                   (arg (apply (car procs) args)))
          (if (null? procs)
              arg
              (loop (cdr procs) ((car procs) arg)))))))

  )
