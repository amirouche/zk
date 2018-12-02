(library (zk buffer)
  (export
   fingertree-split3*
   fingertree-ref
   fingertree-split*
   fingertree-take
   fingertree-drop
   string->buffer
   buffer->string
   buffer-line-count
   buffer-line-length
   buffer-take-lines
   buffer-drop-lines
   buffer-char-insert
   buffer-char-delete
   buffer-char-for-each
   buffer-merge-lines-at
   buffer-split-line-at
   buffer-newline-at
   %newline
   string-split
   )
  (import
   (rnrs)
   (only (srfi s13 strings) string-join)
   (zk fingertrees)
   )

  ;; fingertree procedure specific for the indexed list

  (define (fingertree-split3* index fingertree)
    (fingertree-split3 (lambda (x) (< index x)) fingertree))

  (define (fingertree-ref fingertree index)
    (call-with-values (lambda () (fingertree-split3* index fingertree))
      (lambda (head item tail) item)))

  (define (fingertree-split* index fingertree)
    (fingertree-split (lambda (x) (< index x)) fingertree))

  (define (fingertree-take buffer count)
    (call-with-values (lambda () (fingertree-split* count buffer))
      (lambda (head tail)
        head)))

  (define (fingertree-drop buffer count)
    (call-with-values (lambda () (fingertree-split* count buffer))
      (lambda (head tail)
        tail)))

  (define (make-index-list lst)
    (list->fingertree lst 0 + (lambda (x) 1)))

  ;; buffer procedures

  (define (make-line line)
    (make-index-list (map char->integer (string->list line))))

  (define (compose . procs)
    (let ((procs (reverse procs)))
      (lambda (arg)
        (let loop ((procs procs)
                   (arg arg))
          (if (null? procs)
              arg
              (loop (cdr procs)
                    ((car procs) arg)))))))

  (define (string-split string char)
    (let loop ((in (string->list string))
               (out '(())))
      (if (null? in)
          (map (compose list->string reverse) (reverse out))
      (if (char=? (car in) char)
          (loop (cdr in) (cons '() out))
          (loop (cdr in) (cons (cons (car in) (car out)) (cdr out)))))))

  (define (string->buffer string)
    (make-index-list (map make-line (string-split string #\newline))))

  (define (line->string line)
    (list->string (map integer->char (fingertree->list line))))

  (define (buffer->string buffer)
    (string-join (map line->string (fingertree->list buffer)) "\n"))

  (define buffer-line-count fingertree-measure)

  (define (buffer-line-length buffer index)
    (fingertree-measure (fingertree-ref buffer index)))

  (define buffer-take-lines fingertree-take)

  (define buffer-drop-lines fingertree-drop)

  (define (buffer-char-insert buffer line column char)
    (call-with-values (lambda () (fingertree-split3* line buffer))
      (lambda (buffer-head line buffer-tail)
        (let ((new (call-with-values (lambda () (fingertree-split* column line))
                     (lambda (head tail)
                       (fingertree-append (fingertree-snoc head char) tail)))))
          (fingertree-append (fingertree-snoc buffer-head new) buffer-tail)))))

  (define (buffer-char-delete buffer line column)
    (call-with-values (lambda () (fingertree-split3* line buffer))
      (lambda (buffer-head line buffer-tail)
        (let ((new (call-with-values (lambda () (fingertree-split3* column line))
                     (lambda (head _ tail)
                       (fingertree-append head tail)))))
          (fingertree-append (fingertree-snoc buffer-head new) buffer-tail)))))

  (define %newline (char->integer #\newline))

  (define (buffer-char-for-each proc buffer)
    (fingertree-fold (lambda (fingertree line)
                       (let ((last (fingertree-fold (lambda (char column)
                                                      (proc column line char)
                                                      (+ 1 column))
                                                    0
                                                    fingertree)))
                         (proc last line %newline)
                         (+ 1 line)))
                     0
                     buffer))

  (define (buffer-merge-lines-at buffer index)
    (call-with-values (lambda () (fingertree-split3* index buffer))
      (lambda (head line tail)
        (call-with-values (lambda () (fingertree-uncons tail))
          (lambda (other tail)
            (fingertree-append head
                               (fingertree-cons (fingertree-append line other)
                                                tail)))))))

  (define (buffer-split-line-at buffer line column)
    (call-with-values (lambda () (fingertree-split3* line buffer))
      (lambda (head line tail)
        (call-with-values (lambda () (fingertree-split* column line))
          (lambda (first second)
            (fingertree-append (fingertree-snoc (fingertree-snoc head first) second) tail))))))

  (define (buffer-newline-at buffer index)
    (call-with-values (lambda () (fingertree-split* index buffer))
      (lambda (head tail)
        (fingertree-append head (fingertree-cons (make-index-list (list)) tail)))))

  ;; (define (buffer-length)
  ;;   (buffer-line-count %buffer))

  ;; (define (buffer-current-line-length)
  ;;   (buffer-line-length %buffer (cursor-line)))

  ;; (define (buffer-insert! char)
  ;;   (set! %buffer (buffer-char-insert %buffer (cursor-line) (cursor-column) char))
  ;;   (cursor-right!))

  ;; (define (buffer-delete!)
  ;;   (cond
  ;;    ((and (eq? (cursor-line) (- (buffer-length) 1)) (eq? (cursor-column) (buffer-current-line-length)))
  ;;     #f)
  ;;    ((eq? (cursor-column) (buffer-current-line-length)) (buffer-merge-lines!))
  ;;    (else (set! %buffer (buffer-char-delete %buffer (cursor-line) (cursor-column))))))

  ;; (define (buffer-merge-lines!)
  ;;   (set! %buffer (buffer-merge-lines-at %buffer (cursor-line))))

  ;; (define (buffer-newline!)
  ;;   (cond
  ;;    ((eq? (cursor-column) (buffer-current-line-length))
  ;;     (set! %buffer (buffer-newline-at %buffer (+ 1 (cursor-line)))))
  ;;    ((eq? (cursor-column) 0)
  ;;     (set! %buffer (buffer-newline-at %buffer (cursor-line))))
  ;;    (else (set! %buffer (buffer-split-line-at %buffer (cursor-line) (cursor-column)))
  ;;          (cursor-x! 0)))
  ;;   (cursor-down!))

  ;; (define (buffer-backspace!)
  ;;   (cond
  ;;    ((and (eq? (cursor-column) 0) (not (eq? (cursor-line) 0)))
  ;;     (cursor-up!)
  ;;     (cursor-end-of-line!)
  ;;     (buffer-merge-lines!))
  ;;    ((not (and (eq? (cursor-column) 0) (eq? (cursor-line) 0))) (cursor-left!) (buffer-delete!))))

  )
