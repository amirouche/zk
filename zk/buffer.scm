(define-module (buffer))

(use-modules (srfi srfi-1))

(use-modules (fingertrees))


;; fingertree procedure specific for the indexed list

(define-public (fingertree-split3* index fingertree)
  (fingertree-split3 (lambda (x) (< index x)) fingertree))

(define-public (fingertree-ref fingertree index)
  (call-with-values (lambda () (fingertree-split3* index fingertree))
    (lambda (head item tail)
      item)))

(define-public (fingertree-split* index fingertree)
  (fingertree-split (lambda (x) (< index x)) fingertree))

(define-public (fingertree-take buffer count)
  (call-with-values (lambda () (fingertree-split* count buffer))
    (lambda (head tail)
      head)))

(define-public (fingertree-drop buffer count)
  (call-with-values (lambda () (fingertree-split* count buffer))
    (lambda (head tail)
      tail)))

(define-public (make-index-list lst) 
  (list->fingertree lst 0 + (lambda (x) 1)))

;; buffer procedures

(define (make-line line)
  (make-index-list (map char->integer (string->list line))))

(define-public (string->buffer string)
  (make-index-list (map make-line (string-split string #\newline))))

(define (line->string line)
  (list->string (map integer->char (fingertree->list line))))

(define-public (buffer->string buffer)
  (string-join (map line->string (fingertree->list buffer)) "\n"))

(define-public buffer-line-count fingertree-measure)

(define-public (buffer-line-length buffer index)
  (fingertree-measure (fingertree-ref buffer index)))

(define-public buffer-take-lines fingertree-take)

(define-public buffer-drop-lines fingertree-drop)

(define-public (buffer-char-insert buffer line column char)
  (call-with-values (lambda () (fingertree-split3* line buffer))
    (lambda (buffer-head line buffer-tail)
      (let ((new (call-with-values (lambda () (fingertree-split* column line))
                   (lambda (head tail)
                     (fingertree-append (fingertree-snoc head char) tail)))))
        (fingertree-append (fingertree-snoc buffer-head new) buffer-tail)))))

(define-public (buffer-char-delete buffer line column)
  (call-with-values (lambda () (fingertree-split3* line buffer))
    (lambda (buffer-head line buffer-tail)
      (let ((new (call-with-values (lambda () (fingertree-split3* column line))
                   (lambda (head _ tail)
                     (fingertree-append head tail)))))
        (fingertree-append (fingertree-snoc buffer-head new) buffer-tail)))))

(define-public %newline (char->integer #\newline))

(define-public (buffer-char-for-each proc buffer)
  (fingertree-fold (lambda (fingertree line)
                     (let ((last (fingertree-fold (lambda (char column)
                                                    (proc column line char)
                                                    (1+ column))
                                                  0
                                                  fingertree)))
                       (proc last line %newline)
                       (1+ line)))
                   0
                   buffer))

(define-public (buffer-merge-lines-at buffer index)
  (call-with-values (lambda () (fingertree-split3* index buffer))
    (lambda (head line tail)
      (call-with-values (lambda () (fingertree-uncons tail))
        (lambda (other tail)
          (fingertree-append head
                             (fingertree-cons (fingertree-append line other)
                                              tail)))))))

(define-public (buffer-split-line-at buffer line column)
  (call-with-values (lambda () (fingertree-split3* line buffer))
    (lambda (head line tail)
      (call-with-values (lambda () (fingertree-split* column line))
        (lambda (first second)
          (fingertree-append (fingertree-snoc (fingertree-snoc head first) second) tail))))))

(define-public (buffer-newline-at buffer index)
  (call-with-values (lambda () (fingertree-split* index buffer))
    (lambda (head tail)
      (fingertree-append head (fingertree-cons (make-index-list (list)) tail)))))
