(import (srfi s9 records))
(import (matchable))

(import (zk termbox))
(import (zk buffer))
(import (zk helpers))

(define %error (open-file-output-port "out.log"
                                      (file-options no-fail)
                                      (buffer-mode line)
                                      (make-transcoder (utf-8-codec))))

(define (dg . rest)
  (write rest %error) (newline %error)
  (car (reverse rest)))

;; buffer

(define %filename "zk.scm")

(define (read-string port)
  (let loop ((out '()))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (list->string (reverse out))
          (loop (cons char out))))))

(define (filename->buffer filename)
  (string->buffer (call-with-input-file filename read-string)))

(define %buffer (filename->buffer %filename))

(define (buffer-length)
  (buffer-line-count %buffer))

(define (buffer-current-line-length)
  (buffer-line-length %buffer (cursor-line)))

(define (buffer-insert! char)
  (set! %buffer (buffer-char-insert %buffer (cursor-line) (cursor-column) char))
  (cursor-right!))

(define (buffer-delete!)
  (cond
   ((and (eq? (cursor-line) (- (buffer-length) 1)) (eq? (cursor-column) (buffer-current-line-length)))
    #f)
   ((eq? (cursor-column) (buffer-current-line-length)) (buffer-merge-lines!))
   (else (set! %buffer (buffer-char-delete %buffer (cursor-line) (cursor-column))))))

(define (buffer-merge-lines!)
  (set! %buffer (buffer-merge-lines-at %buffer (cursor-line))))

(define (buffer-newline!)
  (cond
   ((eq? (cursor-column) (buffer-current-line-length))
    (set! %buffer (buffer-newline-at %buffer (+ 1 (cursor-line)))))
   ((eq? (cursor-column) 0)
    (set! %buffer (buffer-newline-at %buffer (cursor-line))))
   (else (set! %buffer (buffer-split-line-at %buffer (cursor-line) (cursor-column)))
         (cursor-x! 0)))
  (cursor-down!))

(define (buffer-backspace!)
  (cond
   ((and (eq? (cursor-column) 0) (not (eq? (cursor-line) 0)))
    (cursor-up!)
    (cursor-end-of-line!)
    (buffer-merge-lines!))
   ((not (and (eq? (cursor-column) 0) (eq? (cursor-line) 0))) (cursor-left!) (buffer-delete!))))

;; cursor
(define %cursor (cons 0 0))

(define (cursor-x)
  (car %cursor))

(define (cursor-y)
  (cdr %cursor))

(define (cursor-x! x)
  (set! %cursor (cons x (cursor-y))))

(define (cursor-y! y)
  (set! %cursor (cons (cursor-x) y)))

(define (cursor-down!)
  (when (<= (+ (view-y) (cursor-y)) (- (buffer-length) 2))
    (cond
     ((eq? (cursor-y) (- (tb-height) 2)) (view-down!))
     (else (set! %cursor (cons (car %cursor) (+ 1 (cdr %cursor))))))
    (cursor-x! (min (cursor-x) (buffer-current-line-length)))))

(define (cursor-up!)
  (cond
   ((and (eq? (cursor-y) 0) (eq? (view-y) 0)) #f)
   ((eq? (cursor-y) 0) (view-up!))
   (else (cursor-y! (- (cursor-y) 1))))
  (cursor-x! (min (cursor-x) (buffer-current-line-length))))

(define (cursor-left!)
  (cond
   ((and (eq? (view-x) 0) (eq? (cursor-x) 0)) #f)
   (else (set! %cursor (cons (- (car %cursor) 1) (cdr %cursor))))))

(define (cursor-right!)
  (cursor-x! (min (+ 1 (cursor-x)) (buffer-current-line-length))))

(define (cursor-end-of-line!)
  (cursor-x! (buffer-current-line-length)))

(define (cursor-start-of-line!)
  (cursor-x! 0))

(define (cursor-line)
  (+ (cursor-y) (view-y)))

(define (cursor-column)
  (+ (cursor-x) (view-x)))

;; view

(define %view (cons 0 0))

(define (view-x)
  (car %view))

(define (view-y)
  (cdr %view))

(define (view-down!)
  (set! %view (cons (car %view) (+ 1 (cdr %view)))))

(define (view-up!)
  (set! %view (cons (car %view) (- (cdr %view) 1))))

(define (view-left!)
  (set! %view (cons (- (car %view) 1) (cdr %view))))

(define (view-right!)
  (set! %view (cons (+ 1 (car %view)) (cdr %view))))

(define (print line string)
  (let ((chars (map char->integer (string->list string)))
        (max (tb-width)))
    (let loop ((index 0)
               (chars chars))
      (unless (or (null? chars) (eq? index max))
        (tb-change-cell index line (car chars) TB-WHITE TB-DEFAULT)
        (loop (+ 1 index) (cdr chars))))))

(define %styles (make-hashtable equal-hash equal?))

(define-record-type <cell>
  (make-cell x y char)
  char?
  (x cell-x)
  (y cell-y)
  (char cell-char))

(define (tokenize string)
  (map char->integer (string->list string)))

(define %chars (make-hashtable equal-hash equal?))

(for-each (lambda (char) (hashtable-set! %chars char #t)) (tokenize "azertyuiopqsdfghjklmwxcvbn!*-"))

(define (char? char)
  (hashtable-ref %chars char #f))

(define %keywords (make-hashtable equal-hash equal?))

(for-each (lambda (string) (hashtable-set! %keywords (tokenize string) #t))
          '("begin" "call-with-current-continuation" "call/cc"
            "call-with-input-file" "call-with-output-file" "case" "cond"
            "do" "else" "for-each" "if" "lambda"
            "let" "let*" "let-syntax" "letrec"
            ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
            "and" "or" "delay" "force"
            ;; Stefan Monnier <stefan.monnier@epfl.ch> says don't bother:
            ;;"quasiquote" "quote" "unquote" "unquote-splicing"
            "map" "syntax" "syntax-rules"
            ;; others
            "define-module" "define" "define-public" "define-record-type" "use-modules"
            "catch" "throw" "eq?" "eqv?" "equal?" "set!" "when" "unless" "match"
            "call-with-values"
            ))

(define (keyword? token)
  (hashtable-ref %keywords token #f))

(define (maybe-highlight! token)
  (when (keyword? (map cell-char token))
    (let ((x (cell-x (car token)))
          (y (cell-y (car token))))
      (for-each (lambda (index)
                  (hashtable-set! %styles (cons (+ x index) y) TB-RED))
                (iota (length token))))))


(define (highlight-keywords!)
  (let* ((token '())
         (start-y (view-y))
         ;; keep lines that are visible
         (buffer (buffer-take-lines (buffer-drop-lines %buffer start-y) (- (tb-height) 1))))
    (buffer-char-for-each
     (lambda (x y char)
       (if (char? char)
           (set! token (cons (make-cell x (+ start-y y) char) token))
           (begin (maybe-highlight! (reverse token))
                  (set! token '()))))
     buffer)))

(define %semicolon (char->integer #\;))

(define (highlight-comments!)
  (let* ((comment? #f)
         (start-y (view-y))
         ;; keep lines that are visible
         (buffer (buffer-take-lines (buffer-drop-lines %buffer start-y) (- (tb-height) 1))))
    (buffer-char-for-each
     (lambda (x y char)
       (cond
        ((eq? char %newline) (set! comment? #f))
        (comment? (hashtable-set! %styles (cons x (+ start-y y)) TB-YELLOW))
        ((eq? char %semicolon)
         (hashtable-set! %styles (cons x (+ start-y y)) TB-YELLOW)
         (set! comment? #t))
        (else #f)))
     buffer)))

(define (highlight!)
  (set! %styles (make-hashtable equal-hash equal?))
  (highlight-keywords!)
  (highlight-comments!))

(define %level 0)

(define %open (char->integer #\())
(define %close (char->integer #\)))

(define %color-index (list TB-BLUE TB-GREEN TB-CYAN TB-MAGENTA TB-YELLOW))

(define (level->color)
  (list-ref %color-index (modulo %level (length %color-index))))

(define (render-char x y char)
  (unless (eq? char %newline)
    (let ((color (or (hashtable-ref %styles (cons x y) #f) TB-WHITE)))
      (cond
       ((eq? char %open)
        (set! color (level->color))
        (set! %level (+ 1 %level)))
       ((eq? char %close)
        (set! %level (- %level 1))
        (set! color (level->color))))
      (when (and (>= x (view-x))
                 (< x (+ (view-x) (tb-width)))
                 (>= y (view-y))
                 (< y (+ (view-y) (- (tb-height) 1))))
        (tb-change-cell (- x (view-x)) (- y (view-y)) char color TB-DEFAULT)))))

(define (render)
  (set! %level 0)
  (buffer-char-for-each render-char %buffer)
  ;; print status bar and separator
  (print (- (tb-height) 1)
         (string-append " - "
                        %filename
                        " - "
                        (number->string (+ 1 (cursor-line)))
                        "/"
                        (number->string (+ 1 (cursor-column)))
                        " -")))

;; event handling

(define (quit)
  (tb-shutdown)
  (exit))

(define (save!)
  (call-with-output-file %filename
    (lambda (port)
      (display (buffer->string %buffer) port))))

(define key-table
  (make-eq-hashtable))

(hashtable-set! key-table   TB-KEY-CTRL-Q	 (lambda () (quit)))
(hashtable-set! key-table   TB-KEY-CTRL-S	 (lambda () (save!)))
(hashtable-set! key-table   TB-KEY-CTRL-E	 (lambda () (cursor-end-of-line!)))
(hashtable-set! key-table   TB-KEY-CTRL-A	 (lambda () (cursor-start-of-line!)))
(hashtable-set! key-table   TB-KEY-CTRL-D	 (lambda () (buffer-delete!)))
(hashtable-set! key-table   TB-KEY-DELETE	 (lambda () (buffer-delete!)))
(hashtable-set! key-table   TB-KEY-ENTER	 (lambda () (buffer-newline!)))
(hashtable-set! key-table   TB-KEY-SPACE	 (lambda () (buffer-insert! (char->integer #\space))))
(hashtable-set! key-table   TB-KEY-BACKSPACE2	 (lambda () (buffer-backspace!)))
(hashtable-set! key-table   TB-KEY-ARROW-UP	 (lambda () (cursor-up!)))
(hashtable-set! key-table   TB-KEY-ARROW-DOWN	 (lambda () (cursor-down!)))
(hashtable-set! key-table   TB-KEY-ARROW-LEFT	 (lambda () (cursor-left!)))
(hashtable-set! key-table   TB-KEY-ARROW-RIGHT	 (lambda () (cursor-right!)))
(hashtable-set! key-table   TB-KEY-CTRL-P	 (lambda () (cursor-up!)))
(hashtable-set! key-table   TB-KEY-CTRL-N	 (lambda () (cursor-down!)))
(hashtable-set! key-table   TB-KEY-CTRL-B	 (lambda () (cursor-left!)))
(hashtable-set! key-table   TB-KEY-CTRL-F	 (lambda () (cursor-right!)))

(define (dispatch)
  (match (dg (tb-poll-event))
    (($ <event-char> char)
     (buffer-insert! (char->integer char)))
    (($ <event-key> mode key)
     (dg mode key)
     ((hashtable-ref key-table key void)))
    (else #f)))

;; main loop

(define (main)
  (let loop ()
    (tb-clear)
    (highlight!)
    (render)
    (tb-set-cursor (cursor-x) (cursor-y))
    (tb-present)
    (dispatch)
    (loop)))


(tb-init)

(main)
