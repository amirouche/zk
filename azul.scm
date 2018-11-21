(define-module (azul))

(use-modules (srfi srfi-9))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(use-modules (termbox))
(use-modules (buffer))

(define (dg . rest)
  (write rest (current-error-port)) (newline (current-error-port))
  (car (reverse rest)))

(define call-with-sigint
  (if (not (provided? 'posix))
      (lambda (thunk handler-thunk) (thunk))
      (lambda (thunk handler-thunk)
        (let ((handler #f))
          (catch 'interrupt
            (lambda ()
              (dynamic-wind
                (lambda ()
                  (set! handler
                        (sigaction SIGINT (lambda (sig) (throw 'interrupt)))))
                thunk
                (lambda ()
                  (if handler
                      ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                      (sigaction SIGINT (car handler) (cdr handler))
                      ;; restore original C handler.
                      (sigaction SIGINT #f)))))
            (lambda (k . _) (handler-thunk)))))))

;; buffer

(define %filename (cadr (program-arguments)))

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
   ((and (eq? (cursor-line) (1- (buffer-length))) (eq? (cursor-column) (buffer-current-line-length)))
    #f)
   ((eq? (cursor-column) (buffer-current-line-length)) (buffer-merge-lines!))
   (else (set! %buffer (buffer-char-delete %buffer (cursor-line) (cursor-column))))))

(define (buffer-merge-lines!)
  (set! %buffer (buffer-merge-lines-at %buffer (cursor-line))))

(define (buffer-newline!)
  (cond
   ((eq? (cursor-column) (buffer-current-line-length))
    (set! %buffer (buffer-newline-at %buffer (1+ (cursor-line)))))
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
     (else (set! %cursor (cons (car %cursor) (1+ (cdr %cursor))))))
    (cursor-x! (min (cursor-x) (buffer-current-line-length)))))

(define (cursor-up!)
  (cond
   ((and (eq? (cursor-y) 0) (eq? (view-y) 0)) #f)
   ((eq? (cursor-y) 0) (view-up!))
   (else (cursor-y! (1- (cursor-y)))))
  (cursor-x! (min (cursor-x) (buffer-current-line-length))))

(define (cursor-left!)
  (cond
   ((and (eq? (view-x) 0) (eq? (cursor-x) 0)) #f)
   (else (set! %cursor (cons (1- (car %cursor)) (cdr %cursor))))))

(define (cursor-right!)
  (cursor-x! (min (1+ (cursor-x)) (buffer-current-line-length))))

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
  (set! %view (cons (car %view) (1+ (cdr %view)))))

(define (view-up!)
  (set! %view (cons (car %view) (1- (cdr %view)))))

(define (view-left!)
  (set! %view (cons (1- (car %view)) (cdr %view))))

(define (view-right!)
  (set! %view (cons (1+ (car %view)) (cdr %view))))

(define (print line string)
  (let ((chars (map char->integer (string->list string)))
        (max (tb-width)))
    (let loop ((index 0)
               (chars chars))
      (unless (or (null? chars) (eq? index max))
        (tb-change-cell index line (car chars) TB-WHITE TB-DEFAULT)
        (loop (1+ index) (cdr chars))))))

(define %styles (make-hash-table))

(define-record-type <cell>
  (make-cell x y char)
  char?
  (x cell-x)
  (y cell-y)
  (char cell-char))

(define (tokenize string)
  (map char->integer (string->list string)))

(define %chars (make-hash-table))

(for-each (lambda (char) (hash-set! %chars char #t)) (tokenize "azertyuiopqsdfghjklmwxcvbn!*-"))

(define (char? char)
  (hash-ref %chars char))

(define %keywords (make-hash-table))

(for-each (lambda (string) (hash-set! %keywords (tokenize string) #t))
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
  (hash-ref %keywords token))

(define (maybe-highlight! token)
  (when (keyword? (map cell-char token))
    (let ((x (cell-x (car token)))
          (y (cell-y (car token))))
      (for-each (lambda (index)
                  (hash-set! %styles (cons (+ x index) y) TB-RED))
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
        (comment? (hash-set! %styles (cons x (+ start-y y)) TB-YELLOW))
        ((eq? char %semicolon)
         (hash-set! %styles (cons x (+ start-y y)) TB-YELLOW)
         (set! comment? #t))
        (else #f)))
     buffer)))
        
(define (highlight!)
  (set! %styles (make-hash-table))
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
    (let ((color (or (hash-ref %styles (cons x y)) TB-WHITE)))
      (cond
       ((eq? char %open)
        (set! color (level->color))
        (set! %level (1+ %level)))
       ((eq? char %close)
        (set! %level (1- %level))
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
                        (number->string (1+ (cursor-line)))
                        "/"
                        (number->string (1+ (cursor-column)))
                        " -")))

;; event handling

(define (quit)
  (tb-shutdown)
  (exit))

(define (save!)
  (call-with-output-file %filename
    (lambda (port)
      (display (buffer->string %buffer) port))))

(define (dispatch)
  (match (tb-poll-event)
    (($ <char> char)
     (buffer-insert! char))
    (($ <key> mode key)
     (dg mode key)
     (cond
      ((eq? key TB-KEY-CTRL-Q) (quit))
      ((eq? key TB-KEY-CTRL-S) (save!))
      ((eq? key TB-KEY-CTRL-E) (cursor-end-of-line!))
      ((eq? key TB-KEY-CTRL-A) (cursor-start-of-line!))
      ((eq? key TB-KEY-CTRL-D) (buffer-delete!))
      ((eq? key TB-KEY-DELETE) (buffer-delete!))
      ((eq? key TB-KEY-ENTER) (buffer-newline!))
      ((eq? key TB-KEY-SPACE) (buffer-insert! (char->integer #\space)))
      ((eq? key TB-KEY-BACKSPACE2) (buffer-backspace!))
      ((eq? key TB-KEY-ARROW-UP) (cursor-up!))
      ((eq? key TB-KEY-ARROW-DOWN) (cursor-down!))
      ((eq? key TB-KEY-ARROW-LEFT) (cursor-left!))
      ((eq? key TB-KEY-ARROW-RIGHT) (cursor-right!))))
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

(call-with-sigint main values)
