(import (srfi s9 records))
(import (matchable))

(import (zk buffer))
(import (zk event))
(import (zk helpers))
(import (zk termbox))


;; debug log

(define %error (open-file-output-port "out.log"
                                      (file-options no-fail)
                                      (buffer-mode line)
                                      (native-transcoder)))

(define (dg . rest)
  (write rest %error) (newline %error)
  (car (reverse rest)))

;; model

(define-record-type <model>
  (make-model root buffers mini-buffer focus)
  model?
  (root model-root model-root!)
  (buffers model-buffers model-buffers!)
  (mini-buffer model-mini-buffer model-mini-buffer!)
  (focus model-focus model-focus!))

(define-record-type <frame>
  (make-frame buffer parent position topleft)
  frame?
  (buffer frame-buffer frame-buffer!)
  (parent frame-parent frame-parent!)
  (position frame-position frame-position!)
  (topleft frame-topleft frame-topleft!))

(define-record-type <vertical>
  (make-verical parent up down)
  vertical?
  (parent vertical-parent vertical-parent!)
  (up vertical-up vertical-up!)
  (down vertical-down vertical-down!))

(define-record-type <horizontal>
  (make-horizontal parent left right)
  horizontal?
  (parent horizontal-parent horizontal-parent!)
  (left horizontal-left horizontal-left!)
  (right horizontal-right horizontal-right!))

(define-record-type <mode>
  (make-mode name bindings)
  mode?
  (name mode-name)
  (bindings mode-bindings))

(define-record-type <buffer>
  (make-buffer path data view mode insert)
  buffer?
  (path buffer-path buffer-path!)
  (data buffer-data buffer-data!)
  (view buffer-view buffer-view!)
  (mode buffer-mode buffer-mode!)
  (insert buffer-insert buffer-insert!))

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

(define (render-char x y char)
  (when (and (>= x (view-x))
             (< x (+ (view-x) (tb-width)))
             (>= y (view-y))
             (< y (+ (view-y) (- (tb-height) 1))))
        (tb-change-cell (- x (view-x)) (- y (view-y)) char TB-WHITE TB-DEFAULT)))

(define (print line string)
  (let ((chars (map char->integer (string->list string)))
        (max (tb-width)))
    (let loop ((index 0)
               (chars chars))
      (unless (or (null? chars) (eq? index max))
        (tb-change-cell index line (car chars) TB-WHITE TB-DEFAULT)
        (loop (+ 1 index) (cdr chars))))))


(define (render model)
  (print 0 "echo"))

(define model-current-bindings
  (compose mode-bindings buffer-mode frame-buffer model-focus))

(define model-current-insert
  (compose buffer-insert frame-buffer model-focus))

(define (dispatch model)
  (let ((bindings (model-current-bindings model))
        (insert (model-current-insert model)))
    (let ((key (dg (make-event (tb-poll-event)))))
      (let ((callback (hashtable-ref bindings key insert)))
        (callback model key)))))

;; main loop

(define (exit* model key)
  (tb-shutdown)
  (exit))

(define (frame-split model key)
  (let ((clone (frame-clone frame)))
    #f))

(define scheme-bindings (make-hashtable equal-hash equal?))
(hashtable-set! scheme-bindings '((#t #f) . q) exit*)
(hashtable-set! scheme-bindings TB-KEY-CTRL-S frame-split)

(define scheme-mode (make-mode "scheme" scheme-bindings))

(define filename "zk.scm")

(define (read-string port)
  (let loop ((out '()))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (list->string (reverse out))
          (loop (cons char out))))))

(define (filename->buffer filename)
  (string->buffer (call-with-input-file filename read-string)))

(define buffer (make-buffer filename
                            (filename->buffer %filename)
                            (lambda (model) (dg 'view))
                            scheme-mode
                            (lambda (model key) (dg 'insert))))

(define root (make-frame buffer '() '(0 . 0) '(0 . 0)))

(define buffers (make-hashtable string-hash string=?))
(hashtable-set! buffers filename buffer)

(define model (make-model root buffers #f root))

(define (main)
  (tb-init)
  (let loop ()
    (tb-clear)
    (render model)
    (tb-present)
    (dispatch model)
    (loop)))

(main)
