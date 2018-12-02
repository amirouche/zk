(import (srfi s9 records))
(import (matchable))

(import (zk buffer))
(import (zk key))
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
  (make-model root frame-buffers mini-buffer focus)
  model?
  (root model-root model-root!)
  (frame-buffers model-frame-buffers model-frame-buffers!)
  (mini-buffer model-mini-buffer model-mini-buffer!)
  (focus model-focus model-focus!))

(define-record-type <frame>
  (make-frame frame-buffer parent cursor-position top-left bottom-right)
  frame?
  (frame-buffer frame-buffer frame-buffer!)
  (parent frame-parent frame-parent!)
  (cursor-position frame-cursor-position frame-cursor-position!)
  (top-left frame-top-left frame-top-left!)
  (bottom-right frame-bottom-right frame-bottom-right!))

(define (frame-layout! frame top-left bottom-right)
  (frame-top-left! frame top-left)
  (frame-bottom-right! frame bottom-right))

(define-record-type <frame-vertical>
  (make-frame-vertical parent up down)
  frame-vertical?
  (parent frame-vertical-parent frame-vertical-parent!)
  (up frame-vertical-up frame-vertical-up!)
  (down frame-vertical-down frame-vertical-down!))

(define-record-type <frame-horizontal>
  (make-frame-horizontal parent left right)
  frame-horizontal?
  (parent frame-horizontal-parent frame-horizontal-parent!)
  (left frame-horizontal-left frame-horizontal-left!)
  (right frame-horizontal-right frame-horizontal-right!))

(define-record-type <mode>
  (make-mode name bindings)
  mode?
  (name mode-name)
  (bindings mode-bindings))

(define-record-type <frame-buffer>
  (make-frame-buffer path data view mode insert)
  frame-buffer?
  (path frame-buffer-path frame-buffer-path!)
  (data frame-buffer-data frame-buffer-data!)
  (view frame-buffer-view frame-buffer-view!)
  (mode frame-buffer-mode frame-buffer-mode!)
  (insert frame-buffer-insert frame-buffer-insert!))

;;

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


(define frame-view (compose frame-buffer-view frame-buffer))

(define (%frame-render model frame)
  (let ((frame-buffer (frame-buffer frame)))
    ((frame-buffer-view frame-buffer) model frame)))

(define (frame-render model frame)
  (match frame
    ((? frame? frame) (%frame-render model frame))
    ((? frame-vertical? frame)
     (frame-render model (frame-vertical-up frame))
     (frame-render model (frame-vertical-down frame)))
    ((? frame-horizontal? frame)
     (frame-render model (frame-horizontal-left frame))
     (frame-render model (frame-horizontal-right frame)))))

(define (render model)
  (if (model-mini-buffer model)
      (let ((view (frame-view (model-mini-buffer model))))
        (view model (model-mini-buffer model)))
      (let ((root (model-root model)))
        (frame-render model root))))

(define (model-current-focus model)
  (if (model-mini-buffer model)
      (model-mini-buffer model)
      (model-focus model)))

(define model-current-bindings
  (compose mode-bindings frame-buffer-mode frame-buffer model-current-focus))

(define model-current-insert
  (compose frame-buffer-insert frame-buffer model-current-focus))

(define (dispatch model)
  (let ((bindings (model-current-bindings model))
        (insert (model-current-insert model)))
    (let ((key (dg (make-key (tb-poll-event)))))
      (let ((callback (hashtable-ref bindings key insert)))
        (callback model key)))))

;; layout

(define (%layout! frame top-left bottom-right)
  ;; (pk frame top-left bottom-right)
  (match frame
    ((? frame? frame)
     (frame-layout! frame top-left bottom-right))
    ((? frame-vertical? frame)
     (let* ((height (- (position-y bottom-right) (position-y top-left)))
            (half (floor (/ height 2))))
       (%layout! (frame-vertical-up frame)
                 top-left
                 (make-position (position-x bottom-right)
                                (+ half (position-y top-left) 1)))
       (%layout! (frame-vertical-down frame)
                 (make-position (position-x top-left)
                                (- (position-y bottom-right) half))
                 bottom-right)))
    ((? frame-horizontal? frame)
     (let* ((width (- (position-x bottom-right) (position-x top-left)))
            (half (floor (/ width 2))))
       (%layout! (frame-horizontal-left frame)
                 top-left
                 (make-position (+ (position-x top-left) half 1)
                                (position-y bottom-right)))
       (%layout! (frame-horizontal-right frame)
                 (make-position (- (position-x bottom-right) half)
                                (position-y top-left))
                 bottom-right)))))

(define (layout! model)
  (%layout! (model-root model)
            (make-position 0 0)
            ;; height minus the mini-buffer size
            (make-position (tb-width) (- (tb-height) 1))))

(define (frame-mock name)
  (let ((frame-buffer (make-frame-buffer name
                                         #f
                                         (lambda (model frame)
                                           (let ((position (frame-top-left frame)))
                                             (tb-change-cell (position-x position)
                                                             (position-y position)
                                                             120
                                                             TB-WHITE
                                                             TB-DEFAULT))
                                           (let ((position (frame-bottom-right frame)))
                                             (tb-change-cell (- (position-x position) 1)
                                                             (- (position-y position) 1)
                                                             121
                                                             TB-WHITE
                                                             TB-DEFAULT)))
                                         #f
                                         #f)))
    (make-frame frame-buffer
                #f
                (make-position 0 0)
                #f
                #f)))


;; main loop

(define (make-position x y)
  (cons x y))

(define position-x car)
(define position-y cdr)

(define (make-bindings)
  (make-hashtable equal-hash equal?))

(define scheme-bindings (make-bindings))

(define (zk-exit model key)
  (tb-shutdown)
  (exit))

(hashtable-set! scheme-bindings '((#t #f) . q) zk-exit)

;; meta command

;; TODO: replace with combinators
(define (make-binding ctrl alt symbol)
  (cons (list ctrl alt) symbol))

(define key-char (compose char->integer cdr))

(define (meta-command-view model frame)
  (print 0 "meta-command-view")
  (let ((mini-buffer (model-mini-buffer model)))
    (let ((frame-buffer (frame-buffer mini-buffer)))
      (let ((buffer (frame-buffer-data frame-buffer)))
        (print 1 (buffer->string buffer)))
      (tb-set-cursor (position-x (frame-cursor-position mini-buffer))
                     (position-y (frame-cursor-position mini-buffer))))))

(define (meta-command-on-enter model key)
  (dg 'mini-buffer-input (buffer->string
                          (frame-buffer-data
                           (frame-buffer
                            (model-mini-buffer model)))))
  (zk-exit model #f))

(define (meta-command-mode)
  (let ((bindings (make-bindings)))
    (hashtable-set! bindings (make-binding #f #f 'enter) meta-command-on-enter)
    (hashtable-set! bindings (make-binding #t #f #\q) zk-exit)
    (make-mode "meta-command" bindings)))

(define (meta-command-insert model key)
  ;; meta-command always works with mini-buffer
  (let* ((frame (model-mini-buffer model))
         (frame-buffer* (frame-buffer frame))
         (buffer (frame-buffer-data frame-buffer*)))
    (let ((new (buffer-char-insert buffer
                                   0
                                   (position-y (frame-cursor-position frame))
                                   (key-char key))))
      (frame-buffer-data! frame-buffer* new)
      (frame-cursor-position! frame
                              (make-position (position-x (frame-cursor-position frame))
                                             (+ 1 (position-y (frame-cursor-position frame))))))))

(define (meta-command key)
  (let ((frame-buffer (make-frame-buffer #f
                                         (string->buffer "")
                                         meta-command-view
                                         (meta-command-mode)
                                         meta-command-insert)))
    (make-frame frame-buffer
                #f
                (make-position 1 0)
                (make-position 1 0)
                (make-position 1 0))))

(define (zk-meta-command model key)
  (model-mini-buffer! model (meta-command key)))

(hashtable-set! scheme-bindings (make-binding #t #f #\x) zk-meta-command)

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

;; (lambda (model top-left bottom-right) (dg 'view))
;; (define (file-buffer-view filename

(define %frame-buffer (make-frame-buffer filename
                                         (filename->buffer %filename)
                                         void
                                         scheme-mode
                                         (lambda (model key) (dg 'insert))))

(define root (make-frame %frame-buffer
                         #f
                         (make-position 0 0)
                         (make-position 0 0)
                         (make-position 0 0)))

(define frame-buffers (make-hashtable string-hash string=?))
(hashtable-set! frame-buffers filename %frame-buffer)

(define model (make-model (make-frame-horizontal
                           #f
                           (make-frame-vertical #f (frame-mock "top") (frame-mock "bottom"))
                           (make-frame-vertical #f (frame-mock "top") (frame-mock "bottom")))
                          frame-buffers
                          #f
                          root
                          ))

(define (main)
  (tb-init)
  (let loop ()
    (tb-clear)
    (layout! model)
    (dg (model-root model))
    (render model)
    (tb-present)
    (dispatch model)
    (loop)))

(main)
