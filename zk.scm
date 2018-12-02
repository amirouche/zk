(import (srfi s1 lists))
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

(define (make-position x y)
  (cons x y))

(define position-x car)
(define position-y cdr)

;; model

(define-record-type <model>
  (make-model root frame-buffers mini-buffer focus)
  model?
  (root model-root model-root!)
  (frame-buffers model-frame-buffers model-frame-buffers!)
  (mini-buffer model-mini-buffer model-mini-buffer!)
  (focus model-focus model-focus!))

(define-record-type <frame>
  (%make-frame frame-buffer parent cursor-position view-position top-left bottom-right)
  frame?
  (frame-buffer frame-buffer frame-buffer!)
  (parent frame-parent frame-parent!)
  (cursor-position frame-cursor-position frame-cursor-position!)
  (view-position frame-view-position frame-view-position!)
  (top-left frame-top-left frame-top-left!)
  (bottom-right frame-bottom-right frame-bottom-right!))

(define %position-init (make-position 0 0))

(define (make-frame frame-buffer)
  (%make-frame frame-buffer #f %position-init %position-init #f #f))

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
  (view frame-buffer-render frame-buffer-render!)
  (mode frame-buffer-mode frame-buffer-mode!)
  (insert frame-buffer-insert frame-buffer-insert!))

;;

(define (read-string port)
  (let loop ((out '()))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (list->string (reverse out))
          (loop (cons char out))))))

(define (filename->buffer filename)
  (string->buffer (call-with-input-file filename read-string)))

(define (print column line string)
  (let ((chars (map char->integer (string->list string)))
        (max (tb-width)))
    (let loop ((index column)
               (chars chars))
      (unless (or (null? chars) (eq? index max))
        (tb-change-cell index line (car chars) TB-WHITE TB-DEFAULT)
        (loop (+ 1 index) (cdr chars))))))

(define (%frame-render model frame)
  (let ((frame-buffer (frame-buffer frame)))
    ((frame-buffer-render frame-buffer) model frame)))

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
      (let ((view (frame-render (model-mini-buffer model))))
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

;; main loop

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

(define (meta-command-render model frame)
  (print 0 0 "meta-command-render")
  (let ((mini-buffer (model-mini-buffer model)))
    (let ((frame-buffer (frame-buffer mini-buffer)))
      (let ((buffer (frame-buffer-data frame-buffer)))
        (print 0 1 (buffer->string buffer)))
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
                                         meta-command-render
                                         (meta-command-mode)
                                         meta-command-insert)))
    (make-frame frame-buffer)))

(define (zk-meta-command model key)
  (model-mini-buffer! model (meta-command key)))

(hashtable-set! scheme-bindings (make-binding #t #f #\q) zk-exit)

(define scheme-mode (make-mode "scheme" scheme-bindings))

(define %filename "zk.scm")

(define (read-string port)
  (let loop ((out '()))
    (let ((char (read-char port)))
      (if (eof-object? char)
          (list->string (reverse out))
          (loop (cons char out))))))

(define (filename->buffer filename)
  (string->buffer (call-with-input-file filename read-string)))

(define (%%frame-buffer-render-char x0 y0 view-x max-x)
  (lambda (x y char)
   (tb-change-cell (+ x0 x) (+ y0 y) char TB-WHITE TB-DEFAULT)))

(define (%%frame-buffer-render buffer x0 y0 view-x view-y width height)
  (let* ((head (buffer-take-lines buffer (+ view-y height)))
         (view (buffer-drop-lines head view-y)))
    (buffer-char-for-each (%%frame-buffer-render-char x0 y0 view-x width) view)))

(define (%truncate string width)
  (if (> (string-length string) width)
      (list->string (take (string->list string) width))
      string))

(define (%frame-buffer-render-mode-line model frame width x0 y)
  (let ((path (frame-buffer-path (frame-buffer frame)))
        (mode (mode-name (frame-buffer-mode (frame-buffer frame)))))
    (print x0 y (%truncate (string-append " - " path " - " mode) width))))

(define (%frame-buffer-render model frame)
  (let* ((view-position (frame-view-position frame))
         (top-left (frame-top-left frame))
         (bottom-right (frame-bottom-right frame))
         (view-x (position-x view-position))
         (view-y (position-y view-position))
         (width (- (position-x bottom-right)
                   (position-x top-left)))
         (height (- (position-y bottom-right)
                    (position-y top-left))))
    (%%frame-buffer-render (frame-buffer-data (frame-buffer frame))
                          (position-x top-left)
                          (position-y top-left)
                          view-x
                          view-y
                          width
                           ;; minus mode line
                          (- height 1))
    (%frame-buffer-render-mode-line model
                                    frame
                                    width
                                    (position-x top-left)
                                    (position-y bottom-right))))

(define (%frame-buffer-insert model key)
  42)

(define %frame-buffer
  (make-frame-buffer %filename
                     (filename->buffer %filename)
                     %frame-buffer-render
                     scheme-mode
                     %frame-buffer-insert))

(define frame-buffers (make-hashtable string-hash string=?))
(hashtable-set! frame-buffers %filename %frame-buffer)

(define focus (make-frame %frame-buffer))
(define other (make-frame %frame-buffer))

(define model (make-model
               (make-frame-horizontal
                #f
                focus
                other)
               frame-buffers
               #f
               focus
               ))

(define (main)
  (tb-init)
  (let loop ()
    (tb-clear)
    (layout! model)
    (render model)
    (tb-present)
    (dispatch model)
    (loop)))


(main)
