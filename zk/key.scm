(library (zk key)
  (export make-key)
  (import
   (chezscheme)
   (matchable)
   (zk termbox)
   (zk helpers)
   )

  (define (make-key termbox-event)
    (match termbox-event
      (($ <event-char> char) (event-char->event char))
      (($ <event-key> mode key) (event-key->event mode key))

      ;; XXX: in case of resize event the screen is redrawn, not need
      ;; to support it explicitly here
      ;; (($ <event-resize> width height) (event-resize->event width height))

      ;; mouse is not supported
      ;; (($ <event-mouse> mode key x y ) (event-mouse->event mode key x y))
      (_ '())))

  ;; events use `list` and `cons` in order to make it possible to
  ;; compare with `equal?`
  (define (%make-key ctrl alt key)
    (cons (list ctrl alt) key))

  (define (event-char->event char)
    (%make-key #f #f char))

  (define-syntax switch
    (syntax-rules ()
      [(switch key (value out) ... (else expr))
       (cond
        ((eq? key value) out)
        ...
        (else expr))]))

  (define (event-key->event mode key)
    (let ((alt (eq? mode TB-MOD-ALT)))
      (switch key
        (TB-KEY-F1 (%make-key #f alt 'F1))
        (TB-KEY-F2 (%make-key #f alt 'F2))
        (TB-KEY-F3 (%make-key #f alt 'F3))
        (TB-KEY-F4 (%make-key #f alt 'F4))
        (TB-KEY-F5 (%make-key #f alt 'F5))
        (TB-KEY-F6 (%make-key #f alt 'F6))
        (TB-KEY-F7 (%make-key #f alt 'F7))
        (TB-KEY-F8 (%make-key #f alt 'F8))
        (TB-KEY-F9 (%make-key #f alt 'F9))
        (TB-KEY-F10 (%make-key #f alt 'F10))
        (TB-KEY-F11 (%make-key #f alt 'F11))
        (TB-KEY-F12 (%make-key #f alt 'F12))
        (TB-KEY-INSERT (%make-key #f alt 'insert))
        (TB-KEY-DELETE (%make-key #f alt 'delete))
        (TB-KEY-HOME (%make-key #f alt 'home))
        (TB-KEY-END (%make-key #f alt 'end))
        (TB-KEY-PGUP (%make-key #f alt 'page-up))
        (TB-KEY-PGDN (%make-key #f alt 'page-down))
        (TB-KEY-ARROW-UP (%make-key #f alt 'arrow-up))
        (TB-KEY-ARROW-DOWN (%make-key #f alt 'arrow-down))
        (TB-KEY-ARROW-LEFT (%make-key #f alt 'arrow-left))
        (TB-KEY-ARROW-RIGHT (%make-key #f alt 'arrow-right))

        ;; Mouse is not supported
        ;; (TB-KEY-MOUSE-LEFT (%make-key #f alt 'xxx))
        ;; (TB-KEY-MOUSE-RIGHT (%make-key #f alt 'xxx))
        ;; (TB-KEY-MOUSE-MIDDLE (%make-key #f alt 'xxx))
        ;; (TB-KEY-MOUSE-RELEASE (%make-key #f alt 'xxx))
        ;; (TB-KEY-MOUSE-WHEEL-UP (%make-key #f alt 'xxx))
        ;; (TB-KEY-MOUSE-WHEEL-DOWN (%make-key #f alt 'xxx))

        (TB-KEY-CTRL-TILDE (%make-key #t alt #\~))
        (TB-KEY-CTRL-2 (%make-key #t alt #\2))
        (TB-KEY-CTRL-A (%make-key #t alt #\a))
        (TB-KEY-CTRL-B (%make-key #t alt #\b))
        (TB-KEY-CTRL-C (%make-key #t alt #\c))
        (TB-KEY-CTRL-D (%make-key #t alt #\d))
        (TB-KEY-CTRL-E (%make-key #t alt #\e))
        (TB-KEY-CTRL-F (%make-key #t alt #\f))
        (TB-KEY-CTRL-G (%make-key #t alt #\g))
        (TB-KEY-BACKSPACE (%make-key #t alt 'backspace))
        (TB-KEY-CTRL-H (%make-key #t alt #\h))
        (TB-KEY-TAB (%make-key #t alt 'tab))
        (TB-KEY-CTRL-I (%make-key #t alt #\i))
        (TB-KEY-CTRL-J (%make-key #t alt #\j))
        (TB-KEY-CTRL-K (%make-key #t alt #\k))
        (TB-KEY-CTRL-L (%make-key #t alt #\l))
        (TB-KEY-ENTER (%make-key #f alt 'enter))
        (TB-KEY-CTRL-M (%make-key #t alt #\m))
        (TB-KEY-CTRL-N (%make-key #t alt #\n))
        (TB-KEY-CTRL-O (%make-key #t alt #\o))
        (TB-KEY-CTRL-P (%make-key #t alt #\p))
        (TB-KEY-CTRL-Q (%make-key #t alt #\q))
        (TB-KEY-CTRL-R (%make-key #t alt #\r))
        (TB-KEY-CTRL-S (%make-key #t alt #\s))
        (TB-KEY-CTRL-T (%make-key #t alt #\t))
        (TB-KEY-CTRL-U (%make-key #t alt #\u))
        (TB-KEY-CTRL-V (%make-key #t alt #\v))
        (TB-KEY-CTRL-W (%make-key #t alt #\w))
        (TB-KEY-CTRL-X (%make-key #t alt #\x))
        (TB-KEY-CTRL-Y (%make-key #t alt #\y))
        (TB-KEY-CTRL-Z (%make-key #t alt #\z))
        (TB-KEY-ESC (%make-key #t alt 'escape))
        ;; not sure what it is and clash with TB-KEY-ESC
        ;; (TB-KEY-CTRL-LSQ-BRACKET (%make-key #t alt 'F1))
        ;; clash with TB-KEY-CTRL-ESC
        ;; (TB-KEY-CTRL-3 (%make-key #t alt '3))
        (TB-KEY-CTRL-4 (%make-key #t alt #\4))
        ;; clash with TB-KEY-CTRL-BACKSLASH
        ;; (TB-KEY-CTRL-BACKSLASH (%make-key #t alt 'F1))
        (TB-KEY-CTRL-5 (%make-key #t alt #\5))
        ;; not sure what it is and clash with TB-KEY-CTRL-5
        ;; (TB-KEY-CTRL-RSQ-BRACKET (%make-key #t alt 'F1))
        (TB-KEY-CTRL-6 (%make-key #t alt #\6))
        (TB-KEY-CTRL-7 (%make-key #t alt #\7))
        ;; not sure what it is and class with TB-KEY-CTRL-7
        ;; (TB-KEY-CTRL-SLASH (%make-key #t alt 'F1))
        ;; (TB-KEY-CTRL-UNDERSCORE (%make-key #t alt 'F1))
        (TB-KEY-SPACE (%make-key #f alt #\space))
        ;; not sure what it is but defined in termbox
        (TB-KEY-BACKSPACE2 (%make-key #t alt 'backspace2))
        ;; clash with TB-KEY-BACKSPACE2
        ;; (TB-KEY-CTRL-8 (%make-key #t alt #\8))
        (else '()))))
    )
