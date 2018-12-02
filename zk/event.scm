(library (zk event)
  (export make-event)
  (import
   (chezscheme)
   (matchable)
   (zk termbox)
   (zk helpers)
   )

  (define (make-event termbox-event)
    (match termbox-event
      (($ <event-char> char) (event-char->event char))
      (($ <event-key> mode key) (event-key->event mode key))

      ;; XXX: in case of resize event the screen is redrawn, not need
      ;; to support it explicitly here
      ;; (($ <event-resize> width height) (event-resize->event width height))

      ;; mouse is not supported
      ;; (($ <event-mouse> mode key x y ) (event-mouse->event mode key x y))
      (_ '())))

  ;; events use consand `cons` in order to make it possible to compare
  ;; with `equal?`
  (define (%make-event ctrl alt key)
    (cons (list ctrl alt) key))

  (define integer->symbol
    (compose string->symbol list->string list))

  (define (event-char->event char)
    (%make-event #f #f (integer->symbol char)))

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
        (TB-KEY-F1 (%make-event #f alt 'F1))
        (TB-KEY-F2 (%make-event #f alt 'F2))
        (TB-KEY-F3 (%make-event #f alt 'F3))
        (TB-KEY-F4 (%make-event #f alt 'F4))
        (TB-KEY-F5 (%make-event #f alt 'F5))
        (TB-KEY-F6 (%make-event #f alt 'F6))
        (TB-KEY-F7 (%make-event #f alt 'F7))
        (TB-KEY-F8 (%make-event #f alt 'F8))
        (TB-KEY-F9 (%make-event #f alt 'F9))
        (TB-KEY-F10 (%make-event #f alt 'F10))
        (TB-KEY-F11 (%make-event #f alt 'F11))
        (TB-KEY-F12 (%make-event #f alt 'F12))
        (TB-KEY-INSERT (%make-event #f alt 'insert))
        (TB-KEY-DELETE (%make-event #f alt 'delete))
        (TB-KEY-HOME (%make-event #f alt 'home))
        (TB-KEY-END (%make-event #f alt 'end))
        (TB-KEY-PGUP (%make-event #f alt 'page-up))
        (TB-KEY-PGDN (%make-event #f alt 'page-down))
        (TB-KEY-ARROW-UP (%make-event #f alt 'arrow-up))
        (TB-KEY-ARROW-DOWN (%make-event #f alt 'arrow-down))
        (TB-KEY-ARROW-LEFT (%make-event #f alt 'arrow-left))
        (TB-KEY-ARROW-RIGHT (%make-event #f alt 'arrow-right))

        ;; Mouse is not supported
        ;; (TB-KEY-MOUSE-LEFT (%make-event #f alt 'xxx))
        ;; (TB-KEY-MOUSE-RIGHT (%make-event #f alt 'xxx))
        ;; (TB-KEY-MOUSE-MIDDLE (%make-event #f alt 'xxx))
        ;; (TB-KEY-MOUSE-RELEASE (%make-event #f alt 'xxx))
        ;; (TB-KEY-MOUSE-WHEEL-UP (%make-event #f alt 'xxx))
        ;; (TB-KEY-MOUSE-WHEEL-DOWN (%make-event #f alt 'xxx))

        (TB-KEY-CTRL-TILDE (%make-event #t alt '~))
        (TB-KEY-CTRL-2 (%make-event #t alt '2))
        (TB-KEY-CTRL-A (%make-event #t alt 'a))
        (TB-KEY-CTRL-B (%make-event #t alt 'b))
        (TB-KEY-CTRL-C (%make-event #t alt 'c))
        (TB-KEY-CTRL-D (%make-event #t alt 'd))
        (TB-KEY-CTRL-E (%make-event #t alt 'e))
        (TB-KEY-CTRL-F (%make-event #t alt 'f))
        (TB-KEY-CTRL-G (%make-event #t alt 'g))
        (TB-KEY-BACKSPACE (%make-event #t alt 'backspace))
        (TB-KEY-CTRL-H (%make-event #t alt 'h))
        (TB-KEY-TAB (%make-event #t alt 'tab))
        (TB-KEY-CTRL-I (%make-event #t alt 'i))
        (TB-KEY-CTRL-J (%make-event #t alt 'j))
        (TB-KEY-CTRL-K (%make-event #t alt 'k))
        (TB-KEY-CTRL-L (%make-event #t alt 'l))
        (TB-KEY-ENTER (%make-event #t alt 'enter))
        (TB-KEY-CTRL-M (%make-event #t alt 'm))
        (TB-KEY-CTRL-N (%make-event #t alt 'n))
        (TB-KEY-CTRL-O (%make-event #t alt 'o))
        (TB-KEY-CTRL-P (%make-event #t alt 'p))
        (TB-KEY-CTRL-Q (%make-event #t alt 'q))
        (TB-KEY-CTRL-R (%make-event #t alt 'r))
        (TB-KEY-CTRL-S (%make-event #t alt 's))
        (TB-KEY-CTRL-T (%make-event #t alt 't))
        (TB-KEY-CTRL-U (%make-event #t alt 'u))
        (TB-KEY-CTRL-V (%make-event #t alt 'v))
        (TB-KEY-CTRL-W (%make-event #t alt 'w))
        (TB-KEY-CTRL-X (%make-event #t alt 'x))
        (TB-KEY-CTRL-Y (%make-event #t alt 'y))
        (TB-KEY-CTRL-Z (%make-event #t alt 'z))
        (TB-KEY-ESC (%make-event #t alt 'escape))
        ;; not sure what it is and clash with TB-KEY-ESC
        ;; (TB-KEY-CTRL-LSQ-BRACKET (%make-event #t alt 'F1))
        ;; clash with TB-KEY-CTRL-ESC
        ;; (TB-KEY-CTRL-3 (%make-event #t alt '3))
        (TB-KEY-CTRL-4 (%make-event #t alt '4))
        ;; clash with TB-KEY-CTRL-BACKSLASH
        ;; (TB-KEY-CTRL-BACKSLASH (%make-event #t alt 'F1))
        (TB-KEY-CTRL-5 (%make-event #t alt '5))
        ;; not sure what it is and clash with TB-KEY-CTRL-5
        ;; (TB-KEY-CTRL-RSQ-BRACKET (%make-event #t alt 'F1))
        (TB-KEY-CTRL-6 (%make-event #t alt '6))
        (TB-KEY-CTRL-7 (%make-event #t alt '7))
        ;; not sure what it is and class with TB-KEY-CTRL-7
        ;; (TB-KEY-CTRL-SLASH (%make-event #t alt 'F1))
        ;; (TB-KEY-CTRL-UNDERSCORE (%make-event #t alt 'F1))
        (TB-KEY-SPACE (%make-event #f alt 'space))
        ;; not sure what it is but defined in termbox
        (TB-KEY-BACKSPACE2 (%make-event #t alt 'backspace2))
        ;; clash with TB-KEY-BACKSPACE2
        ;; (TB-KEY-CTRL-8 (%make-event #t alt '8))
        (else '()))))
    )
