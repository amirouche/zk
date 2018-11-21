;; stdlib
(import (rnrs))
(import (chezscheme))

;; others
(import (zk termbox))


(tb-init)
(tb-change-cell 1 1 #\a #x00 #xff)
(tb-present)
(sleep (make-time 'time-duration 0 3))
(tb-shutdown)
