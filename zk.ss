;; stdlib
(import (rnrs))
(import (chezscheme))

;; others
(import (zk termbox))


(tb-init)
(tb-change-cell 1 1 #\a #x01 #x02)
(tb-present)
(sleep (make-time 'time-duration 0 3))
(tb-shutdown)
