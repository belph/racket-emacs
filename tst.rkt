#lang racket
(provide val)
(eprintf "running!\n")
(define val 'foobar)
(system-type 'gc)
(+ 2 2)
