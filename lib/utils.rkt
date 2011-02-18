#lang racket
; Miscallenous utility functions.
(provide
  string-nonempty?
  )

(define (string-nonempty? str)
  (> (string-length str) 0)
  )
