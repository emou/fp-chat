#lang racket
(require racket/date)

(provide debug-message
         info-message
         )

(define (get-date)
  (date->string (current-date))
  )

(define (debug-message msg)
  (_show-message msg "DEBUG")
  )

(define (info-message msg)
  (_show-message msg "INFO")
  )

(define (_show-message msg level)
      (display (string-append "[" (get-date) "][" level "] "))
      (display msg)
      (display "\n")
  )
