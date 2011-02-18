; A module to help validating fields
#lang racket

(provide validation-error
         validation-error?
         raise-validation-error
         make-validation-error
         validate
         )

(require "utils.rkt")

; Custom exception struct.
(define-struct (validation-error exn:fail:user) ())
(define (raise-validation-error msg )
  (raise (make-validation-error msg (current-continuation-marks))))

; Validates the value `val` using the predicate `pred?`
; On success, returns `val`
; On failure, raises a `validation-error` with message `errmsg`.
(define (validate val pred? errmsg)
    (or (and (pred? val) val) (raise-validation-error errmsg))
)

