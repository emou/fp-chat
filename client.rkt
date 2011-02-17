; Chat client.
#lang racket

(require "chat-protocol.rkt")
(require "lib/logging.rkt")

; These should be configurable
(define HOSTNAME "localhost")
(define PORT 8081)
(define USERNAME "emou")

(define client-logger (make-logger 'client))

(define (client hostname port)

  (define (error-out message out)
    (error-message "Client error: " out)
    (error-message message out)
    )

  ; Send a request and read the response.
  (define (communicate in out cmd msg)
    (begin
      (write-header cmd out)
      (write-message msg out)
      (debug-message "Request sent. Reading response...")
      (values (read-header in)
              (read-message in))
      )
    )

  (define (signin in out username)
    (communicate in out CMD_SIGNIN username)
    )

  (define (signout in out username)
    (communicate in out CMD_SIGNOUT username)
    )

  (define (send in out)
    (communicate in out CMD_SEND "A test message!")
    )

  (define (connect)
    (let-values ([(in out) (tcp-connect hostname port)])
                ; Turn off port buffering
                (file-stream-buffer-mode out 'none)
                (values in out)
                )
    )

  (let-values ([(in out) (connect)])
              (begin
                (signout in out USERNAME)
                (signout in out USERNAME)
                (signin in out USERNAME)
                (signout in out USERNAME)
                )
              )
  )

(client HOSTNAME PORT)
