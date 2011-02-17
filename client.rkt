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
    (display "Client error: " out)
    (display message out)
    )

  ; Send a request and read the response.
  (define (communicate in out cmd msg)
    (begin
      (write-header cmd out)
      (write-message msg out)
      (values (read-header in)
              (read-message in))
      )
    )

  (define (signin in out username)
    (let-values ([(header msg) (communicate in out CMD_SIGNIN username)])
                (begin
                  (info-message
                    (string-append
                      "Server response header: " header "\n"
                      "Server response message: " msg "\n")
                    )
                  )
                )
    )

  (define (signout in out)
    (display "Signout called")
    )

  (define (send in out)
    (display "Send called")
    )

  (define (connect)
    (let-values ([(in out) (tcp-connect hostname port)])
                ; Turn off port buffering for the output port
                (file-stream-buffer-mode out 'none)
                (values in out)
                )
    )

  (let-values ([(in out) (connect)])
              (signin in out USERNAME)
              )
  )

(client HOSTNAME PORT)
