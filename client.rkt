; Chat client.
#lang racket

(require "chat-protocol.rkt")

(define (client hostname port)

  (define (error-out message out)
    (display "Client error: " out)
    (display message out)
    )

  (define (communicate in out cmd msg)
    (write-bytes (make-header cmd) out)
    (write-bytes (string->bytes/utf-8 msg) out)
    (values (read-bytes HEADER_SIZE in)
            (read-message in))
    )

  (define (signin in out username)
    (let-values ([(header msg) (communicate in out CMD_SIGNIN username)])
                (begin
                  (display "Server response header: ")
                  (display header)
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
    (define-values (in out)
                   (tcp-connect hostname port))
    (values (in out))
    )

  (define-values (in out)
                 (connect))
                 (signin USERNAME)
  )

; These should be configurable
(define HOSTNAME "localhost")
(define PORT 8081)
(define USERNAME "emou")

(client HOSTNAME PORT)
