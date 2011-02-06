; Chat server.
#lang racket

(require racket/date)
(require "chat-protocol.rkt")

; Constants
(define HOSTNAME #f)    ; bind to all interfaces
(define PORT 8081)      ; bind to this port
(define REUSE-PORT #t)  ; for debugging
(define MAX-CLIENTS 30) ; maximum number of clients waiting

(define (server port)

  (define (error-out message out)
    (display "Server error: " out)
    (display message out)
    )

  (define listener
    (begin
      (display "Starting up the server...\n")
      (let ((res (tcp-listen port MAX-CLIENTS REUSE-PORT HOSTNAME)))
        (display "Server awaiting for connections on port ")
        (display port)
        (display ".\n")
        res
        )
      )
    )

  (define (handle-request listener)
    (define-values (in out)
                   (tcp-accept listener))
    (thread (lambda ()
              (communicate in out)
              (close-input-port in)
              (close-output-port out)
              )
            )
    )

  (define (communicate in out)
    (let ((header (get-header in)))
      (if (version-match? header)
        (command (get-command header) in out)
        (error-out "Invalid protocol version." out)
        )
      )
    )

  (define (command cmd in out)
    (cond ((= cmd CMD_SIGNIN) (signin in out))
          ((= cmd CMD_SIGNOUT) (signout in out))
          ((= cmd CMD_SEND) (send in out))
          (else (error-out "Invalid command found in request." out))
          )
    )

  (define (signin in out username)
    (display "Signin called")
    )

  (define (signout in out)
    (display "Signout called")
    )

  (define (send in out)
    (display "Send called")
    )

  (define (mainloop)
    (handle-request listener)
    (mainloop)
    )

  (mainloop)

  )

(server PORT)
