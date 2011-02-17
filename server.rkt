; Chat server.
#lang racket

(require racket/date)
(require racket/set)
(require "chat-protocol.rkt")
(require "lib/logging.rkt")

; Constants
(define HOSTNAME #f)    ; bind to all interfaces
(define PORT 8081)      ; bind to this port
(define REUSE-PORT #t)  ; for debugging
(define MAX-CLIENTS 30) ; maximum number of clients waiting

(define (server port)
  (define users (make-hash '())) ; map users to their corresponding sockets

  (define (error-out errcode message out)
    (begin
      (write-header errcode out)
      (write-message message out)
      )
    )

  (define listener
    (begin
      (info-message "Starting up the server...")
      (let ((res (tcp-listen port MAX-CLIENTS REUSE-PORT HOSTNAME)))
        (info-message (string-append "Server awaiting for connections on port " (number->string port) "."))
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
      (display header)
      (if (version-match? header)
        (command (get-command header) in out)
        (error-out ERR_PROTO_VERSION "Invalid protocol version." out)
        )
      )
    )

  (define (find-command cmd)
    (cond ((= cmd CMD_SIGNIN)   signin      )
          ((= cmd CMD_SIGNOUT)  signout     )
          ((= cmd CMD_SEND)     send        )
          (else                 #f          )
          )
    )

  (define (command cmd in out)
    (let ((worker (find-command cmd))
          (msg (read-message in)) ; Don't move this down!
          )
      (if worker
        (let-values ([(resp-hdr resp-msg ) (worker msg in out)])
                    (write-header resp-hdr out)
                    (write-message resp-msg out)
                    (debug-message (string-append "Server response: " resp-msg))
                    )
        (error-out ERR_UNKNOWN_CMD "Invalid command found in request. Ignoring." out)
        )
      )
    )

  (define (signin username in out)
    (info-message (string-append "Trying to sign in user " username "."))
    ; Not thread-safe...
    (if (hash-has-key? users username)
      (values ERR_USER_TAKEN (string-append "Username " username " is already taken."))
      (begin
        (hash-set! users username (cons in out))
        (info-message (string-append "User " username " signed in succesfully."))
        (info-message (string-append "Currently " (number->string (hash-count users)) " users are logged in."))
        (values RET_OK "Signin sucessful.")
        )
      )
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
