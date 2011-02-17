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

  (define users (make-hash '()))     ; Map users to sockets: easy check if user is signed in
  (define user-threads (set))        ; A set of user threads so the server can broadcast messages to them
                                     ; (yes, racket has message-passing. VERY COOL.)

  (define (register-user! username in out)
    (hash-set! users username (cons in out))
    )
  (define (unregister-user! username)
    (and (user-exists? username) (hash-remove! users username))
    )
  (define (user-exists? username)
    (hash-has-key? users username) 
  )
  (define (register-thread! t)
    (set-add user-threads t)
  )

  ; Broadcast the message
  (define (add-message! msg)
    (set-for-each user-threads (lambda (t) (thread-send t msg)))
  )

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
              (register-thread! (current-thread))
              (file-stream-buffer-mode out 'none)
              (communicate in out)
              (close-input-port in)
              (close-output-port out)
              )
            )
    )

  (define (communicate in out)
    (define-values (get-user set-user!)
                   (let ((user '())) ; Initially, no user is associated with this connection
                     (define (get) user)
                     (define (set username) (set! user username))
                     (values get set)
                     ))

    ; Dispatch the command and call it based on the header field for command
    (define (command cmd in out)
      (let ((worker (find-command cmd))
            (msg (read-message in)) ; Don't move this down!
            )
        (if worker
          (let-values ([(resp-hdr resp-msg) (worker msg in out)])
                      (debug-message "Writing header...")
                      (write-header resp-hdr out)
                      (debug-message "Header written.")
                      (debug-message "Writing message...")
                      (write-message resp-msg out)
                      (debug-message "Message written.")
                      (debug-message (string-append "Server response: " resp-msg))
                      )
          (error-out ERR_UNKNOWN_CMD "Invalid command found in request. Ignoring." out)
          )
        )
      )

    ; Sign in command
    (define (signin username in out)
      (info-message (string-append "Trying to sign in user " username "."))
      ; Not thread-safe...
      (if (hash-has-key? users username)
        (values ERR_USER_TAKEN (string-append "Username " username " is already taken."))
        (begin
          (register-user! username in out)
          (set-user! username)
          (debug-message (string-append "Setting user to " (get-user) "."))
          (debug-message (string-append "User " username " signed in succesfully."))
          (debug-message (string-append "Currently " (number->string (hash-count users)) " users are logged in."))
          (values RET_OK "Signin successful.")
          )
        )
      )

    ; Sign out command
    (define (signout username in out)
      (info-message (string-append "Trying to sign out user " username "."))
      (if (not (hash-has-key? users username))
        (values ERR_UNKNOWN_USER (string-append username " is not signed in."))
        (begin
          (unregister-user! username)
          (info-message (string-append "User " username " signed out succesfully."))
          (info-message (string-append "Currently " (number->string (hash-count users)) " users are logged in."))
          (values RET_OK "Signout successful.")
          )
        )
      )
    
    ; Command that send a message. This is the most important one.
    (define (send msg in out)
      (add-message! (string-append "User " (get-user) " says: " msg))
      (values RET_OK "Message delivered.")
      )

    ; Command dispatcher
    (define (find-command cmd)
      (cond ((= cmd CMD_SIGNIN)   signin      )
            ((= cmd CMD_SIGNOUT)  signout     )
            ((= cmd CMD_SEND)     send        )
            (else                 #f          )
            )
      )

    ; Loop till connection is closed
    (define (communicate-loop)
      (let ((header (get-header in)))
        (cond
          [(eof-object? header)
           (unregister-user! (get-user))]

          [(not (version-match? header))
           (error-out ERR_PROTO_VERSION "Invalid protocol version." out)]

          [else
            (begin
              (command (get-command header) in out)
              (communicate-loop)
              )])
        )
      )

    ; Go!
    (communicate-loop)

    )

  (define (mainloop)
    (handle-request listener)
    (mainloop)
    )

  (mainloop)

  )

(server PORT)
