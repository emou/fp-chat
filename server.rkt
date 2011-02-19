; Chat server.
; XXX: Server should be a class!?
#lang racket

(require racket/date)
(require racket/set)
(require "lib/chat-protocol.rkt")
(require "lib/logging.rkt")

; Constants
; XXX: Make these configurable on the command line.
(define HOSTNAME #f)            ; bind to all interfaces
(define PORT DEFAULT_PORT)      ; bind to this port
(define REUSE-PORT #t)          ; for debugging
(define MAX-CLIENTS 30)         ; maximum number of clients waiting

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
    (set! user-threads (set-add user-threads t))
    )
  (define (unregister-thread! t)
    (set-remove user-threads t)
    )

  ; Queue a broadcast command
  (define (add-message! cmd msg)
    (set-for-each user-threads (lambda (t) (thread-send t (cons cmd msg)))))

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
              (debug-message "Connection ended. Removing thread...")
              (unregister-thread! (current-thread))
              (close-input-port in)
              (close-output-port out)
              )
            )
    )

  (define (communicate in out)
    (define-values (get-user set-user!)
                   (let ((user null)) ; Initially, no user is associated with this connection
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
          (let-values ([(resp-hdr resp-msg post-action) (worker msg in out)])
                      (debug-message "Writing header...")
                      (write-header resp-hdr out)
                      (debug-message "Header written. Writing message...")
                      (write-message resp-msg out)
                      (debug-message "Message written.")
                      (debug-message (string-append "Server response: " resp-msg))
                      (and post-action (post-action))
                      )
          (error-out ERR_UNKNOWN_CMD "Invalid command found in request. Ignoring." out)
          )
        )
      )

    ; Sign in command
    (define (signin username in out)
      (define (post-action)
        (add-message! PUSH_JOINED username)
        )
      (info-message (string-append "Trying to sign in user " username "."))
      ; Not thread-safe...
      (if (hash-has-key? users username)
        (values ERR_USER_TAKEN (string-append "Username " username " is already taken.") #f)
        (begin
          (register-user! username in out)
          (set-user! username)
          (debug-message (string-append "Setting user to " (get-user) "."))
          (debug-message (string-append "User " username " signed in succesfully."))
          (debug-message (string-append "Currently " (number->string (hash-count users)) " users are logged in."))
          (values RET_OK "Signin successful." post-action)
          )
        )
      )

    ; Sign out command
    (define (signout username in out)
      (info-message (string-append "Trying to sign out user " username "."))
      (if (not (hash-has-key? users username))
        (values ERR_UNKNOWN_USER (string-append username " is not signed in.") #f)
        (begin
          (unregister-user! username)
          (info-message (string-append "User " username " signed out succesfully."))
          (info-message (string-append "Currently " (number->string (hash-count users)) " users are logged in."))
          (values RET_OK "Signout successful." #f)
          )
        )
      )
    
    ; Command that sends the list of currently logged in users, separated by newlines, to the client
    (define (userlist msg in out)
      (values RET_OK (string-join (hash-map users (lambda (u _) u)) (string SEP)) #f)
      )

    ; Command that queues a message for sending. This is the most important one.
    (define (send msg in out)
      (define (post-action)
        (add-message! PUSH_MSG (string-append "User " (get-user) " says: " msg))
        )
      (values RET_OK "Message queued for dispatching." post-action)
      )

    ; Broadcasting
    (define (push-message msg in out)
        (values PUSH_MSG msg #f)
      )

    (define (push-joined msg in out)
        (values PUSH_JOINED msg #f)
    )

    ; Command dispatcher
    (define (find-command cmd)
      (cond ((= cmd CMD_SIGNIN)            signin      )
            ((= cmd CMD_SIGNOUT)           signout     )
            ((= cmd CMD_SEND)              send        )
            ((= cmd CMD_GET_USER_LIST)     userlist    )
            ((= cmd PUSH_MSG)              push-message)
            ((= cmd PUSH_JOINED)           push-joined )
            (else                          #f          )
            )
      )

    ; Loop till connection is closed
    (define (communicate-loop)
      (debug-message "Looping in communicate-loop")
      ; Wait for messages or for client input
      (let* ([msg-evt (thread-receive-evt)]
             [ready (sync msg-evt in)])
        (if (eq? ready msg-evt)
          (handle-push)
          (handle-input ready)
          )
        )
      )

    ; Send messages from other users to the client
    (define (handle-push)
      (debug-message (string-append "Pushing..."))
      (let ([instruction (thread-try-receive)])
        ; Send push only if user is logged in
        (and (get-user) instruction (begin
                           (let-values ([(h m post-action) ((find-command (car instruction)) (cdr instruction) in out)])
                                       (write-header h out)
                                       (write-message m out)
                                       (and post-action (post-action))
                                       )
                           )
             )
        (communicate-loop)
        )
      )

    ; Handle requests by the client
    (define (handle-input in)
      (debug-message "Handling input...")
      (let ([header (get-header in)])
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
