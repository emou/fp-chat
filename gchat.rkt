; A GUI for the chat client
; Run with gracket!
#lang racket/gui
(require "lib/client.rkt")
(require "lib/logging.rkt")
(require "lib/validation.rkt")
(require "lib/utils.rkt")
(require "lib/chat-protocol.rkt")

(define APP_NAME "GChat")
(define DEFAULT_HOST "localhost")

; =======================================
; Global state. Yeah, ugly.
; =======================================
(define client null)
(define users '())
(define listening-thread null)

; Sets and get the list of users
(define (set-users! lst)
  (debug-message (string-append "Refreshing list of users to "
                                "(" (string-join lst ", ") ")..."))
  (set! users lst)
  (send users-list-box set users)
  )

(define (get-users) users)

; The body of the worker thread.
; XXX: This is similar as on the server side. Extract it somewhere!?
(define (listen-loop)
  (let (; the sunshine
        [in (send client get-input-port)])
    (define (loop)
      ; Wait for messages or for client input
      (let* ([msg-evt (thread-receive-evt)]
             [ready (sync msg-evt in)])
        (if (eq? ready msg-evt)
          (handle-messages)
          (handle-input ready)
          )
        )
      )
    (loop)
    )
  )

; Send messages to other users
(define (handle-messages)
  (debug-message "Trying to send a message for broadcast...")
  (let ([instruction (thread-try-receive)])
    (and instruction (send client command (car instruction) (cdr instruction)))
    (debug-message (string-append "Sent a broadcast message " (cdr instruction)))
    (listen-loop)
    )
  )

; Handle server messages
(define (handle-input in)
  (debug-message "Hm, server wants to tell us something...")
  (let ([header (get-header in)])
    (cond
      [(eof-object? header)
       (connection-closed)]

      [else
        (begin
          (handle-push (get-command header) (read-message in))
          (listen-loop)
          )])
    )
  )

(define (handle-push cmd msg)
  (let ([handler (find-push-handler cmd)])
    (if handler
      (handler cmd msg)
      (debug-message (string-append "Unknown push command "
                                    (number->string cmd) " with message "
                                    msg " recieved.")))
    )
  )

(define (find-push-handler cmd)
  (cond ((= cmd PUSH_MSG)     message-recieved )
        ((= cmd PUSH_JOINED)  user-joined      )
        (else                 #f               )
        )
  )

(define (message-recieved cmd msg)
    (debug-message (string-append "Recieved push message '" msg "'!"))
)

(define (user-joined cmd username)
    (debug-message (string-append "User " username " joined!"))
)

; Establishes a new connection to the server and signs in the given user
(define (new-connection host port username)
  (let ([new-client (new client%
                     [host host]
                     [port port]
                     [push-handler handle-push]
                     )])
    (set! client new-client)
    (send client connect)
    (send client signin username)
    (let ([new-users (send client get-users-list)])
      (set-users! new-users))
    (send connect-dialog show #f)
    ; Start the listening thread
    (set! listening-thread (thread (lambda ()
                                     (listen-loop)    
                                     )))
    (send main-win show #t)
    (send main-win set-label
          (string-append "Connected to "
                         host ":" (number->string port) " as " username))))

(define (textbox-changed t e)
  (and (eq? (send e get-event-type) 'text-field-enter)
       (let ([msg (send t get-value)])
         ; Queue the message for sending
         (thread-send listening-thread (cons CMD_SEND msg))
         ; Clear the textbox
         (send t set-value ""))))

(define (connection-closed)
    (define msg ("The server has closed the connection."))
    (message-box (t msg))
    (debug-message msg) 
    (send main-win show #f)
    (send connect-dialog show #t)
)

; =======================================
; Field validations.
; =======================================
(define (clean-port port-value)
  (validate (string->number port-value) number? "Port must be a number."))

(define (clean-host host-value)
  (validate host-value string-nonempty? "Host cannot be blank."))

(define (clean-username username-value)
    (validate username-value string-nonempty? "Username cannot be blank."))

(define (clean-connection-parameters host-value port-value username-value)
  (values (clean-host host-value)
          (clean-port port-value)
          (clean-username username-value))
  )

; =======================================
; Shorthand for defining window titles
; =======================================
(define (t label)
  (string-append APP_NAME " - " label)
  )

; =======================================
; Error handling. Should pop up a dialog
; in the future?
; =======================================
(define (handle-error err)
  (message-box (t "Error") (exn-message err))
  (debug-message err) 
  )

; =======================================
; Main Window.
; =======================================
(define main-win  (new frame%
                        [label APP_NAME]
                        [width 600]
                        [height 400]
                        [min-width 200]
                        [min-height 200]
                        ))

; The box that holds the logged in users
(define users-list-box (new list-box%
                            [parent main-win]
                            [choices (get-users)]
                            [label "List of users"]
                            ))

(define textbox (new text-field%
                     [parent main-win]
                     [label "Send a message:"]
                     [callback textbox-changed]
                            ))

; =======================================
; Connection dialog.
; =======================================
(define connect-dialog (new dialog%
                            [parent main-win]
                            [label (t "Connect to server")]))

; Alignment
(define panel (new horizontal-panel%
                   [parent connect-dialog]
                   [alignment '(center center)]))

; =======================================
; Fields in the connection dialog.
; =======================================
(define host-field (new text-field%
                        [parent connect-dialog]
                        [label "Host"]))
(send host-field set-value DEFAULT_HOST)

(define port-field (new text-field%
                        [parent connect-dialog]
                        [label "Port"]))
(send port-field set-value (number->string DEFAULT_PORT))

(define username-field (new text-field%
                            [parent connect-dialog]
                            [label "Username"]))

; =======================================
; The "Connect" button.
; =======================================
(new button% [parent connect-dialog]
             [label "Connect"]
     (callback (lambda (button event)
                 ; XXX: handle errors better?
                 (with-handlers ([validation-error? handle-error] [(lambda (e) #t) handle-error])
                                (let-values ([(host port username) (clean-connection-parameters
                                                               (send host-field get-value)
                                                               (send port-field get-value)
                                                               (send username-field get-value))])
                                            (new-connection host port username))))))

; Show the connect dialog at the beginning
(send connect-dialog show #t)
