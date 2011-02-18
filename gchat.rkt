; A GUI chat client
; Run with gracket!
#lang racket/gui
(require "lib/client.rkt")
(require "lib/logging.rkt")
(require "lib/validation.rkt")
(require "lib/utils.rkt")

(define APP_NAME "GChat")

; =======================================
; Global state. Yeah, ugly.
; =======================================
(define mainwindow (new frame% [label APP_NAME]))
(define client null)

; Establishes a new connection to the server
(define (new-connection host port username)
  (let ([client (new client% [host host] [port port])])
    (send client connect)
    (send client signin username)
    (send mainwindow show #t)
    (send mainwindow set-label
          (string-append "Connected to "
                         host ":" (number->string port) " as " username))))

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
(define frame (new frame% [label (t "Main Window")]))

; =======================================
; Connection dialog.
; =======================================
(define connect-dialog (new dialog%
                            [parent frame]
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

(define port-field (new text-field%
                        [parent connect-dialog]
                        [label "Port"]))

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
(send connect-dialog show #t)
