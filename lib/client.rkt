#lang racket
; Chat client library.
(provide 
  client%
  )

(require "chat-protocol.rkt")
(require "logging.rkt")

; Custom exception struct.
(define-struct (server-error exn:fail) (retcode))
(define (raise-server-error retcode msg)
  (raise (make-server-error msg (current-continuation-marks) retcode)))

(define client%
  (class object%
         (init host port)
         (define (get-host) host)
         (define (get-port) port)
         (define in null)
         (define out null)

         (super-new)

         (define/public (connect)
                        (let-values ([(i o) (tcp-connect (get-host) (get-port))])
                                    ; Turn off port buffering
                                    (file-stream-buffer-mode o 'none)
                                    (set! in i)
                                    (set! out o)
                                    )
                        )

         (define/public (error-out message)
                        (error-message "Client error: ")
                        (error-message message)
                        )

         (define (command cmd msg)
           (begin
             (write-header cmd out)
             (write-message msg out)
             (debug-message "Request sent. Reading response...")
             (let ([h (read-header in)]
                   [m  (read-message in)])
               (and (check-retcode h m) (values h m))
               )
             )
           )

         (define (check-retcode header msg)
           (or (= (get-retcode header) RET_OK)
               (raise-server-error (get-retcode header) msg)))

         (define/public (get-users-list)
                        (let-values ([(h m) (command CMD_GET_USER_LIST "")])
                                    (split-message m)
                          )
                        )

         (define/public (recieve-message)
                        #f)

         ; Sign in / out of chat
         (define/public (signin username)
                        (command CMD_SIGNIN username))
         (define/public (signout username)
                        (command CMD_SIGNOUT username))

         ; Send messages to the chat
         (define/public (send in out msg)
                        (command CMD_SEND msg))

         )
  )
