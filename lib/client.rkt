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
         (init host port push-handler)
         (define (get-host) host)
         (define (get-port) port)
         (define (get-push-handler) push-handler)
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

         (define/public (command cmd msg)
                        (begin
                          (write-header cmd out)
                          (write-message msg out)
                          (debug-message "Request sent. Reading response...")
                          (check-retcode in)
                          )
                        )

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

         ; The idea is that the user of the client class
         ; can check for events on `in`
         (define/public (get-input-port) in)

         ; Same idea.
         (define/public (get-output-port) out)

         (define (check-retcode in)
           (let* ([header (read-header in)]
                  [msg  (read-message in)]
                  [rc (get-retcode header)])
             (cond [(is-push? rc) ; We received a push. So get it before sending our crud.
                     (begin
                       ((get-push-handler) rc msg)
                       (check-retcode in))]

                    [(= rc RET_OK) (values header msg)]
                    [else
                      (raise-server-error rc msg)]))
           )

         )
  )
