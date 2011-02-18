; Chat client.
#lang racket
(provide 
  client%
  )

(require "chat-protocol.rkt")
(require "lib/logging.rkt")

; These should be configurable
(define HOSTNAME "localhost")
(define PORT 8081)
(define USERNAME "emou")

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
             (values (read-header in)
                     (read-message in))
             )
           )

         (define/public (recieve-message)
           #f
           )

         ; Sign in / out of chat
         (define/public (signin username)
           (command CMD_SIGNIN username)
           )
         (define/public (signout username)
           (command CMD_SIGNOUT username)
           )

         ; Send messages to the chat
         (define/public (send in out msg)
           (command CMD_SEND msg)
           )

         )
  )
