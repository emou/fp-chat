#lang racket
(provide HEADER_SIZE
         VERSION_BYTE
         CODE_BYTE
         VERSION
         CMD_SIGNIN
         CMD_SIGNOUT
         CMD_SEND
         EOM
         ERR_PROTO_VERSION
         ERR_UNKNOWN_CMD
         ERR_USER_TAKEN
         ERR_UNKNOWN_USER
         RET_OK
         make-header
         get-header
         get-proto-version
         get-command
         read-message
         read-header
         write-message
         write-header
         version-match?)

(define VERSION 1)          ; Protocol version used in the server

; Request header
(define HEADER_SIZE 4)      ; Size in bytes of the message header
(define VERSION_BYTE 0)     ; Byte 0 contains protocol version number
(define CODE_BYTE 1)        ; This field contains server commands in requests
; or return codes from the server to the client in responses

; Server commands in requests to the server
(define CMD_SIGNIN  0)      ; Start a new session
(define CMD_SIGNOUT 1)      ; Quit the chat
(define CMD_SEND    2)      ; Send a message

; Server return codes from the server to the client
; On error, message bodies are empty
(define RET_OK 0)           ; Everything went fine
(define ERR_USER_TAKEN 1)   ; Nickname was already taken
(define ERR_NO_ROOM 2)      ; Too many clients
(define ERR_PROTO_VERSION 3); Invalid protocol version
(define ERR_UNKNOWN_CMD 4)  ; Unknown error
(define ERR_UNKNOWN_USER 5) ; No such user is currently logged in

(define EOM #\return)           ; \r means end of message.
; Note that the header can still contain null-bytes.

(define CHUNK_SIZE 128)     ; size in bytes of the input chunks

; Get the header of the message
(define (get-header in)
  (read-bytes HEADER_SIZE in)
  )

; Extract information from the header
(define (get-proto-version header)
  (bytes-ref header VERSION_BYTE)
  )

(define (get-command header)
  (bytes-ref header CODE_BYTE)
  )

; Checks if the request protocol version matches the server
(define (version-match? header)
  (= (get-proto-version header) VERSION)
  )

; Header
(define (make-header code)
  (let ((header (make-bytes HEADER_SIZE 0)))
    (begin
      (bytes-set! header VERSION_BYTE VERSION)
      (bytes-set! header CODE_BYTE code)
      )
    header
    )
  )

(define (write-header code out)
  (begin
    (write-bytes (make-header code) out)
    )
  )

; Message
(define (read-message in)
  (let ([buff (read-bytes-line in 'return)])
    (if (eof-object? buff)
      #f
      (bytes->string/utf-8 buff))
    )
  )

(define (read-header in)
  (read-bytes HEADER_SIZE in)
  )

(define (write-message msg out)
  (begin
    (write-bytes (string->bytes/utf-8 msg) out)
    (write-char EOM out)
    )
  )
