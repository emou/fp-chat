#lang racket
(provide HEADER_SIZE
         VERSION_BYTE
         CODE_BYTE
         VERSION
         CMD_SIGNIN
         CMD_SIGNOUT
         CMD_SEND
         EOM
         make-header
         get-header
         get-proto-version
         get-command
         read-message
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
(define RET_USER_TAKEN 1)   ; Nickname was already taken
(define RET_NO_ROOM 2)      ; Too many clients

(define EOM 0)              ; NULL Byte means end of message.
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

; Make the header of the message
(define (make-header code)
  (let ((header (make-bytes HEADER_SIZE 0)))
    (begin
      (bytes-set! header VERSION_BYTE VERSION)
      (bytes-set! header CODE_BYTE code)
      )
    header
    )
  )

(define (read-message in)
  (let ((out (open-output-bytes)))
    (define (reader)
      (let ((head (peek-byte in)))
        (if (not (or (= head EOM) (eof-object? head)))
          (begin
            (write-bytes (read-bytes CHUNK_SIZE in) out)
            (reader)
            )
          (bytes->string/utf-8 out)
          )
        )
      )
    (reader)
    )
  )
