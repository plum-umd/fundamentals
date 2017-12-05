;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname chatclient) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Simple chat client

;;; Data Definitions
;;

(define-struct msg (name content))
;; A Message is a (make-msg String String).

;; message-template : Message -> ???
(define (message-template msg)
  (... (msg-name msg) ... (msg-content msg) ...))

(define msg0 (make-msg "DVH" "Hello, world!"))
(define msg1 (make-msg "Nick & Sam" "Help! What should we do?!"))
(define msg2 (make-msg "DVH" "Do not be afraid; do not be discouraged, for the Design Recipe will be with you wherever you go."))


;; A Natural is a non-negative integer.

(define-struct textbox (content cursor))
;; A Textbox is a (make-textbox String Natural)
;; Interp: a textbox (make-textbox str n) has its cursor immediately before the
;; `n'th character of `str'. The cursor's value must never exceed the length of
;; the string.

(define (textbox-template tb)
  (... (textbox-content tb) ... (textbox-cursor tb) ...))

(define tb0 (make-textbox "foo " 0))
(define tb1 (make-textbox "foo " 1))
(define tb2 (make-textbox "foo " 2))
(define tb3 (make-textbox "foo " 3))
(define tb4 (make-textbox "foo " 4))

;; A History is a [Listof Message]
;; Interp: All seen messages, where newer messages are toward the head.
(define hist0 '())
(define hist1 (list msg0))
(define hist2 (cons msg1 hist1))
(define hist3 (cons msg2 hist2))

(define-struct client (tb hist name))
;; A ChatClient is a (make-client Textbox History String)
;; Interp: The local state of a chat client.
(define name0 "Foo")
(define name1 "Bazoinks")
(define name2 "Really long name right here")
(define cc0 (make-client tb0 hist0 name0))
(define cc1 (make-client tb1 hist1 name1))
(define cc2 (make-client tb2 hist2 name2))

;; A Sexp is one of:
;; - String
;; - Symbol
;; - Number
;; - Boolean
;; - Char
;; - [Listof Sexp]

;; A Package is a (make-package ChatClient Sexp)

;; A HandlerResult is one of:
;; - WorldState
;; - Package


;;;
;; Drawing functions

;; message->image : Message -> Image
;; Create an image representation of the given message.
(define MSG-NAME-COLOR "red")
(define MSG-NAME-SIZE 16)
(define MSG-CTNT-COLOR "orange")
(define MSG-CTNT-SIZE 14)
(define MSG-DELIM ": ")
#;(define (message->image msg) empty-image) ; <- stub
(define (message->image msg)
  (beside (text (msg-name msg) MSG-NAME-SIZE MSG-NAME-COLOR)
          (text (string-append MSG-DELIM (msg-content msg))
                MSG-CTNT-SIZE
                MSG-CTNT-COLOR)))

(check-expect (message->image msg0)
              (beside (text (msg-name msg0) MSG-NAME-SIZE MSG-NAME-COLOR)
                      (text (string-append MSG-DELIM (msg-content msg0))
                            MSG-CTNT-SIZE MSG-CTNT-COLOR)))
(check-expect (message->image msg1)
              (beside (text (msg-name msg1) MSG-NAME-SIZE MSG-NAME-COLOR)
                      (text (string-append MSG-DELIM (msg-content msg1))
                            MSG-CTNT-SIZE MSG-CTNT-COLOR)))
(check-expect (message->image msg2)
              (beside (text (msg-name msg2) MSG-NAME-SIZE MSG-NAME-COLOR)
                      (text (string-append MSG-DELIM (msg-content msg2))
                            MSG-CTNT-SIZE MSG-CTNT-COLOR)))


;; textbox->image : Textbox -> Image
;; Create an image representation of the given Textbox.
(define TBCTNT-COLOR "orange")
(define TBCTNT-SIZE 16)
(define TBCURS-COLOR "red")
(define TBCURS-HEIGHT 20)
(define TBCURS-WIDTH 2)
(define TBCURS (rectangle TBCURS-WIDTH TBCURS-HEIGHT 'solid TBCURS-COLOR))
(define TB-BORDER 5)
(define TB-WIDTH 200)
(define TB-HEIGHT (+ (* 2 TB-BORDER) (image-height TBCURS)))
(define (textbox->image tb)
  (overlay/align "left" "middle"
                 (beside (rectangle TB-BORDER (- TB-HEIGHT 2) "solid" "white")
                         (text (before-cursor tb) TBCTNT-SIZE TBCTNT-COLOR)
                         TBCURS
                         (text (after-cursor  tb) TBCTNT-SIZE TBCTNT-COLOR))
                 (empty-scene TB-WIDTH TB-HEIGHT)))

;; history->image : History -> Image
;; Create an image representation of the history.
(define (history->image h)
  (foldl (λ (m i) (above/align "left" (message->image m) i)) empty-image h))

(check-expect (history->image hist0) empty-image)
(check-expect (history->image hist1) (message->image msg0))
(check-expect (history->image hist2) (above/align "left"
                                                  (message->image msg0)
                                                  (message->image msg1)))

;; client->image : ChatClient -> Image
;; Create an image representation of the chat client.
;;                                
;;   +--------------------------+ -+-
;;   |                          |  |
;;   |                          |  |
;;   |                          |  |
;;   |         History          | 200px
;;   |                          |  |
;;   |                          |  |
;;   |                          |  |
;;   +--------------------------+ -+-
;;   |   Name:  textbox ...     | 20px
;;   +--------------------------+ -+-
;;
;;   |-100px-|------200px--------|
;;
(define HIST-HEIGHT 200)
(define NM-SIZE TBCTNT-SIZE)
(define NM-COLOR "blue")
(define NM-WIDTH 100)
(define NM-HEIGHT TB-HEIGHT)
(define CLIENT-WIDTH (+ NM-WIDTH TB-WIDTH))
(define (client->image c)
  (local [;; trunc : Image Natural Natural -> Image
          (define (trunc i w h)
            (place-image i
                         (+ TB-BORDER (/ (image-width i) 2))
                         (- h TB-BORDER (/ (image-height i) 2))
                         (empty-scene w h)))
          (define hist
            (trunc (history->image (client-hist c)) CLIENT-WIDTH HIST-HEIGHT))
          (define name (trunc (text (string-append (client-name c) ": ") NM-SIZE NM-COLOR)
                              NM-WIDTH
                              NM-HEIGHT))
          (define txt (textbox->image (client-tb c)))]
    (above hist (beside/align "center" name txt))))


;;;
;; Key-handling functions

;; before-cursor : Textbox -> String
;; Return the string content of tb that comes before its cursor.
#;(define (before-cursor tb) "")
(define (before-cursor tb)
  (substring (textbox-content tb) 0 (textbox-cursor tb)))
(check-expect (before-cursor tb0) "")
(check-expect (before-cursor tb1) "f")
(check-expect (before-cursor tb2) "fo")
(check-expect (before-cursor tb3) "foo")
(check-expect (before-cursor tb4) "foo ")

;; after-cursor : Textbox -> String
;; Return the string content of tb that comes after its cursor.
#;(define (after-cursor tb) "")
(define (after-cursor tb) 
  (substring (textbox-content tb)
             (textbox-cursor tb)
             (string-length (textbox-content tb))))
(check-expect (after-cursor tb0) "foo ")
(check-expect (after-cursor tb1) "oo ")
(check-expect (after-cursor tb2) "o ")
(check-expect (after-cursor tb3) " ")
(check-expect (after-cursor tb4) "")

;; textbox-left : Textbox -> Textbox
;; Move the cursor left in the textbox.
(define (textbox-left tb)
  (make-textbox (textbox-content tb) (max (- (textbox-cursor tb) 1) 0)))
(check-expect (textbox-left tb0) tb0)
(check-expect (textbox-left tb1) tb0)
(check-expect (textbox-left tb2) tb1)

;; textbox-right : Textbox -> Textbox
;; Move the cursor right in the textbox.
(define (textbox-right tb)
  (make-textbox (textbox-content tb)
                (min (+ (textbox-cursor tb) 1)
                     (string-length (textbox-content tb)))))
(check-expect (textbox-right tb0) tb1)
(check-expect (textbox-right tb1) tb2)
(check-expect (textbox-right tb4) tb4)

;; textbox-delete : Textbox -> Textbox
;; Delete the character to the right of the cursor.
(define (textbox-delete tb)
  (let ([left (before-cursor tb)]
        [right (after-cursor tb)])
    (cond [(= 0 (string-length right)) tb]
          [else (make-textbox (string-append left (substring right 1))
                              (textbox-cursor tb))])))
(check-expect (textbox-delete tb0) (make-textbox "oo " 0))
(check-expect (textbox-delete tb1) (make-textbox "fo " 1))
(check-expect (textbox-delete tb4) tb4)

;; textbox-backspace : Textbox -> Textbox
;; Delete the character to the left of the cursor.
(define (textbox-backspace tb)
  (let ([left (before-cursor tb)]
        [right (after-cursor tb)])
    (cond [(= 0 (string-length left)) tb]
          [else (let ([left* (substring left 0 (- (string-length left) 1))])
                  (make-textbox (string-append left* right)
                                (- (textbox-cursor tb) 1)))])))
(check-expect (textbox-backspace tb0) tb0)
(check-expect (textbox-backspace tb1) (make-textbox "oo " 0))
(check-expect (textbox-backspace tb4) (make-textbox "foo" 3))

;; textbox-insert : Textbox 1String -> Textbox
;; Insert the given singleton string after the cursor in the textbox.
(define (textbox-insert tb s)
  (make-textbox (string-append (before-cursor tb)
                               s
                               (after-cursor tb))
                (+ 1 (textbox-cursor tb))))
(check-expect (textbox-insert tb0 " ") (make-textbox " foo " 1))
(check-expect (textbox-insert tb1 "X") (make-textbox "fXoo " 2))
(check-expect (textbox-insert tb4 "b") (make-textbox "foo b" 5))

;; textbox-handle-key : Textbox KeyEvent -> Textbox
;; Create a new textbox given some key event.
(define (textbox-handle-key tb ke)
  (cond [(key=? ke "left") (textbox-left tb)]
        [(key=? ke "right") (textbox-right tb)]
        [(key=? ke "\b") (textbox-backspace tb)]
        [(key=? ke "\u007F") (textbox-delete tb)]
        [(= (string-length ke) 1) (textbox-insert tb ke)]
        [else tb]))

;; client-handle-key : ChatClient KeyEvent -> HandlerResult
;; Update the chat client given some key event.
(define (client-handle-key c ke)
  (cond [(key=? ke "\r") (send-message c)]
        [else (make-client (textbox-handle-key (client-tb c) ke)
                           (client-hist c)
                           (client-name c))]))

;; send-message : ChatClient -> Package
;; Create a package which adds a message to the chat client and chat server.
(define (send-message c)
  ...)

;; receive-message : ChatClient Sexp -> ChatClient
(define (receive-message c m)
  ...)


(define (run-client c)
  (big-bang c
    [register "home.labich.org"]
    [port 8889]
    [name (string->symbol (client-name c))]
    [to-draw client->image]
    [on-key client-handle-key]
    [on-receive receive-message]))


