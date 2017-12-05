;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname yo-client2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Yo App - exchange "yo" messages with users online
;; Use (run '!) to start a demo.

(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A World is a (make-world Name [Maybe Name] (Listof Name) (Listof History))
(define-struct world (you them users history))

;; A History is a (make-history Name [Listof Chat])
(define-struct history (user chats))

;; A Chat is one of:
;; - (make-in String)
;; - (make-out String)
(define-struct in (msg))
(define-struct out (msg))

;; A Name is a String


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yo Client

;; yo-client : Name -> World
;; Start a yo client with the given username
(define (yo-client username)
  (big-bang (make-world username #false '() '())
            [name username]
            [register LOCALHOST]
            [on-receive handle-msg]
            [on-key handle-key]
            [to-draw draw-world]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example: run 3 clients (assumes server is already running)

(define (run _)
   (launch-many-worlds (yo-client "DVH")
                       (yo-client "AB")
                       (yo-client "RZ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define FONT-SIZE 12)
(define LINE-HEIGHT (* 1.4 FONT-SIZE))
(define WIDTH 200)
(define LINE (rectangle WIDTH LINE-HEIGHT "solid" "white"))
(define HLINE (rectangle WIDTH LINE-HEIGHT "solid" "yellow"))
(define USR-WINDOW-HEIGHT 100)
(define MSG-WINDOW-HEIGHT 300)
(define SCENE-HEIGHT (+ USR-WINDOW-HEIGHT MSG-WINDOW-HEIGHT))
(define OUT-COLOR "plum")
(define IN-COLOR "light blue")

(define W0 (make-world "Me" #false (list "You") '()))
(define W1 (make-world "Me" "You" (list "You") '()))
(define W2 (make-world "Me" "You" (list "You")
                       (list (make-history "You" (list (make-out "yo"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Handlers

;; handle-msg : World SExpr -> HandlerResult
;; Receive a message from the server
(check-expect (handle-msg W0 (list "users" (list "A" "B" "C")))
              (receive-users W0 (list "A" "B" "C")))
(check-expect (handle-msg W0 (list "from" "You" "yo"))
              (receive-chat W0 "You" "yo"))
(check-expect (handle-msg W0 (list "username?"))
              (make-package W0 (list "username" "Me")))
(check-expect (handle-msg W0 "bogus") W0)
(define (handle-msg w msg)  
  (cond [(valid-message? msg)
         (local [(define tag (first msg))]
           (cond [(string=? tag "users")
                  (receive-users w (second msg))]
                 [(string=? tag "from")
                  (receive-chat w (second msg) (third msg))]    
                 [(string=? tag "username?")
                  (make-package w (list "username" (world-you w)))]))]
        [else w]))

;; handle-key : World KeyEvent -> HandlerResult
;; Handle key events; numeric keys select user, enter key tries to send yo
(check-expect (handle-key W0 "1") W1)
(check-expect (handle-key W0 "\r") W0)
(check-expect (handle-key W1 "\r") (send-yo W1))
(check-expect (handle-key W0 "a") W0)
(define (handle-key w ke)
  (cond [(number? (string->number ke))
         (select-user w (string->number ke))]
        [(key=? "\r" ke)
         (send-yo w)]
        [else w]))

;; draw-world : World -> Image
;; Draw list of users above history of currently select user
(define (draw-world w)  
  (place-image/align
   (above (show-users (world-them w) (world-users w))
          (above (line WIDTH 0 (pen "red" 1 "long-dash" "round" "bevel"))
                 (show-chats (select-chats (world-them w) (world-history w)))))
   1 1
   "left" "top"
   (empty-scene (+ 2 WIDTH) (+ 3 SCENE-HEIGHT))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message receivers

;; receive-users : World [Listof String] -> World
;; Receive list of users online
(define (receive-users w us)
  (make-world (world-you w)
              (world-them w)
              (filter (λ (n) (not (string=? n (world-you w)))) us)
              (world-history w)))


;; receive-chat : World Name String -> World
;; Receive a chat message (content) from given user
(define (receive-chat w from content)
  (make-world (world-you w)
              (world-them w)
              (world-users w)
              (update-history from
                              content
                              (world-history w)
                              make-in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message validity checking

;; valid-message? : SExpr -> Boolean
;; Determe if message is one of:
;; - (list "username?")
;; - (list "users" [Listof String])
;; - (list "from" String String)
(check-expect (valid-message? (list "username?")) #true)
(check-expect (valid-message? (list "from" "DVH" "yo")) #true)
(check-expect (valid-message? (list "users" (list "DVH" "AB"))) #true)
(check-expect (valid-message? #false) #false)
(check-expect (valid-message? (list "username?" "blah")) #false)
(check-expect (valid-message? (list "from")) #false)
(check-expect (valid-message? (list "users")) #false)
(check-expect (valid-message? (list "users" (list #false))) #false)
(define (valid-message? msg)
  (and (cons? msg)
       (string? (first msg))
       (if (empty? (rest msg))
           (string=? (first msg) "username?")
           (cond [(string=? (first msg) "users")
                  (and (list? (second msg))
                       (andmap string? (second msg)))]                 
                 [(string=? (first msg) "from")
                  (and (cons? (rest msg))
                       (cons? (rest (rest msg)))
                       (empty? (rest (rest (rest msg))))
                       (string? (second msg))
                       (string? (third msg)))]
                 [else #false]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select-user : World Digit -> World
;; Select user in list of users counting from 1; invalid selection does nothing
(check-expect (select-user W0 1) W1)
(check-expect (select-user W0 2) W0)
(define (select-user w n)
  (cond [(< 0 n (add1 (length (world-users w))))
         (make-world (world-you w)
                     (list-ref (world-users w) (sub1 n))
                     (world-users w)
                     (world-history w))]
        [else w]))

;; send-yo : World -> HandlerResult
;; Send yo message to selected user (if there is one; update chat-history
(check-expect (send-yo W0) W0)
(check-expect (send-yo W1)
              (make-package (yo-history W1)
                            (list "message" "You" "yo")))
(define (send-yo w)
  (cond [(false? (world-them w)) w]
        [else
         (make-package (yo-history w)
                       (list "message" (world-them w) "yo"))]))


;; yo-history : World -> World
;; Add sent "yo" to the chat history with current user
;; Assume: there is a user selected
(check-expect (yo-history W1) W2)             
(define (yo-history w)
  (make-world (world-you w)
              (world-them w)
              (world-users w)
              (update-history (world-them w)
                              "yo"
                              (world-history w)
                              make-out)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GUI

;; show-users : [Maybe Name] [Listof Name] -> Image
;; Render list of users, highlighting the selected one (if exists)
(check-expect (show-users #false (list "A" "B"))
              (crop/align
               "left" "top"
               WIDTH USR-WINDOW-HEIGHT
               (above (show-user 1 "A" LINE)
                      (show-user 2 "B" LINE))))
(check-expect (show-users "B" (list "A" "B"))
              (crop/align
               "left" "top"
               WIDTH USR-WINDOW-HEIGHT
               (above (show-user 1 "A" LINE)
                      (show-user 2 "B" HLINE))))
(define (show-users selected users)
  (local [;; show-user/select : Number Name -> Image
          ;; Show user, highlight if selected
          (define (show-user/select n u)
            (show-user n u
                       (if (and (string? selected) (string=? selected u))
                           HLINE
                           LINE)))]
    (crop/align
     "left" "top"
     WIDTH USR-WINDOW-HEIGHT
     (foldr above empty-image
            (map show-user/select (build-list (length users) add1) users)))))

;; show-user : Number Name Image -> Image
;; Show numbered user on background of given color
(check-expect (show-user 5 "DVH" LINE)
              (overlay/align "left" "middle"
                             (text " 5: DVH" FONT-SIZE "black")
                             LINE))
(define (show-user n name line)
  (overlay/align "left" "middle"
                 (text (string-append " " (number->string n) ": " name)
                       FONT-SIZE
                       "black")
                 line))
;; show-chats : [Listof Chat] -> Image
;; Render all of the chats
(check-expect (show-chats '())
              (crop/align "left" "bottom" WIDTH MSG-WINDOW-HEIGHT empty-image))
(check-expect (show-chats (list (make-out "yo")))
              (crop/align "left" "bottom"
                          WIDTH MSG-WINDOW-HEIGHT
                          (show-chat (make-out "yo"))))
(define (show-chats chats)
  (crop/align "left" "bottom"
              WIDTH MSG-WINDOW-HEIGHT
              (foldl above empty-image
                     (map show-chat chats))))

;; show-chat : Chat -> Image
;; Render a single chat
(check-expect (show-chat (make-in "yo"))
              (left-line (show-txt "yo" IN-COLOR)))
(check-expect (show-chat (make-out "yo"))
              (right-line (show-txt "yo" OUT-COLOR)))
(define (show-chat c)  
  (cond [(in? c)  (left-line (show-txt (in-msg c) IN-COLOR))]
        [(out? c) (right-line (show-txt (out-msg c) OUT-COLOR))]))

;; left-line : Image -> Image
;; Put image on left of LINE
(define (left-line img)
  (overlay/align "left" "middle" img LINE))

;; right-line : Image ->Image
;; Put image on right of LINE
(define (right-line img)
  (overlay/align "right" "middle" img LINE))

;; show-txt : String Color -> Image
;; Render a string on given color background
(define (show-txt s c)
  (local [(define txt (text s FONT-SIZE "black"))]
    (overlay txt
             (rectangle (* 1.5 (image-width txt)) LINE-HEIGHT "solid" c))))

;; select-chats : [Maybe Name] [Listof History] -> [Listof Chats]
;; Select all of the chats from the given user
(define (select-chats name h)
  (cond [(false? name) '()]
        [(empty? h) '()]
        [(cons? h)
         (if (string=? (history-user (first h)) name)
             (history-chats (first h))
             (select-chats name (rest h)))]))

;; update-history : String String [Listof History] [String -> Chat]
;;    -> [Listof History]
(define (update-history from content h in/out)
  (cond [(empty? h) (list (make-history from (list (in/out content))))]
        [(cons? h)
         (if (string=? (history-user (first h)) from)
             (cons (make-history from (cons (in/out content)
                                            (history-chats (first h))))
                   (rest h))
             (cons (first h)
                   (update-history from content (rest h) in/out)))]))
