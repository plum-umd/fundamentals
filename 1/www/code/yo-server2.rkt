;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname yo-server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)

;; A UState is a [Listof (make-user IWorld [Maybe Name])]
(define-struct user (iw name))

;; A Name is a String

(define (run-server _)
  (universe '()
            [on-new receive-new]
            [on-msg receive-msg]))

(define U0 (list (make-user iworld1 "AB")
                 (make-user iworld2 #false)))
(define U1 (list (make-user iworld1 "AB")
                 (make-user iworld2 "DVH")))

;; receive-new : UState IWorld -> Bundle
;; Register a new user, request username
(check-expect (receive-new U1 iworld1)
              (make-bundle (cons (make-user iworld1 #false) U1)
                           (list (make-mail iworld1 (list "username?")))
                           '()))
(define (receive-new u iw)
  (make-bundle (cons (make-user iw #false) u)
               (list (make-mail iw (list "username?")))
               '()))

;; receive-msg : UState IWorld SExpr -> Resonse
;; Receive a message, either register user or broadcast to receipient
(check-expect (receive-msg U0 iworld2 (list "username" "DVH"))
              (broadcast-users (update-username U0 iworld2 "DVH")))
(check-expect (receive-msg U1 iworld2 (list "message" "DVH" "Yo"))
              (send-message U1 iworld2 "DVH" "Yo"))
(check-expect (receive-msg U1 iworld1 (list "bogus")) U1)
(check-expect (receive-msg U1 iworld1 "bogus") U1)
(define (receive-msg u iw msg)
  (cond [(and (list? msg) (andmap string? msg) (>= (length msg) 2))
         (local [(define tag (first msg))]
           (cond [(string=? tag "username")
                  (broadcast-users (update-username u iw (second msg)))]
                 [(string=? tag "message")
                  (send-message u iw (second msg) (third msg))]
                 [else u]))]
        [else u]))                  

;; update-username : UState IWorld Name -> UState
;; Associate given username with iworld
(check-expect (update-username '() iworld1 "DVH") '())
(check-expect (update-username U0 iworld2 "DVH") U1)
(define (update-username u iw name)
  (cond [(empty? u) u]
        [(cons? u)
         (if (iworld=? (user-iw (first u)) iw)
             (cons (make-user iw name) (rest u))
             (cons (first u)
                   (update-username (rest u) iw name)))]))

;; broadcast-users : UState -> Bundle
;; Broadcast the name of all the users to all users
(check-expect (broadcast-users '()) (make-bundle '() '() '()))
(check-expect
 (broadcast-users U1)
 (make-bundle U1
              (list (make-mail iworld1 (list "users" (list "AB" "DVH")))
                    (make-mail iworld2 (list "users" (list "AB" "DVH"))))
              '()))
(define (broadcast-users u)
  (local [(define user-names (filter string? (map user-name u)))]
    (make-bundle u
                 (map (λ (usr)
                        (make-mail (user-iw usr) (list "users" user-names)))
                      u)
                 '())))

;; send-message : UState IWorld Name String -> Response
(check-expect (send-message '() iworld1 "DVH" "Yo") (make-bundle '() '() '()))
(check-expect (send-message U0 iworld1 "DVH" "Yo") (make-bundle U0 '() '()))
(check-expect (send-message U1 iworld1 "DVH" "Yo")
              (make-bundle U1
                           (list (make-mail iworld2 (list "from" "AB" "Yo")))
                           '()))              
(define (send-message u iw name content)
  (make-bundle u (send-mail u iw name content) '()))


;; send-mail : UState IWorld Name String -> [Listof Mail]
;; Send content to iworld assocaite with username (if any)
(check-expect (send-mail '() iworld1 "DVH" "Yo") '())
(check-expect (send-mail U0 iworld1 "DVH" "Yo") '())
(check-expect (send-mail U1 iworld1 "DVH" "Yo")
              (list (make-mail iworld2 (list "from" "AB" "Yo"))))
(define (send-mail u iw to content)
  (local [(define from-name (lookup-name u iw))
          (define to-iw     (lookup-iw u to))]
    (cond [(false? from-name) '()]
          [(false? to-iw) '()]
          [else
           (list (make-mail to-iw (list "from" from-name content)))])))

;; lookup-name : UState IWorld -> [Maybe Name]
;; Look up username associated with iworld, if any
(check-expect (lookup-name '() iworld1) #false)
(check-expect (lookup-name U0 iworld2) #false)
(check-expect (lookup-name U1 iworld2) "DVH")
(define (lookup-name u iw)
  (cond [(empty? u) #false]
        [(cons? u)
         (if (iworld=? (user-iw (first u)) iw)
             (user-name (first u))
             (lookup-name (rest u) iw))]))

;; lookup-iw : UState Name -> [Maybe IWorld]
;; Look up iworld associated with username, if any
(check-expect (lookup-iw '() "DVH") #false)
(check-expect (lookup-iw U1 "DVH") iworld2)
(define (lookup-iw u name)
  (cond [(empty? u) #false]
        [(cons? u)
         (if (and (string? (user-name (first u)))
                  (string=? (user-name (first u)) name))
             (user-iw (first u))
             (lookup-iw (rest u) name))]))

  

         
                    
                    