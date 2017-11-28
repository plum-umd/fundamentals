;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ml) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Content is a [Listof Item]
; Interp: a collection of content in a markup document
 
; An Item is one of:
; - a String
; - an Element
; Interp: an item in a document, either text or an element
 
; An Element is a (make-elem Tag Content)
; Interp: an element is content annotated with a tag
(define-struct elem (tag content))
 
; A Tag is a String
; Interp: the name of the tag, e.g. "p"

;; : Content -> ?
#;
(define (content-template c)
  (cond [(empty? c) ...]
        [(cons? c)
         (... (item-template (first c))
              (content-template (rest c)))]))

;; : Item -> ?
#;
(define (item-template i)
  (cond [(string? i) ...]
        [(elem? i) (... (elem-template i) ...)]))

;; : Element -> ?
#;
(define (elem-template e)
  (... (elem-tag e) (content-template (elem-content e)) ...))

(define P-HI (make-elem "p" (list "hi")))
(define I-HI (make-elem "i" (list "hi")))
(define B-HI (make-elem "b" (list "hi")))
(define IP-HI (make-elem "i" (list (make-elem "p" (list "hi")))))
(define II-HI (make-elem "i" (list (make-elem "i" (list "hi")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1:

;; NOTE: there is some subtlety about white space that we ignore.
;; For example, you might want to represent the newline and white space between
;; the end of </h1> and <p>, or you might choose to ignore it.

;; This chooses to ignore some whitespace:
(define eg1
  (list (make-elem "h1" (list "Mark it"))
        (make-elem "p"
                   (list "Edit a file named "
                         (make-elem "b" (list "EXACTLY"))
                         " "
                         (make-elem "tt" (list "ml.rkt"))
                         " for this part of the assignment.  "
                         "Add your information to the standard file header "
                         "at the top of the file."))
        (make-elem "p"
                   (list "Markup languages "
                         (make-elem "i" (list "abound"))
                         ".  Examples include HTML, XML, XHTML, LaTeX, troff, "
                         "YAML, and many many more. A"
                         (make-elem "b" (list "markup language"))
                         " is used to..."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2: content-to-HTML

;; content-to-HTML : Content -> String
;; Convert content to HTML string
(check-expect (content-to-HTML '()) "")
(check-expect (content-to-HTML (list P-HI)) "<p>hi</p>")
(define (content-to-HTML c)
  ;; Alt w/abs:
  #;(foldr (λ (i s) (string-append (item-to-HTML i) s)) "" c)
  (cond [(empty? c) ""]
        [(cons? c)
         (string-append (item-to-HTML (first c))
                        (content-to-HTML (rest c)))]))

;; item-to-HTML : Item -> String
;; Convert item to HTML string
(check-expect (item-to-HTML "hi") "hi")
(check-expect (item-to-HTML P-HI) "<p>hi</p>")
(define (item-to-HTML i)
  (cond [(string? i) i]
        [(elem? i) (elem-to-HTML i)]))

;; elem-to-HTML : Element -> String
;; Convert element to HTML string
(check-expect (elem-to-HTML P-HI) "<p>hi</p>")
(define (elem-to-HTML e)
  (string-append "<" (elem-tag e) ">"
                 (content-to-HTML (elem-content e))
                 "</" (elem-tag e) ">"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3: content-contains-tag?

;; content-contains-tag? : Content Tag -> Boolean
;; Does the content contain an element with given tag name?
(check-expect (content-contains-tag? '() "p") #false)
(check-expect (content-contains-tag? (list "p") "p") #false)
(check-expect (content-contains-tag? (list P-HI) "p") #true)
(define (content-contains-tag? c t)
  ;; Alt w/abs:
  #;(ormap (λ (i) (item-contains-tag? i t)) c)
  (cond [(empty? c) #false]
        [(cons? c)
         (or (item-contains-tag? (first c) t)
             (content-contains-tag? (rest c) t))]))

;; item-contains-tag? : Item Tag -> Boolean
;; Does the item contain an element with given tag name?
(check-expect (item-contains-tag? "p" "p") #false)
(check-expect (item-contains-tag? P-HI "p") #true)
(define (item-contains-tag? i t)
  (cond [(string? i) #false]
        [(elem? i) (elem-contains-tag? i t)]))

;; elem-contains-tag? : Element Tag -> Boolean
;; Does the element contain an element with given tag name?
(check-expect (elem-contains-tag? P-HI "p") #true)
(check-expect (elem-contains-tag? P-HI "i") #false)
(check-expect (elem-contains-tag? IP-HI "p") #true)
(define (elem-contains-tag? e t)
  (or (string=? t (elem-tag e))
      (content-contains-tag? (elem-content e) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4: content-length

;; content-length : Content -> Number
;; Sum length of all textual items in content
(check-expect (content-length '()) 0)
(check-expect (content-length (list "hi")) 2)
(check-expect (content-length (list P-HI)) 2)
(define (content-length c)
  ;; Alt w/abs:
  #;(foldr (λ (i n) (+ (item-length i) n)) 0 c)
  (cond [(empty? c) 0]
        [(cons? c)
         (+ (item-length (first c))
            (content-length (rest c)))]))

;; item-length : Item -> Number
;; Sum length of all textual items in item
(check-expect (item-length "hi") 2)
(check-expect (item-length P-HI) 2)
(define (item-length i)
  (cond [(string? i) (string-length i)]
        [(elem? i) (elem-length i)]))

;; elem-length : Element -> Number
;; Sum length of all textual items in element
(check-expect (elem-length P-HI) 2)
(define (elem-length e)
  (content-length (elem-content e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5: content-contains-elem?

;; p? : Element -> Boolean
;; Is the element tagged with "p"?
(define (p? e)
  (string=? "p" (elem-tag e)))

;; content-contains-elem? : Content (Element -> Boolean) -> Boolean
;; Does the content contain an element satisfying pred?
(check-expect (content-contains-elem? '() p?) #false)
(check-expect (content-contains-elem? (list P-HI) p?)
              #true)
(define (content-contains-elem? c pred)
  ;; Alt w/abs:
  #;(ormap (λ (i) (item-contains-elem? i pred)) c)
  (cond [(empty? c) #false]
        [(cons? c)
         (or (item-contains-elem? (first c) pred)
             (content-contains-elem? (rest c) pred))]))

;; item-contains-elem? : Item (Element -> Boolean) -> Boolean
;; Does the item contain an element satisfying pred?
(check-expect (item-contains-elem? "hi" p?) #false)
(check-expect (item-contains-elem? P-HI p?) #true)
(define (item-contains-elem? i pred)
  (cond [(string? i) #false]
        [(elem? i) (elem-contains-elem? i pred)]))

;; elem-contains-elem? : Element (Element -> Boolean) -> Boolean
;; Does the element contain an element satisfying pred?
(check-expect (elem-contains-elem? P-HI p?) #true)
(check-expect (elem-contains-elem? I-HI p?) #false)
(check-expect (elem-contains-elem? IP-HI p?) #true)
(define (elem-contains-elem? e pred)
  (or (pred e)
      (content-contains-elem? (elem-content e) pred)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6: content-retag

;; content-retag : Content Tag Tag -> Content
;; Change tag of any element named t1 to t2 in content
(check-expect (content-retag '() "p" "i") '())
(check-expect (content-retag (list P-HI) "p" "i")
              (list (make-elem "i" (list "hi"))))
(define (content-retag c t1 t2)
  ;; Alt w/abs:
  #;(map (λ (i) (item-retag i t1 t2)) c)
  (cond [(empty? c) '()]
        [(cons? c)
         (cons (item-retag (first c) t1 t2)
               (content-retag (rest c) t1 t2))]))

;; item-retag : Item Tag Tag -> Item
;; Change tag of any element named t1 to t2 in item
(check-expect (item-retag "hi" "p" "i") "hi")
(check-expect (item-retag P-HI "p" "i")
              (make-elem "i" (list "hi")))
(define (item-retag i t1 t2)
  (cond [(string? i) i]
        [(elem? i) (elem-retag i t1 t2)]))

;; elem-retag : Element Tag Tag -> Element
;; Change tag of any element named t1 to t2 in element
(check-expect (item-retag P-HI "p" "i") I-HI)
(check-expect (item-retag B-HI "p" "i") B-HI)
(check-expect (item-retag IP-HI "p" "i") II-HI)
(define (elem-retag e t1 t2)
  (make-elem (if (string=? t1 (elem-tag e)) t2 (elem-tag e))
             (content-retag (elem-content e) t1 t2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7: content-max-nesting

;; content-max-nesting : Content -> Number
;; Compute the max nesting level of elements in the content
(check-expect (content-max-nesting '()) 0)
(check-expect (content-max-nesting (list "hi")) 0)
(check-expect (content-max-nesting (list P-HI P-HI)) 1)
(check-expect (content-max-nesting (list IP-HI)) 2)
(define (content-max-nesting c)
  ;; Alt w/abs:
  #;(foldr (λ (i n) (max (item-max-nesting i) n)) 0 c)
  (cond [(empty? c) 0]
        [(cons? c)
         (max (item-max-nesting (first c))
              (content-max-nesting (rest c)))]))

;; item-max-nesting : Item -> Number
;; Compute the max nesting level of elements in the item
(check-expect (item-max-nesting "hi") 0)
(check-expect (item-max-nesting P-HI) 1)
(check-expect (item-max-nesting IP-HI) 2)
(define (item-max-nesting i)
  (cond [(string? i) 0]
        [(elem? i) (elem-max-nesting i)]))

;; elem-max-nesting : Element -> Number
;; Compute the max nesting level of elements in the element
(check-expect (elem-max-nesting P-HI) 1)
(check-expect (elem-max-nesting IP-HI) 2)
(define (elem-max-nesting e)
  (add1 (content-max-nesting (elem-content e))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8: content-text

;; content-text : Content -> [Listof String]
;; Produce a list of all text items in the content
(check-expect (content-text '()) '())
(check-expect (content-text (list "hi")) (list "hi"))
(check-expect (content-text (list P-HI)) (list "hi"))
(define (content-text c)
  ;; Alt w/abs:
  #;(foldr (λ (i los) (append (item-text i) los)) '() c)
  (cond [(empty? c) '()]
        [(cons? c)
         (append (item-text (first c))
                 (content-text (rest c)))]))

;; item-text : Item -> [Listof String]
;; Produce a list of all text items in the item
(check-expect (item-text "hi") (list "hi"))
(check-expect (item-text P-HI) (list "hi"))
(define (item-text i)
  (cond [(string? i) (list i)]
        [(elem? i) (elem-text i)]))

;; elem-text : Element -> [Listof String]
;; Produce a list of all text items in the text
(check-expect (elem-text P-HI) (list "hi"))
(define (elem-text e)
  (content-text (elem-content e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9: content-skeleton

;; content-skeleton : Content -> Content
;; Produce a content like the given one with all text removed
(check-expect (content-skeleton '()) '())
(check-expect (content-skeleton (list P-HI)) (list (make-elem "p" '())))
(check-expect (content-skeleton (list P-HI)) (list (make-elem "p" '())))
(define (content-skeleton c)
  ;; Alt w/abs
  #;(foldr (λ (i c) (append (item-skeletons i) c)) '() c)
  (cond [(empty? c) '()]
        [(cons? c)
         (append (item-skeletons (first c))
                 (content-skeleton (rest c)))]))

;; item-skeletons : Item -> [List Element]
;; Remove text from item producing either '() or a list with one element
(check-expect (item-skeletons "hi") '())
(check-expect (item-skeletons P-HI) (list (make-elem "p" '())))
(define (item-skeletons i)
  (cond [(string? i) '()]
        [(elem? i) (list (elem-skeleton i))]))

;; elem-skeleton : Element -> Element
;; Produce an element like the given one with all text removed
(check-expect (elem-skeleton P-HI) (make-elem "p" '()))
(define (elem-skeleton e)
  (make-elem (elem-tag e) (content-skeleton (elem-content e))))


(check-expect (content-skeleton eg1)
              (list
               (make-elem "h1" '())
               (make-elem "p" (list (make-elem "b" '()) (make-elem "tt" '())))
               (make-elem "p" (list (make-elem "i" '()) (make-elem "b" '())))))

