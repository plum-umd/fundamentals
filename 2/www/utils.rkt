#lang racket
(provide internal lecture-title assn-title lab-title
         hidden
         if-internal
         indented indented-span
         scheme-from-file
         class-system-filename
         class-system-url
         dot
         labref labref* lablink
         assnref assnref* assnlink
         lecref lecref* leclink
         lecture-files
         lab-files
         assn-files
         bsl-block isl-block
         ;java-block
         defn-tt
         bsl isl
         ;java
         incercise
         exercise
         styled-verbatim
         ascii-art
         (rename-out [racket r]))
(require scribble/base scribble/core scribble/html-properties scriblib/render-cond)


(define (gen-title title type num . contents)
  (apply title #:style 'unnumbered #:tag "" #:tag-prefix (format "~a~a" type num)
         (list 
          (make-element (format "~aNum" (string-titlecase type))
           (format "~a ~a:" (string-titlecase type) num)) " " contents)))
(define (lecture-title num . title-contents)
  (gen-title title "lecture" num title-contents))
(define (assn-title num . title-contents)
  (gen-title title "assignment" num title-contents))
(define (lab-title num . title-contents)
  (gen-title title "lab" num title-contents))

(define exercise-body-style
  (make-style "ExerciseBody" null))
(define incercise-body-style
  (make-style "IncerciseBody" null))

(define incercise-style
  (make-style "Incercise" null))

(define exercise-style
  (make-style "Exercise" null))

(define (defn-tt . t)
  (emph (tt t)))

(define (incercise . t) 
  (nested #:style incercise-body-style 
          (para #:style incercise-style "Do Now!")
          t))

(define (exercise . t) 
  (nested #:style exercise-body-style 
          (para #:style exercise-style "Exercise")
          t))

;(define (secref s #:underline? [u? #t] #:doc [doc #f] #:tag-prefixes [prefix #f])
;  (make-link-element (if u? #f "plainlink") null (make-section-tag s #:doc doc #:tag-prefixes prefix)))

(define (class-item-ref item name)
  (let* ([title-item (string-titlecase item)]
         [title-item-name (string-append title-item " " name)]
         [le (secref #:tag-prefixes (list (string-append item name)) "")])
    (make-delayed-element
     (lambda (renderer sec ri)
       (let ([dest (resolve-get sec ri (link-element-tag le))])
         (if dest       
             le
             title-item-name
             )))
     (lambda () title-item-name)
     (lambda () title-item-name))))
(define (class-item-ref* item name [link-text #f])
  (cond-element
   ; [latex (class-item-ref item name)] ; this works out weirdly...
   [else
    (let* ([title-item (string-titlecase item)]
           [title-item-name (or link-text (string-append title-item " " name))]
           [le (secref #:tag-prefixes (list (string-append item name)) "")
               #;(make-link-element 
                  #f title-item-name
                  (make-section-tag "" #:tag-prefixes (list (string-append item name))))])
      (make-delayed-element
       (lambda (renderer sec ri)
         (let ([dest (resolve-get sec ri (link-element-tag le))])
           (if dest       
               (make-link-element (element-style le) title-item-name (link-element-tag le))
               title-item-name
               )))
       (lambda () title-item-name)
       (lambda () title-item-name)))]))
  

(define (labref name)
  (class-item-ref "lab" name))
(define (labref* name)
  (class-item-ref* "lab" name))
(define (lablink name text)
  (class-item-ref* "lab" name text))
(define (assnref name)
  (class-item-ref "assignment" name))
(define (assnref* name)
  (class-item-ref* "assignment" name))
(define (assnlink name text)
  (class-item-ref* "assignment" name text))
(define (lecref name)
  (class-item-ref "lecture" name))
(define (lecref* name)
  (class-item-ref* "lecture" name))
(define (leclink name text)
  (class-item-ref* "lecture" name text))

(define-syntax (hidden stx)
  (syntax-case stx ()
    [(hidden . forms)
     #'(list)]))

(define (my-codeblock lang args)
  (codeblock #:keep-lang-line? #f #:context #f (string-append "#lang " lang "\n") (apply string-append (flatten args))))
(define (my-code lang args)
  (code #:lang lang (apply string-append (flatten args))))

(define (bsl-block arg . args)
  (my-codeblock "htdp/bsl" (cons arg args)))
(define (bsl arg . args)
  (my-code "htdp/bsl" (cons arg args)))
(define (isl-block arg . args)
  (my-codeblock "htdp/isl+" (cons arg args)))
(define (isl arg . args)
  (my-code "htdp/isl+" (cons arg args)))
#;
(define (java-block arg . args)
  (nested #:style (div-style "JavaHighlightBlock") 
          (my-codeblock "java" (cons arg args))))
#;
(define (java arg . args)
  ;(cond-element
   ;[latex (make-element (make-style #f '(escape))
   ;                     (list "\\texorpdfstring{\\lstinline[language=Java]¡" args "¡}{" args "}"))]
   ;[else
    (make-element (make-style "RktBlk" '(tt-chars))
                  (make-element (make-style "JavaHighlight" '(tt-chars)) 
                                (my-code "java" (cons arg args))))
  ;])
)

(define (indented-span . args)
  (make-element (make-style "InsetSpan" null) args))


(define-syntax (internal stx)
  (syntax-case stx ()
    [(internal . forms)
     (if (getenv "INTERNAL")
         #'(begin . forms)
         #'(begin))]))

(define-syntax (if-internal stx)
  (syntax-case stx ()
    [(if-internal e1 e2)
     (if (getenv "INTERNAL")
         #'e1
         #'e2)])) 

(define (indented . args)
  (apply nested #:style 'inset args))

(define related-files
  (make-style "RelatedFiles" (list 'command (make-alt-tag "div"))))
(define (starter-files type files)
   (nested #:style related-files 
           (if (null? files) 
               (nested #:style 'inset (para #:style "boxed" "There are " (emph "no") " related files for " type))
               (nested #:style 'inset 
                       (para #:style "boxed"
                             "Related files:" 
                             (linebreak)
                             (map (λ(f) 
                                    (let ([filename (path-element->string (last (explode-path f)))])
                                      (list (hspace 2) (link (string-append "files/" filename) filename) (hspace 2) " "))) files))))))
(define (lecture-files . files)
  (starter-files "lecture" files))
(define (lab-files . files)
  (starter-files "lab" files))
(define (assn-files . files)
  (starter-files "assignment" files))


(require (for-syntax racket syntax/strip-context))


(define-for-syntax (check-tab l filename)
  (when (regexp-match #rx"[\t]" l)
    (error 'from-file "file ~s contains tab characters" filename)))

(define-for-syntax (extract-from-file filename start end)
  (parameterize ([read-accept-reader #t])
    (call-with-input-file filename
      (λ (port)
        (let loop ([in? #f])
          (let ([l (read-line port 'any)])
            (cond
              [(eof-object? l)
               '()]
              [(and (not in?) 
                    (regexp-match start l)
                    (regexp-match end l))
               (list l "\n")]
              [(and (not in?) (regexp-match start l))
               (check-tab l filename)
               (list* l "\n" (loop #t))]
              [(and in? (regexp-match end l))
               (check-tab l filename)
               (list l "\n")]
              [in?
               (check-tab l filename)
               (list* l "\n" (loop #t))]
              [else
               (loop #f)])))))))

(require scribble/manual)

(define-syntax (scheme-from-file stx)
  (syntax-case stx ()
    [(_ fname #:start start #:end end)
     (let* ([l (extract-from-file (syntax-e #'fname) 
                                  (syntax-e #'start)
                                  (if (eq? (syntax-e #'end) 'same)
                                      (syntax-e #'start)
                                      (syntax-e #'end)))])
       #`(codeblock #:indent 0 #,(datum->syntax stx (car l)) #,@(cdr l)))]))

#;

(define-syntax (scheme-from-file stx)
  (syntax-case stx ()
    [(_ fname #:start start #:end end SCH)
     (let* ([l (extract-from-file (syntax-e #'fname) 
                                  (syntax-e #'start)
                                  (if (eq? (syntax-e #'end) 'same)
                                      (syntax-e #'start)
                                      (syntax-e #'end)))]
            [s (if (eq? (syntax-e #'end) 'same)
                   (string-append (apply string-append l)
                                  "\ncode:blank")
                   (apply string-append l))]
            [p (open-input-string s)]
            [os 
             (with-handlers ((exn:fail:read?
                              (λ (x)
                                (fprintf
                                 (current-error-port)
                                 "error reading from ~s, start ~s end ~s\n ~a"
                                 (syntax-e #'fname)
                                 (syntax-e #'start)
                                 (syntax-e #'end)
                                 (exn-message x)))))
               (port-count-lines! p)
               (let loop ([x null])
                 (let ([v (read-syntax (syntax-e #'fname) p)])
                   (if (eof-object? v)
                       (reverse x)
                       (loop (cons (replace-context #'fname v)
                                   x))))))])
       #`(begin (SCH . #,os)
                #;
                (make-styled-paragraph '(".1in") "vspace*")))]
    [(_ fname #:start start #:end end)
     #'(scheme-from-file fname #:start start #:end end schemeblock0)]
    ))

(define-syntax (class-system-filename stx)
  (datum->syntax #'here (getenv "CLASS")))

(define class-system-url
  (format "http://www.ccs.neu.edu/home/vkp/Teaching/2510-fl12~a" class-system-filename))

(define dot
  (racketidfont "."))


(define (styled-verbatim #:indent [i 0] #:style [style plain] s . more)
  (make-nested-flow (convert-block-style style) (list (apply verbatim #:indent i s more))))
(define (convert-block-style style)
  (cond
    [(style? style) style]
    [(or (string? style) (symbol? style)) (make-style style null)]
    [else plain]))

(define (ascii-art #:indent [i 0] #:style [style #f] . contents)
  (let* 
      ([ascii-style (λ(name other-props) (make-style name (append (list 'escape (make-alt-tag "pre")) other-props)))]
       [new-style (cond
                     [(style? style) (ascii-style (style-name style) (style-properties style))]
                     [(or (string? style) (symbol? style)) (ascii-style style '())]
                     [else (ascii-style "AsciiArt" '())])])
    (make-paragraph new-style (string-join contents ""))))


(define (pre-style name)
  (make-style name (list (make-alt-tag "pre"))))
(define (div-style name)
  (make-style name (list (make-alt-tag "div"))))
(define (span-style name)
  (make-style name (list (make-alt-tag "span"))))
(define (pyret-block . body)
  (nested #:style (pre-style "PyretHighlight") (apply literal body)))
(define (pyret . body)
  ; (define body-nonbreaking-hyphens (map (lambda (s) (string-replace s "-" -~-)) body))
  (elem #:style (span-style "PyretHighlight") (apply tt body)))
