#lang racket/base
(provide panopto-vid)
(require scribble/manual
         scribble/core 
         scribble/html-properties)
          
;; Embed a public panopto video into page
(define (panopto-vid src)
  (elem #:style 
        (style #f (list (alt-tag "iframe") 
                        (attributes 
                          `((src . ,src)
                            (width . "720")
                            (height . "405")
                            (gesture . "media")
                            (allowfullscreen . "")
                            (style . "padding: 0px; border: 1px solid #464646;")))))))

