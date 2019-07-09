#lang racket

;; Download submissions, unzip, cd in to folder
;; Run

;(current-directory "Downloads/submissions (2)")

(define h (make-hash))

(define (path-user p)
  (first (string-split (path->string p) "_")))

(define (hash-join h k v)
  (hash-set! h k (set-add (hash-ref h k (set)) v)))

(for-each (λ (p)
            (hash-join h (path-user p) p))
          (directory-list))

(define l
  (shuffle
   (for/list ([(k ps) (in-hash h)])
     (set->list ps))))

(require file/zip)
(let-values ([(angela sam) (split-at l (quotient (length l) 2))])
  (values (apply zip "angela.zip" (apply append angela))
          (apply zip "sam.zip" (apply append sam))))

