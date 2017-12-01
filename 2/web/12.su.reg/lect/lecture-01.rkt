;; *****************************
;; *  CS 2510 Lecture 1 Notes  *
;; *  Review and Accumulators  *
;; *****************************

;;> Data definitions...

;; A [Listof Number] is one of:
;;  - empty
;;  - (cons Number [Listof Number])

;;> Simple recursive function...

;; sum-list : [Listof Number] -> Number
;; Add all the numbers in the list
(define (sum lon)
  (cond [(empty? lon) 0]
        [else (+ (first lon)
                 ;; The sum of the rest of the numbers
                 (sum (rest lon)))]))

(check-expect (sum empty) 0)
(check-expect (sum (list 1)) 1)
(check-expect (sum (list 1 2 3)) 6)


;;> Accumulator Style Sum

;; sum-acc : [Listof Number] -> Number
;; Add all the numbers in the list using accumulator style
(define (sum-acc lon)
  (local [;; help : Number [Listof Number] -> Number
          ;; The ACC is the sum of the numbers seen so far.
          (define (help acc lon)
            (cond [(empty? lon) acc]
                  [else (help (+ acc (first lon))
                              (rest lon))]))]
    (help 0 lon)))

(check-expect (sum-acc empty) 0)
(check-expect (sum-acc (list 1)) 1)
(check-expect (sum-acc (list 1 5 6)) 12)


;;> More complex use of accumulators
;;> Problem: Given a list of points (Posns), compute the
;;>          total length of a "trip" from the first point
;;>          to the last point in the list.

;;> Helper
;; dist : Posn Posn -> Number
;; Compute the straight-line distance between two points
(define (dist p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

(check-expect (dist (make-posn 0 0) (make-posn 0 0)) 0)
(check-expect (dist (make-posn 0 0) (make-posn 3 4)) 5)
(check-expect (dist (make-posn 0 0) (make-posn 0 5)) 5)
  
;;> The top-level function...

;; trip : [Listof Posn] -> Number
;; Find the total length of a trip over the list of posns

;; (define (trip lop)
;;   (cond [(empty? lop) 0]
;;         [(empty? (rest lop)) 0]
;;         [else (+ (dist (first lop) (first (rest lop)))
;;                  (trip (rest lop)))]))
;; 

(define (trip lop)
  (cond [(empty? lop) 0]
        [else (trip-acc 0 (first lop) (rest lop))]))

(check-expect (trip empty) 0)
(check-expect (trip (list (make-posn 0 0) (make-posn 3 4))) 5)
(check-expect (trip (list (make-posn 0 0) (make-posn 0 5))) 5)
(check-expect (trip (list (make-posn 0 0) (make-posn 3 4) (make-posn 5 4))) 7)


;;> The helper with two accumulors

;; trip-acc : Number Posn [Listof Posn] -> Number
;; The total trip, after travelling D, from P, over the list
;; The d-ACC is the Distance travelled so far, the p-ACC is the
;;   Last point visited, i.e., our current position.
(define (trip-acc d p lop)
  (cond [(empty? lop) d]
        [else (trip-acc
               ;; Update the first ACC with the distance from the
               ;;   current point to the next one
               (+ d (dist p (first lop)))
               ;; This point becomes the last visited point
               (first lop)
               ;; Rest of the list, places to go still
               (rest lop))]))

(check-expect (trip-acc 0 (make-posn 0 0) (list (make-posn 3 4))) 5)
(check-expect (trip-acc 45 (make-posn 0 0) (list (make-posn 3 4))) 50)
