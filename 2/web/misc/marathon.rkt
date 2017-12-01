#lang class/1

;; Parametric interfaces
;; ---------------------

;; An [Listof X] implements 
;; - all : [Question X] -> Boolean
;; - filter : [Question X] -> [Listof X]

;; A [NEListof X] implements
;; - mostest : [Comparison X] -> X

;; A [Question X] implements
;; - ask : X -> Boolean

;; A [Comparison X] implements
;; - compare : X X -> Boolean


;; Gender
;; ------
;; A Gender implements
;; - is-male? : -> Boolean
;; - is-female? : -> Boolean
(require "gender.rkt") ; provides m, f.


;; Runner
;; ------

;; A Runner is a (new runner% String Natural Natural Natural Gender)
;; Interp: runnner with name, age in years, bib number, 
;; time in minutes, & gender.
(define-class runner%
  (fields name age bib time gender))

;; Example runners
(define johnny (new runner% "Kelley" 97 1001 351 m))
(define bobby  (new runner% "Cheruiyot" 33 8 127 m))
(define roberta (new runner% "Gibb" 23 121 200 f))


;; [Question Runner]
;; -----------------
;; A [Question Runner] implements
;; - ask : Runner -> Boolean
;;   Ask this question of the runner.

(define-class is-old%
  ;; ask : Runner -> Boolean
  ;; Is the given runner older than 50?
  (check-expect ((new is-old%) . ask johnny) true)
  (check-expect ((new is-old%) . ask roberta) false)  
  (define (ask r)
    (> (r . age) 50)))

(define-class is-fast%
  ;; ask : Runner -> Boolean
  ;; Is the given runner fast (time less than 250)?  
  (check-expect ((new is-fast%) . ask johnny) false)
  (check-expect ((new is-fast%) . ask roberta) true)             
  (define (ask r)
    (< (r . time) 250)))

(define-class is-male%
  ;; ask : Runner -> Boolean
  ;; Is the given runner male?
  (check-expect ((new is-male%) . ask johnny) true)
  (check-expect ((new is-male%) . ask roberta) false)
  (define (ask r)
    (r . gender . is-male?)))

(define-class is-female%
  ;; ask : Runner -> Boolean
  ;; Is the given runner female?
  (check-expect ((new is-female%) . ask johnny) false)
  (check-expect ((new is-female%) . ask roberta) true)
  (define (ask r)
    (r . gender . is-female?)))

  
;; [Comparison Runner]
;; -------------------
;; A [Comparison Runner] implements
;; - compare : Runner Runner -> Boolean
;;   Does the first runner compare better than the second?
 
(define-class is-younger%
  ;; compare : Runner Runner -> Boolean
  ;; Is r1 younger than r2?
  (check-expect ((new is-younger%) . compare johnny roberta) false)
  (check-expect ((new is-younger%) . compare roberta johnny) true)
  (define (compare r1 r2)
    (< (r1 . age) (r2 . age))))

(define-class is-older%
  ;; compare : Runner Runner -> Boolean
  ;; Is r1 older than r2?
  (check-expect ((new is-older%) . compare johnny roberta) true)
  (check-expect ((new is-older%) . compare roberta johnny) false)
  (define (compare r1 r2)
    (> (r1 . age) (r2 . age))))

(define-class is-faster%
  ;; compare : Runner Runner -> Boolean
  ;; Is r1 faster than r2?
  (check-expect ((new is-faster%) . compare johnny roberta) false)
  (check-expect ((new is-faster%) . compare roberta johnny) true)
  (define (compare r1 r2)
    (< (r1 . time) (r2 . time))))
  
(define-class is-slower%
  ;; compare : Runner Runner -> Boolean
  ;; Is r1 slower than r2?
  (check-expect ((new is-slower%) . compare johnny roberta) true)
  (check-expect ((new is-slower%) . compare roberta johnny) false)
  (define (compare r1 r2)
    (> (r1 . time) (r2 . time))))

(define-class has-longer-name%
  ;; compare : Runner Runner -> Boolean
  ;; Does r1 have a longer name than r2?
  (check-expect ((new has-longer-name%) . compare johnny roberta) true)
  (check-expect ((new has-longer-name%) . compare roberta johnny) false)
  (define (compare r1 r2)
    (> (string-length (r1 . name))
       (string-length (r2 . name)))))


;; [Listof Runner]
;; ---------------

;; A [Listof Runner] implements
;; - fastest-old : -> Runner
;;   Get fastest runner older than 50 in this list.
;; - fastest-male : -> MaybeRunner
;;   Get fastest male runner in this list.
;; - all-old? : -> Boolean
;;   Are all the runners in this list over 50?
;; - all-fast? : -> Boolean
;;   Are all the runners in this list faster than 250?
;; - males : -> LoR
;;   Get list of male runners in this list.
;; - females : -> LoR
;;   Get list of female runners in this list.
;; - sort-asc-name : -> LoR
;;   Get list of runners sorted in ascending order by name.
;; - sort-asc-time : -> LoR
;;   Get list of runners sorted in ascending order by time.

;; A [NEListof Runner] implements
;; - fastest : -> Runner
;;   Get fastest runner in this list.
;; - slowest : -> Runner
;;   Get slowest runner in this list.
;; - youngest : -> Runner
;;   Get youngest runner in this list.
;; - oldest : -> Runner
;;   Get oldest runner in this list.
;; - mostest : [Comparison Runner] -> Runner

;; Abstract class for [Listof X].
(define-class list%
  
  ;; Only defined for [Listof Runner].  
  (define (all-old?)
    (this . all (new is-old%)))
  
  (define (all-fast?)
    (this . all (new is-fast%)))
  
  (define (males)
    (this . filter (new is-male%)))
  
  (define (females)
    (this . filter (new is-female%))))

(define-class mt% ; implements [Listof X]
  (super list%)
  
  ;; all : [Question X] -> Boolean
  (define (all q) true)
  
  ;; filter : [Question X] -> [Listof X]
  (define (filter q) this)
  
  ;; mostest/acc : X [Comparison X] -> X
  (define (mostest/acc x rc)
    x))

(define-class cons% ; implements [Listof X], [NEListof X]
  (super list%)
  (fields first rest)
  
  ;; An alternative definition of all that uses functions
  ;; instead of function objects.
  ;; all-f : [Runner -> Boolean] -> Boolean  
  #;
  (define (all-f p)
    (and (p (this . first))
         (this . rest . all-f p)))              
  
  ;; all : [Question X] -> Boolean
  (define (all q)
    (and (q . ask (this . first))
         (this . rest . all q)))
  
  ;; filter : [Question X] -> [Listof X]
  (define (filter q)
    (cond [(q . ask (this . first))
           (new cons% (this . first) (this . rest . filter q))]
          [else
           (this . rest . filter q)]))
  
  ;; mostest : [Comparison X] -> X
  (define (mostest rc)
    (this . rest . mostest/acc (this . first) rc))
  
  ;; mostest/acc : X [Comparison X] -> X
  (define (mostest/acc x c)
    (this . rest . mostest/acc 
          (cond [(c . compare x (this . first)) x]
                [else (this . first)])
          c))
  
  ;; Only defined when this is a [Listof Runner]
  ;; youngest : -> Runner
  (define (youngest)    
    (this . mostest (new is-younger%)))
  
  ;; oldest : -> Runner
  (define (oldest)
    (this . mostest (new is-older%)))
  
  ;; fastest : -> Runner
  (define (fastest)
    (this . mostest (new is-faster%)))
  
  ;; slowest : -> Runner
  (define (slowest)
    (this . mostest (new is-slower%))))
  
 

;; Example [Listof Runner]
(define rs
  (new cons% johnny (new cons% bobby (new cons% roberta (new mt%)))))
(define mt
  (new mt%))

(check-expect (rs . fastest) bobby)
(check-expect (rs . slowest) johnny)
(check-expect (rs . youngest) roberta)
(check-expect (rs . oldest) johnny)
#|
(check-expect (rs . fastest-old) johnny)
(check-expect (rs . fastest-male) bobby)
|#
(check-expect ((new mt%) . all-old?) true)
(check-expect ((new mt%) . all-fast?) true)
(check-expect (rs . all-old?) false)
(check-expect (rs . all-fast?) false)
(check-expect ((new cons% johnny (new mt%)) . all-old?) true)
(check-expect ((new cons% roberta (new mt%)) . all-fast?) true)
(check-expect (rs . males)
              (new cons% johnny (new cons% bobby (new mt%))))
(check-expect (rs . females)
              (new cons% roberta (new mt%)))
#|
(check-expect (rs . sort-asc-name)
              (new cons% bobby (new cons% roberta (new cons% johnny mt))))
(check-expect (rs . sort-asc-time)
              (new cons% bobby (new cons% roberta (new cons% johnny mt))))
|#              

