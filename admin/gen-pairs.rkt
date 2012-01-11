#lang racket


(define x (shuffle (file->lines "student-list")))

(define class-size (length x))
(define num-pairs (quotient class-size 2))

(define one (first x))
(define others (if (odd? class-size) 
                   (rest x)
                   x))


(define pairs (map list (take others num-pairs) 
                   (drop  others num-pairs)))

(define (get-username s) (second (regexp-match ".*?([a-z_0-9]+)$" s)))

(define pairs*
  (if (odd? class-size)
      (cons (cons one (first pairs))
        (rest pairs))
      pairs))

(define all-users (map (λ (l) (map get-username l)) pairs*))

;; the first real partnership assignment.  Use to rerun if neccessary.
(define all-users1 '()
  #|
pair 001: tlunter, dcalacci, iredpath
pair 002: gwong, kevrcav
pair 003: ajhorst, butlerch
pair 004: stoye, emichel
pair 005: ckohler, gchan93
pair 006: ronbrz, cclark
pair 007: jgoode, cmoss
pair 008: aloud, mlhamlin
pair 009: lestille, rmacnz
pair 010: wjj, kingm
pair 011: chris11, psanshi
pair 012: ksoldau, erenn16
pair 013: 11bowerj, bsoohoo
pair 014: colemanm, rramsey
pair 015: ajacks, taldrich
pair 016: asdjkl, gloden
pair 017: SKIP
pair 018: jkantor, pletchst
pair 019: mullinsk, nhahn
pair 020: mechanik, manning
|#
  )

(define (pad i)
  (define s (number->string i))
  (cond [(< i 10) (string-append "00" s)]
        [(< i 100) (string-append "0" s)]
        [else (string-append "" s)]))

(for ([members all-users]
      [i (in-naturals 1)])
  (printf "[cs2510hspring2012:/pair~a]\n" (pad i))
  (for ([m members])
    (printf "~a = rw\n" m))
  (printf "\n"))

(printf "\nfor the blog\n")

(for ([members all-users]
      [i (in-naturals 1)])
  (printf "pair ~a: " (pad i))
  (for ([m (add-between members ", ")]) (display m))
  (newline))
