#lang racket


(define x (shuffle (file->lines "student-list")))

(define one (first x))
(define others (rest x))

(define pairs (map list (take others 20) 
                   (drop  others 20)))

(define (get-username s) (second (regexp-match ".*?([a-z_0-9]+)$" s)))

(define pairs*
  (cons (cons one (first pairs))
        (rest pairs)))

(define all-users (map (λ (l) (map get-username l)) pairs*))

;; the first real partnership assignment.  Use to rerun if neccessary.
(define all-users1 '()
  #|
pair 001: tlunter, dcalacci, ian_redpath
pair 002: gwong, ishaqr
pair 003: ajhorst, butlerch
pair 004: stoye, emichel
pair 005: ckohler, gchan93
pair 006: ronbrz, cclark
pair 007: jgoode, cmoss
pair 008: alec_loudenback, mlhamlin
pair 009: louella_estillero, rmacnz
pair 010: william_johnston, matthew_king
pair 011: chris11, psanshi
pair 012: ksoldau, erenn16
pair 013: 11bowerj, bsoohoo
pair 014: colemanm, rramsey
pair 015: ajacks, taldrich
pair 016: asdjkl, gloden
pair 017: blaisenu, kevrcav
pair 018: jkantor, pletchst
pair 019: mullinsk, nathaniel_hahn
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
