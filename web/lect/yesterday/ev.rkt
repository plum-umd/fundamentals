#lang racket
(struct lam (x e)   #:transparent)
(struct app (e0 e1) #:transparent)
(struct lit (l)     #:transparent)
(struct vbl (x)     #:transparent)

(define (ev e ρ)
  (match e
    [(lam x e)   (λ (v) (ev e (extend ρ x v)))]
    [(lit l)     l]
    [(app e0 e1) ((ev e0 ρ) (ev e1 ρ))]
    [(vbl x)     (lookup ρ x)]))

(define (co e)
  (match e
    [(lam x e)
     (define c (co e))
     (λ (ρ) (λ (v) (c (extend ρ x v))))]
    [(lit l)
     (λ (ρ) l)]
    [(app e0 e1)
     (define c0 (co e0))
     (define c1 (co e1))
     (λ (ρ) ((c0 ρ) (c1 ρ)))]
    [(vbl x)
     (λ (ρ) (lookup ρ x))]))
     
    

(define mt-env (hash))
(define (lookup ρ x)
  (hash-ref ρ x))
(define (extend ρ x v)
  (hash-set ρ x v))

(ev (app (lam 'x (app (lit add1) (vbl 'x)))
         (lit 50))
    mt-env)


