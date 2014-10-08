#lang typed/racket

(define-syntax deftype
  (syntax-rules ()
    [(_ name [cons-name (field-name : field-type) ...] ...)
     (begin
       (struct cons-name ([field-name : field-type] ...) #:transparent) ...
       (define-type name (U cons-name ...)))]))

;; (deftype arith-s
;;   [num-s (n : Number)]
;;   [plus-s (l : arith-s) (r : arith-s)]
;;   [bminus-s (l : arith-s) (r : arith-s)]
;;   [mult-s (l : arith-s) (r : arith-s)])

;; (deftype arith-c
;;   [num-c (n : Number)]
;;   [plus-c (l : arith-c) (r : arith-c)]
;;   [mult-c (l : arith-c) (r : arith-c)])

(deftype expr-c
  [num-c (n : Number)]
  [id-c (s : Symbol)]
  [app-c (fun : Symbol) (arg : expr-c)]
  [plus-c (l : expr-c) (r : expr-c)]
  [mult-c (l : expr-c) (r : expr-c)])

(deftype fun-def-c
  (fd-c (name : Symbol) (arg : Symbol) (body : expr-c)))

(deftype Binding
  [bind (name : Symbol) (val : Number)])

(define-type Env Binding)
(define mt-env empty)
(define extend-env cons)

(: interp (expr-c (Listof fun-def-c) -> Number))
(define (interp e fds)
  (match e
    [(num-c n) n]
    [(plus-c l r) (+ (interp l fds) (interp r fds))]
    [(mult-c l r) (* (interp l fds) (interp r fds))]
    [(app-c f a) (let ([fd : fun-def-c (get-fundef f fds)])
                       (interp (subst (interp a fds)
                                      (fd-c-arg fd)
                                      (fd-c-body fd))
                               fds))]
    [(id-c s) (error 'interp "shouldn't get here")]))

(: get-fundef (Symbol (Listof fun-def-c) -> fun-def-c))
(define (get-fundef name fds)
  (or (findf (match-lambda [(fd-c n _ _) (symbol=? n name)]) fds)
      (error "oops")))

(: subst (Number Symbol expr-c -> expr-c))
(define (subst what for in)
  (define with (num-c what))
  (: go (expr-c -> expr-c))
  (define/match (go in)
    [((num-c n)) in]
    [((id-c s)) (if (symbol=? s for) with in)]
    [((app-c f a)) (app-c f (go a))]
    [((plus-c l r)) (plus-c (go l) (go r))])
  (go in))

;; (: desugar (arith-s -> arith-c))
;; (define (desugar expr)
;;   (match expr
;;     [(num-s n) (num-c n)]
;;     [(plus-s l r) (plus-c (desugar l) (desugar r))]
;;     [(mult-s l r) (mult-c (desugar l) (desugar r))]
;;     [(bminus-s l r) (plus-c (desugar l) (mult-c (num-c -1) (desugar r)))]))

;; (: eval-arith (arith-c -> Number))
;; (define (eval-arith expr)
;;   (match expr
;;     [(num-c n) n]
;;     [(plus-c l r) (+ (eval-arith l) (eval-arith r))]
;;     [(mult-c l r) (* (eval-arith l) (eval-arith r))]))

;; (: parse-arith (Sexp -> arith-c))
;; (define (parse-arith expr)
;;   (match expr
;;     [(? number? n) (num-c n)]
;;     [(list '+ l r) (plus-c (parse-arith l) (parse-arith r))]
;;     [(list '* l r) (mult-c (parse-arith l) (parse-arith r))]))

;; (eval-arith (parse-arith '(+ (* 42 2) (* 2 (+ 1 0)))))
