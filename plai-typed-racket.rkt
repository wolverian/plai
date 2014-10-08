#lang typed/racket

(require typed/rackunit)

(define-syntax deftype
  (syntax-rules ()
    [(_ name [cons-name (field-name : field-type) ...] ...)
     (begin
       (struct cons-name ([field-name : field-type] ...) #:transparent) ...
       (define-type name (U cons-name ...)))]))

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

(define-type Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)

(: interp (expr-c Env (Listof fun-def-c) -> Number))
(define (interp expr env fds)
  (match expr
    [(num-c n) n]
    [(plus-c l r) (+ (interp l env fds) (interp r env fds))]
    [(mult-c l r) (* (interp l env fds) (interp r env fds))]
    [(app-c f a) (let ([fd : fun-def-c (get-fundef f fds)])
                       (interp (fd-c-body fd)
                               (extend-env (bind (fd-c-arg fd)
                                                 (interp a env fds))
                                           mt-env)
                               fds))]
    [(id-c n) (lookup n env)]))

(: lookup (Symbol Env -> Number))
(define (lookup name env)
   (let ([binding (findf (match-lambda [(bind n _) (symbol=? n name)]) env)])
     (if binding
         (bind-val binding)
         (error 'lookup "no such binding"))))

(: get-fundef (Symbol (Listof fun-def-c) -> fun-def-c))
(define (get-fundef name fds)
  (or (findf (match-lambda [(fd-c n _ _) (symbol=? n name)]) fds)
      (error "oops")))

(check-equal? (interp (plus-c (num-c 10) (app-c 'const-5 (num-c 10)))
                      mt-env
                      (list (fd-c 'const-5 '_ (num-c 5))))
              15)

(check-equal? (interp (plus-c (num-c 10) (app-c 'double (plus-c (num-c 1) (num-c 2))))
                      mt-env
                      (list (fd-c 'double 'x (plus-c (id-c 'x) (id-c 'x)))))
              16)

(check-equal? (interp (plus-c (num-c 10) (app-c 'quadruple (plus-c (num-c 1) (num-c 2))))
                      mt-env
                      (list (fd-c 'quadruple 'x (app-c 'double (app-c 'double (id-c 'x))))
                            (fd-c 'double 'x (plus-c (id-c 'x) (id-c 'x)))))
              22)

(check-exn
 exn:fail?
 (lambda () (interp (app-c 'f1 (num-c 3))
               mt-env
               (list (fd-c 'f1 'x (app-c 'f2 (num-c 4)))
                     (fd-c 'f2 'y (plus-c (id-c 'x) (id-c 'y)))))))
