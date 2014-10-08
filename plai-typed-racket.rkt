#lang typed/racket

(require typed/rackunit)

(define-syntax deftype
  (syntax-rules ()
    [(_ name [cons-name (field-name : field-type) ...] ...)
     (begin
       (struct cons-name ([field-name : field-type] ...) #:transparent) ...
       (define-type name (U cons-name ...)))]))

(deftype Expr-C
  [num-c (n : Number)]
  [id-c (s : Symbol)]
  [app-c (fun : Expr-C) (arg : Expr-C)]
  [plus-c (l : Expr-C) (r : Expr-C)]
  [mult-c (l : Expr-C) (r : Expr-C)]
  [lam-c (arg : Symbol) (body : Expr-C)])

(deftype Value
  [num-v (n : Number)]
  [clos-v (arg : Symbol) (body : Expr-C) (env : Env)])

(deftype Binding
  [bind (name : Symbol) (val : Value)])

(define-type Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)

(: interp (Expr-C Env -> Value))
(define (interp expr env)
  (match expr
    [(num-c n) (num-v n)]
    [(plus-c l r) (num+ (interp l env) (interp r env))]
    [(mult-c l r) (num* (interp l env) (interp r env))]
    [(app-c f arg-val) (match-let ([(clos-v arg body f-env) (interp f env)])
                         (interp body
                                 (extend-env (bind arg (interp arg-val env)) f-env)))]
    [(id-c n) (lookup n env)]
    [(lam-c arg body) (clos-v arg body env)]))

(: num+ (Value Value -> Value))
(define/match (num+ a b)
  [((num-v x) (num-v y)) (num-v (+ x y))]
  [(_ _) (error 'num+ "one argument was not a number")])

(: num* (Value Value -> Value))
(define/match (num* a b)
  [((num-v x) (num-v y)) (num-v (* x y))]
  [(_ _) (error 'num* "one argument was not a number")])

(: lookup (Symbol Env -> Value))
(define (lookup name env)
   (let ([binding (findf (match-lambda [(bind n _) (symbol=? n name)]) env)])
     (if binding
         (bind-val binding)
         (error 'lookup "no such binding"))))


;; (define-test-suite stuff
;;   (check-equal? (interp (plus-c (num-c 10)
;;                                 (app-c (fd-c 'const5
;;                                              '_
;;                                              (num-c 5))
;;                                        (num-c 10)))
;;                         mt-env)
;;                 (num-v 15))

;;   (check-equal? (interp (plus-c (num-c 10)
;;                                 (app-c (fd-c 'double
;;                                              'x
;;                                              (plus-c (id-c 'x) (id-c 'x)))
;;                                        (plus-c (num-c 1) (num-c 2))))
;;                         mt-env)
;;                 (num-v 16))

;;   (check-equal? (interp (plus-c (num-c 10)
;;                                 (app-c (fd-c 'quadruple
;;                                              'x
;;                                              (app-c (fd-c 'double
;;                                                           'x
;;                                                           (plus-c (id-c 'x) (id-c 'x)))
;;                                                     (app-c (fd-c 'double
;;                                                                  'x
;;                                                                  (plus-c (id-c 'x) (id-c 'x)))
;;                                                            (id-c 'x))))
;;                                        (plus-c (num-c 1) (num-c 2))))
;;                         mt-env)
;;                 (num-v 22))

;;   (check-exn exn:fail?
;;              (lambda () (interp (app-c (fd-c 'f1
;;                                         'x
;;                                         (app-c (fd-c 'f2
;;                                                      'y
;;                                                      (plus-c (id-c 'x) (id-c 'y)))
;;                                                (num-c 4)))
;;                                   (num-c 3))
;;                            mt-env))))
