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
  [lam-c (arg : Symbol) (body : Expr-C)]
  [box-c (arg : Expr-C)]
  [unbox-c (arg : Expr-C)]
  [set-box-c (b : Expr-C) (v : Expr-C)]
  [seq-c (b1 : Expr-C) (b2 : Expr-C)])

(deftype Value
  [num-v (n : Number)]
  [clos-v (arg : Symbol) (body : Expr-C) (env : Env)]
  [box-v (v : Location)])

(define-type Location Number)

(deftype Binding
  [bind (name : Symbol) (val : Location)])

(define-type Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)

(deftype Storage
  [cell (location : Location) (val : Value)])

(define-type Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

(deftype Result
  [v*s (v : Value) (s : Store)])

(: interp (Expr-C Env Store -> Result))
(define (interp expr env store)
  (match expr
    [(num-c n) (v*s (num-v n) store)]
    [(plus-c l r) (match-let* ([(v*s v-l s-l) (interp l env store)]
                              [(v*s v-r s-r) (interp r env s-l)])
                    (v*s (num+ v-l v-r) s-r))]
    [(mult-c l r) (match-let* ([(v*s v-l s-l) (interp l env store)]
                              [(v*s v-r s-r) (interp r env s-l)])
                    (v*s (num* v-l v-r) s-r))]
    [(app-c f arg-val) (match-let* ([(v*s (clos-v a b f-e) f-s) (interp f env store)]
                                    [(v*s a-v a-s) (interp arg-val env f-s)]
                                    [where (new-loc)])
                         (interp b
                                 (extend-env (bind a where) f-e)
                                 (override-store (cell where a-v) a-s)))]
    [(id-c n) (v*s (fetch (lookup n env) store) store)]
    [(lam-c arg body) (v*s (clos-v arg body env) store)]
    [(box-c a) (match-let ([(v*s v s) (interp a env store)])
                 (let ([where (new-loc)])
                   (v*s (box-v where)
                        (override-store (cell where v)
                                        s))))]
    [(unbox-c a) (match-let ([(v*s (box-v loc) s) (interp a env store)])
                   (v*s (fetch loc s) s))]
    [(set-box-c b v) (match-let* ([(v*s (box-v loc) s-b) (interp b env store)]
                                  [(v*s v-v s-v) (interp v env s-b)])
                       (v*s v-v (override-store (cell loc v-v) s-v)))]
    [(seq-c a b) (match-let ([(v*s v s) (interp a env store)])
                   (interp b env s))]))

(: new-loc (-> Location))
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(: num+ (Value Value -> Value))
(define/match (num+ a b)
  [((num-v x) (num-v y)) (num-v (+ x y))]
  [(_ _) (error 'num+ "one argument was not a number")])

(: num* (Value Value -> Value))
(define/match (num* a b)
  [((num-v x) (num-v y)) (num-v (* x y))]
  [(_ _) (error 'num* "one argument was not a number")])

(: lookup (Symbol Env -> Location))
(define (lookup name env)
   (let ([binding (findf (match-lambda [(bind n _) (symbol=? n name)]) env)])
     (if binding
         (bind-val binding)
         (error 'lookup "no such binding"))))

(: fetch (Location Store -> Value))
(define (fetch loc store)
  (let ([c (findf (match-lambda [(cell l _) (= loc l)]) store)])
    (if c
        (cell-val c)
        (error 'fetch "no such location"))))

(check-equal? (interp (plus-c (num-c 10) (num-c 20))
                      mt-env
                      mt-store)
              (v*s (num-v 30) '()))

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
