#lang typed/racket

(require typed/rackunit)

;; * Common type support

;;   Unfortunately there's no `match-type` yet. PLAI has one, which
;;   requires you to say `(match TypeName expr cases)`. I'd like to do it
;;   without requiring the explicit TypeName mention.
(define-syntax-rule (deftype name [cons-name (field-name : field-type) ...] ...)
  (begin
    (struct cons-name ([field-name : field-type] ...) #:transparent) ...
    (define-type name (U cons-name ...))))

;; * Surface syntax

(deftype Expr-S
  [num-s (n : Number)]
  [id-s (s : Symbol)]
  [app-s (fun : Expr-S) (arg : Expr-S)]
  [plus-s (l : Expr-S) (r : Expr-S)]
  [mult-s (l : Expr-S) (r : Expr-S)]
  [lam-s (arg : Symbol) (body : Expr-S)]
  [let-s (name : Symbol) (val : Expr-S) (body : Expr-S)]
  [seq-s (b1 : Expr-S) (b2 : Expr-S)])

;; * Parser

(: parse (Sexp -> Expr-S))
(define/match (parse expr)
  [((? number? n)) (num-s n)]
  [((? symbol? s)) (id-s s)]
  [(`(+ ,l ,r)) (plus-s (parse l) (parse r))]
  [(`(* ,l ,r)) (mult-s (parse l) (parse r))]
  [(`(lambda (,(? symbol? a)) ,b)) (lam-s a (parse b))]
  [(`(let ([,(? symbol? name) ,val]) ,b)) (let-s name (parse val) (parse b))]
  [(`(begin ,a ,b)) (seq-s (parse a) (parse b))]
  [((list f arg)) (app-s (parse f) (parse arg))])

;; ** Tests for the parser

(check-equal? (parse '42) (num-s 42))
(check-equal? (parse 'x) (id-s 'x))
(check-equal? (parse '(+ 27 12)) (plus-s (num-s 27) (num-s 12)))
(check-equal? (parse '(* 1 x)) (mult-s (num-s 1) (id-s 'x)))
(check-equal? (parse '(lambda (y) (+ y 42))) (lam-s 'y (plus-s (id-s 'y) (num-s 42))))
(check-equal? (parse '((lambda (y) y) 42)) (app-s (lam-s 'y (id-s 'y)) (num-s 42)))
(check-equal? (parse '(let ([x 42]) x)) (let-s 'x (num-s 42) (id-s 'x)))
(check-equal? (parse '(let ([f (lambda (g) (+ (g 42) 1))]) (f (lambda (x) (* x 2)))))
              (let-s 'f (lam-s 'g (plus-s (app-s (id-s 'g) (num-s 42)) (num-s 1)))
                     (app-s (id-s 'f) (lam-s 'x (mult-s (id-s 'x) (num-s 2))))))

;; * Core syntax

(deftype Expr-C
  [num-c (n : Number)]
  [id-c (s : Symbol)]
  [app-c (fun : Expr-C) (arg : Expr-C)]
  [plus-c (l : Expr-C) (r : Expr-C)]
  [mult-c (l : Expr-C) (r : Expr-C)]
  [lam-c (arg : Symbol) (body : Expr-C)]
  [set-c (var : Symbol) (arg : Expr-C)]
  [seq-c (b1 : Expr-C) (b2 : Expr-C)])

;; * Desugarer

(: desugar (Expr-S -> Expr-C))
(define/match (desugar expr)
  [((num-s n)) (num-c n)]
  [((id-s s)) (id-c s)]
  [((app-s f a)) (app-c (desugar f) (desugar a))]
  [((plus-s l r)) (plus-c (desugar l) (desugar r))]
  [((mult-s l r)) (mult-c (desugar l) (desugar r))]
  [((lam-s a b)) (lam-c a (desugar b))]
  [((let-s n v b)) (app-c (lam-c n (desugar b)) (desugar v))]
  [((seq-s a b)) (seq-c (desugar a) (desugar b))])

;; ** Tests for desugaring

(check-equal? (desugar (num-s 42)) (num-c 42))
(check-equal? (desugar (id-s 'foo)) (id-c 'foo))
(check-equal? (desugar (app-s (id-s 'f) (num-s 42)))
              (app-c (id-c 'f) (num-c 42)))
(check-equal? (desugar (plus-s (num-s 42) (num-s 1)))
                       (plus-c (num-c 42) (num-c 1)))
(check-equal? (desugar (lam-s 'x (plus-s (id-s 'x) (num-s 42))))
              (lam-c 'x (plus-c (id-c 'x) (num-c 42))))
(check-equal? (desugar (let-s 'x (let-s 'y (num-s 42) (plus-s (id-s 'y) (num-s 1)))
                              (plus-s (id-s 'x) (num-s 2))))
              (app-c (lam-c 'x (plus-c (id-c 'x) (num-c 2)))
                     (app-c (lam-c 'y (plus-c (id-c 'y) (num-c 1)))
                            (num-c 42))))
(check-equal? (desugar (seq-s (app-s (id-s 'x) (num-s 1))
                              (app-s (id-s 'y) (num-s 2))))
              (seq-c (app-c (id-c 'x) (num-c 1))
                     (app-c (id-c 'y) (num-c 2))))

;; * Interpreter types

;; ** Values

(deftype Value
  [num-v (n : Number)]
  [clos-v (arg : Symbol) (body : Expr-C) (env : Env)])

;; ** Environments

(define-type Location Number)

(deftype Binding
  [bind (name : Symbol) (val : Location)])

(define-type Env (Listof Binding))
(define mt-env empty)
(define extend-env cons)

;; ** Mutable storage

(deftype Storage
  [cell (location : Location) (val : Value)])

(define-type Store (Listof Storage))
(define mt-store empty)
(define override-store cons)

;; ** Results

(deftype Result
  [v*s (v : Value) (s : Store)])


;; * Interpreter

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
    [(set-c var val) (match-let ([(v*s v-val s-val) (interp val env store)]
                                 [where (lookup var env)])
                       (v*s v-val (override-store (cell where v-val) store)))]
    [(seq-c a b) (match-let ([(v*s v s) (interp a env store)])
                   (interp b env s))]))

;; * Interpreter support
;; ** Numerics

(: num+ (Value Value -> Value))
(define/match (num+ a b)
  [((num-v x) (num-v y)) (num-v (+ x y))]
  [(_ _) (error 'num+ "one argument was not a number")])

(: num* (Value Value -> Value))
(define/match (num* a b)
  [((num-v x) (num-v y)) (num-v (* x y))]
  [(_ _) (error 'num* "one argument was not a number")])

;; ** Support for mutable values

(: new-loc (-> Location))
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

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

;; ** Test support

(define-syntax-rule (check-interp? prog expected)
  (check-equal? (v*s-v (interp (desugar (parse 'prog)) mt-env mt-store)) expected))

(define-syntax over
  (syntax-rules ()
    [(_ target (s ...)) (target s ...)]
    [(_ target s) (target s)]
    [(_ target s ss ...) (begin
                           (over target s)
                           (over target ss ...))]))

;; ** Interpreter tests

(check-interp? (+ 1 2) (num-v 3))

(over check-interp?
      [(+ 1 2) (num-v 3)]
      [(* 1 2) (num-v 2)]
      [(let ([x 42]) x) (num-v 42)]
      [(let ([f (lambda (g) (g 42))]) (f (lambda (x) (+ x 41)))) (num-v 83)]
      [(let ([x (let ([x 42]) (+ x x))]) x) (num-v 84)]
      [((lambda (x) (let ([x 42]) x)) 666) (num-v 42)])
