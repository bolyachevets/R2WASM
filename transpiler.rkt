#lang plai
(print-only-errors)

;; EBNF
;;
;; <R2WASM> :: <id>
;;         | (+ <R2WASM> <R2WASM>)
;;         | (func (<id>+) <R2WASM>)
;; <id> can be any symbols except + and func

(define-type WASM
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WASM?) (rhs WASM?)]
  [func (signature (listof id?)) (body WASM?)])

;; -----------------------------------------------------------------------------------------------
;; will need to use this construct in the parser to make sure that signature covers all parameters in the body of a function
(define-type Env
  [mtEnv]
  [anEnv (params (listof symbol?))])

;; lookup : symbol? Env? -> Bool?
;; helper that makes sure that an identifier in the body of the function has been listed in the function's signature
(define (lookup name env)
  (type-case Env env
    [mtEnv () (error 'interp "unbound identifier ~a" name)]
    [anEnv (params) (any->boolean (member name params))]))

;; any->boolean : any -> boolean
(define (any->boolean x)
  (if x
      #t
      #f))

;; lookup tests
(test (lookup 'adder (anEnv (list 'adder 'x 'y))) true)
(test (lookup 'adder (anEnv (list 'x 'y))) false)

;; -----------------------------------------------------------------------------------------------

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list 'define signature body)
     (func (map parse signature) (parse body))]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [_ (error 'parse "Something went wrong!")]))

;; basic type tests
(test (parse '0) (num 0))
(test (parse 'fib) (id 'fib))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))

;; func test
(test (parse '(define (adder x y) (+ x y))) (func (list (id 'adder) (id 'x) (id 'y)) (add (id 'x) (id 'y))))

;; exception tests
(test/exn (parse '(define)) "")
(test/exn (parse '(define (+ x y))) "")

;; this should give an error in the parser
(test (parse '(define (x) (+ x y))) (func (list (id 'x)) (add (id 'x) (id 'y))))

;; the ultimate test to reach for
#;(test (parse '(define (adder x y) (+ x y)))
      '(module
          (export "add" (func $add))
        (func $add (param $0 i32) (param $1 i32) (result i32)
              (i32.add
               (get_local $1)
               (get_local $0)
               ))))

