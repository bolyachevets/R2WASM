#lang plai
(print-only-errors)

;; EBNF
;;
;; <R2WASM> :: <id>
;;         | (+ <R2WASM> <R2WASM>)
;;         | (func (<id>+) <R2WASM>)
;; <id> can be any symbols except + and define

(define-type WASM
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WASM?) (rhs WASM?)]
  [func (signature (listof id?)) (body WASM?)])

;; -----------------------------------------------------------------------------------------------
;; TODO: need to potentially fix this and incorporate into interp
;; e.g., can use this to check that signature covers all parameters in the body of a function
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
     (func (if (list? signature)
               (map parse signature)
               (list (parse signature)))
       (parse body))]
    [(list '+ lhs rhs) (add (parse lhs) (parse rhs))]
    [_ (error 'parse "Something went wrong in the parser!")]))

;; basic type tests
(test (parse '0) (num 0))
(test (parse 'fib) (id 'fib))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))

;; func test
(test (parse '(define (identity x) x)) (func (list (id 'identity) (id 'x)) (id 'x) ))
(test (parse '(define (adder x y) (+ x y))) (func (list (id 'adder) (id 'x) (id 'y)) (add (id 'x) (id 'y))))
(test (parse '(define (adder x) (+ x 1))) (func (list (id 'adder) (id 'x)) (add (id 'x) (num 1))))
(test (parse '(define x 1)) (func (list (id 'x)) (num 1)))

;; exception tests
(test/exn (parse '(define)) "")
(test/exn (parse '(define (+ x y))) "")

;; this should give an error in the interpreter
(test (parse '(define (x) (+ x y))) (func (list (id 'x)) (add (id 'x) (id 'y))))

(define (interp expr)
  (local [(define interp (lambda args (error "YOU ARE BAD! Don't call interp. Call helper.")))
          (define ($string s) (string->symbol (string-append "$" (if (number? s)
                                                                     (number->string s)
                                                                     s))))         
          (define (helper expr stack-pos)
            (type-case WASM expr
              ;; handle parameters
              [id (i)
                    ;; for now we only take parameters of type i32
                    `(param ,($string stack-pos) i32)]
              ;; handle function
              [func (signature body)
                    (local [(define func-name (symbol->string (id-name (first signature))))
                            (define params-num (length (rest signature)))
                            (define indexed-params (map list (rest signature) (range 0 params-num)))
                            ; deal with params in signature, where parameters index in arg list equals to its position on stack                           
                            (define params-lst (map (λ(x) (helper (first x) (first (rest x)))) indexed-params))
                            (define export-body (list 'export func-name (list 'func ($string func-name))))
                            (define func-body (append (list 'func)
                                                      (list ($string func-name))
                                                      params-lst
                                                      ; hardcoded return type
                                                      (list '(result i32))
                                                      ; interp body
                                                      (list (helper-body body (- params-num 1)))))]
                    (list 'module export-body func-body))]
              [else (error "We allow only functions to be transpiled into WASM text format")]))

          (define (helper-body expr stack-pos)
            (type-case WASM expr
              ;; for now we only hande addition, but this can be extended
              [add (lhs rhs)
                   `(i32.add
                      ,(helper-body lhs stack-pos)
                      ,(helper-body rhs (- stack-pos 1)))]
              [num (v)
                  `(i32.const ,v)]
              ;; handle identifiers in the body of the function differently than parameters
              [id (i)
                  ;; need to decrement stack-pos when popping the params from stack
                   `(get_local ,($string stack-pos))]
              [else (error "Illegal expression in the body of the function")]))]
    (helper expr 0)))

;; parser component tests

(test (interp (parse 'x))
      '(param $0 i32))

(test (interp (parse '(define (identity x) x)))
      '(module
           (export "identity" (func $identity))
         (func $identity (param $0 i32) (result i32)
               (get_local $0))))

(test (interp (parse '(define (adder x y) (+ x y))))
      '(module
          (export "adder" (func $adder))
        (func $adder (param $0 i32) (param $1 i32) (result i32)
              (i32.add
               (get_local $1)
               (get_local $0)))))

(test (interp (parse '(define (add1 x) (+ x 1))))
      '(module
           (export "add1" (func $add1))
         (func $add1 (param $0 i32) (result i32)
               (i32.add
               (get_local $0)
               (i32.const 1)))))

(test (interp (parse '(define c-fn 1)))
      '(module
           (export "c-fn" (func $c-fn))
         (func $c-fn (result i32)
               (i32.const 1))))
