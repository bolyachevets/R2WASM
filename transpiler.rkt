#lang plai
(print-only-errors)

;; EBNF
;;
;; <R2WASM> :: <num> 
;;         | <id>
;;         | (+ <R2WASM> <R2WASM>)
;;         | (- <R2WASM> <R2WASM)
;;         | (* <R2WASM> <R2WASM>)
;;         | (< <R2WASM> <R2WASM>)
;;         | (func (<id>+) <R2WASM>)
;;         | (return <R2WASM>)
;;         | (call <id> <R2WASM>+)
;;         | (if <R2WASM> <R2WASM> <R2WASM>)
;; <id> can be any symbols except +, -, <, if, return, call and define

(define-type WASM
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs WASM?) (rhs WASM?)]
  [sub (lhs WASM?) (rhs WASM?)]
  [mult (lhs WASM?) (rhs WASM?)]
  [less (lhs WASM?) (rhs WASM?)]
  [if0 (test-exp WASM?) (then-exp WASM?) (else-exp WASM?)]
  [return (exp WASM?)]
  [call (f-name id?) (param WASM?)]
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
    [(list '* lhs rhs) (mult (parse lhs) (parse rhs))]
    [(list '- lhs rhs) (sub (parse lhs) (parse rhs))]
    [(list '< lhs rhs) (less (parse lhs) (parse rhs))]
    [(list 'if c-expr t-expr e-expr)
     (if0 (parse c-expr) (parse t-expr) (parse e-expr))]
    [(list f-name sexp) (call (parse f-name) (parse sexp))]
    [_ (error 'parse "Something went wrong in the parser!")]))

;; basic type tests
(test (parse '0) (num 0))
(test (parse 'fib) (id 'fib))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 3 2)) (sub (num 3) (num 2)))
(test (parse '(if 1 2 3)) (if0 (num 1) (num 2) (num 3)))
(test (parse '(fib (+ 1 2))) (call (id 'fib) (add (num 1) (num 2))))

;; func test
(test (parse '(define (identity x) x)) (func (list (id 'identity) (id 'x)) (id 'x) ))
(test (parse '(define (adder x y) (+ x y))) (func (list (id 'adder) (id 'x) (id 'y)) (add (id 'x) (id 'y))))
(test (parse '(define (adder x) (+ x 1))) (func (list (id 'adder) (id 'x)) (add (id 'x) (num 1))))
(test (parse '(define x 1)) (func (list (id 'x)) (num 1)))
(test (parse '(define (less-than-zero x) (if (< x 0) 1 2))) (func (list (id 'less-than-zero) (id 'x)) (if0 (less (id 'x) (num 0)) (num 1) (num 2))))

;; exception tests
(test/exn (parse '(define)) "")
;(test/exn (parse '(define (+ x y))) "")

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
                            (define-values (interp-body stack) (helper-body body (- params-num 1)))
                            (define func-body (append (list 'func)
                                                      (list ($string func-name))
                                                      params-lst
                                                      ; hardcoded return type
                                                      (list '(result i32))
                                                      (list interp-body)))]
                    (list 'module export-body func-body))]
              [else (error "We allow only functions to be transpiled into WASM text format")]))

          (define (helper-body expr stack-pos)
            (type-case WASM expr
              [add (lhs rhs)
                   (local [(define-values (lhs-wat lhs-pos) (helper-body lhs stack-pos))
                           (define-values (rhs-wat rhs-pos) (helper-body rhs lhs-pos))] 
                   (values `(i32.add
                             ,lhs-wat
                             ,rhs-wat)
                           rhs-pos))]
              [sub (lhs rhs)
                   (local [(define-values (lhs-wat lhs-pos) (helper-body lhs stack-pos))
                           (define-values (rhs-wat rhs-pos) (helper-body rhs lhs-pos))] 
                   (values `(i32.sub
                             ,lhs-wat
                             ,rhs-wat)
                           rhs-pos))]
              [mult (lhs rhs)
                    (local [(define-values (lhs-wat lhs-pos) (helper-body lhs stack-pos))
                            (define-values (rhs-wat rhs-pos) (helper-body rhs lhs-pos))] 
                    (values `(i32.mul
                             ,lhs-wat
                             ,rhs-wat)
                           rhs-pos))]
              [less (lhs rhs)
                     (local [(define-values (lhs-wat lhs-pos) (helper-body lhs stack-pos))
                             (define-values (rhs-wat rhs-pos) (helper-body rhs lhs-pos))] 
                    (values `(i32.lt_s
                              ,lhs-wat
                              ,rhs-wat)
                            rhs-pos))]
              [if0 (c-expr t-expr e-expr)
                   (local [(define-values (t-wat t-pos) (helper-body t-expr stack-pos))
                           (define-values (e-wat e-pos) (helper-body e-expr t-pos))
                           (define-values (c-wat c-pos) (helper-body c-expr e-pos))]
                     ;; select instruction returns its first operand if condition is true, or its second operand otherwise.
                    (values `(select
                              (return ,t-wat)
                              (return ,e-wat)
                              ,c-wat)
                            c-pos))]
              [num (v)
                  (values `(i32.const ,v)
                          stack-pos)]
              [id (i)
                  ;; need to decrement stack-pos when popping the params from stack
                   (values `(get_local ,($string stack-pos))
                           (- stack-pos 1))]
              ;; don't need to decrement stack pos for recursive function call
              [call (f-name expr)
                    (local [(define-values (e-wat e-pos) (helper-body expr stack-pos))]
                    (values `(call ,($string (symbol->string (id-name f-name)))
                                   ,e-wat) stack-pos))]
              [else (error "Illegal expression in the body of the function")]))]
    (helper expr 0)))

;; interpret component tests

(test (interp (parse 'x))
      '(param $0 i32))


;; interpret functions

;; a nonsensical function
(test (interp (parse '(define (dec x) (dec (- x 1)))))
      '(module
           (export "dec" (func $dec))
         (func $dec (param $0 i32) (result i32)
               (call $dec (i32.sub
                           (get_local $0)
                           (i32.const 1)))))
      )

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

(test (interp (parse '(define (multplr x y) (* x y))))
      '(module
          (export "multplr" (func $multplr))
        (func $multplr (param $0 i32) (param $1 i32) (result i32)
              (i32.mul
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

(test (interp (parse '(define (silly-if n) (if (< n 1) 2 3))))
      '(module
           (export "silly-if" (func $silly-if))
         (func $silly-if (param $0 i32) (result i32)
                (select
                 (return (i32.const 2))
                 (return (i32.const 3))
                 (i32.lt_s
                  (get_local $0)
                  (i32.const 1)))))

      )

;; need to be careful here as the order of pushing params on stack in signature matters for correct behavior of 'select'
(test (interp (parse '(define (silly-if2 x w v) (if (< x 1) v w))))
      '(module
           (export "silly-if2" (func $silly-if2))
         (func $silly-if2 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
                (select
                    (return (get_local $2))
                    (return (get_local $1))
                 (i32.lt_s
                  (get_local $0)
                  (i32.const 1)))))

      )

;; Behold, The Fibonacci.
(test (interp (parse '(define (fib n) (if (< n 2)
                                          1
                                          (+ (fib (- n 1)) (fib (- n 2)))))))
     '(module
           (export "fib" (func $fib))
         (func $fib (param $0 i32) (result i32)
                (select
                       (return
                        (i32.const 1)
                        )
                      (return
                       (i32.add
                        (call $fib
                              (i32.sub
                               (get_local $0)
                               (i32.const 1)
                               )
                              )
                        (call $fib
                              (i32.sub
                               (get_local $0)
                               (i32.const 2)
                               )
                              )
                        )
                       )
                    (i32.lt_s
                     (get_local $0)
                     (i32.const 2))))
         )

      )


;; TODO: need to fix state threading for factorial
(test (interp (parse '(define (fact n) (if (< n 2)
                                        1
                                       (* n (fact (- n 1)))))))
     '(module (export "fact" (func $fact))
              (func $fact (param $0 i32) (result i32)
                    (select
                     (return (i32.const 1))
                     (return (i32.mul (get_local $0) (call $fact (i32.sub (get_local $-1) (i32.const 1)))))
                     (i32.lt_s (get_local $-1) (i32.const 2)))))
      )

;(module
;           (export "fib" (func $fib))
;         (func $fib (param $0 i32) (result i32)
;                (if (i32.lt_s
;                     (get_local $0)
;                     (i32.const 2))
;                    (return (i32.const 1))
;                    (else (return (i32.add
;                     (call $fib
;                           (i32.sub
;                            (get_local $0)
;                            (i32.const 1)))
;                     (call $fib
;                           (i32.sub
;                            (get_local $0)
;                            (i32.const 2)))
;                     )))
;                    end)
;                    )
;  )



;; exceptions
; TODO need to fix this test by incorporating the lookup above, i.e., check if args in the parameter list are exhaustive
#;(test/exn (interp
           (parse '(define (x) (+ x y))))
          "")

(define (transpile inname outname)
  (local [(define filecontent (first (file->list inname)))
          (define parse-out (parse filecontent))
          (define interp-out (interp parse-out))]
  (with-output-to-file outname #:exists 'replace
    (lambda () (display interp-out)))))

