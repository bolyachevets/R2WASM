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
;;         | (call <id> <R2WASM>+)
;;         | (if <R2WASM> <R2WASM> <R2WASM>)
;;         | <wastr>
;;         | (waprint <R2WASM>)
;; <id> can be any symbols except +, -, <, if, call, print and define

(define-type R2WASM
  [num (n number?)]
  [id (name symbol?)]
  [add (lhs R2WASM?) (rhs R2WASM?)]
  [sub (lhs R2WASM?) (rhs R2WASM?)]
  [mult (lhs R2WASM?) (rhs R2WASM?)]
  [less (lhs R2WASM?) (rhs R2WASM?)]
  [if0 (test-exp R2WASM?) (then-exp R2WASM?) (else-exp R2WASM?)]
  [call (f-name id?) (param R2WASM?)]
  [func (signature (listof id?)) (body R2WASM?)]
  [wastr (s string?)]
  [waprint (str wastr?)])


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
    [(list 'print str-exp) (waprint (parse str-exp))]
    [(list f-name f-exp) (call (parse f-name) (parse f-exp))]
    [(? string?) (wastr sexp)]
    [_ (error 'parse "Something went wrong in the parser!")]))

;; basic type tests
(test (parse '0) (num 0))
(test (parse 'fib) (id 'fib))
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 3 2)) (sub (num 3) (num 2)))
(test (parse '(if 1 2 3)) (if0 (num 1) (num 2) (num 3)))
(test (parse '(fib (+ 1 2))) (call (id 'fib) (add (num 1) (num 2))))
(test (parse "abc") (wastr  "abc"))

;; func test
(test (parse '(define (identity x) x)) (func (list (id 'identity) (id 'x)) (id 'x) ))
(test (parse '(define (adder x y) (+ x y))) (func (list (id 'adder) (id 'x) (id 'y)) (add (id 'x) (id 'y))))
(test (parse '(define (adder x) (+ x 1))) (func (list (id 'adder) (id 'x)) (add (id 'x) (num 1))))
(test (parse '(define x 1)) (func (list (id 'x)) (num 1)))
(test (parse '(define (less-than-zero x) (if (< x 0) 1 2))) (func (list (id 'less-than-zero) (id 'x)) (if0 (less (id 'x) (num 0)) (num 1) (num 2))))
;; treat assignment of string to variable as a function in wasm
(test (parse '(define x "abc")) (func (list (id 'x)) (wastr "abc")))
(test (parse '(print "abc")) (waprint (wastr "abc")))

;; exception tests
(test/exn (parse '(define)) "")
;(test/exn (parse '(define (+ x y))) "")

;; this should give an error in the interpreter
(test (parse '(define (x) (+ x y))) (func (list (id 'x)) (add (id 'x) (id 'y))))

(define (interp expr)
  (local [(define interp (lambda args (error "YOU ARE BAD! Don't call interp. Call helper.")))
          (define ($string s) (string->symbol (string-append "$" (if (symbol? s)
                                                                     (symbol->string s)
                                                                     s))))         
          (define (helper expr)
            (type-case R2WASM expr
              ;; handle parameters
              [id (i)
                    ;; for now we only take parameters of type i32
                    `(param ,($string i) i32)]
              ;; handle function
              [func (signature body)
                    (local [(define func-name (symbol->string (id-name (first signature))))
                            ; deal with params in signature                          
                            (define params-lst (map (λ(x) (helper x)) (rest signature)))
                            (define export-body (list 'export func-name (list 'func ($string func-name))))
                            (define interp-body (helper-body body))
                            (define func-body (append (list 'func)
                                                      (list ($string func-name))
                                                      params-lst
                                                      ; hardcoded return type
                                                      (list '(result i32))
                                                      (list interp-body)))]
                    (list 'module export-body func-body))]
              ;; handle print statements
              [waprint (str)
                     (local [(define wastr-val (helper-body str))
                             ;; import console.log from js
                             (define log-import '(import "console" "log" (func $log (param i32 i32))))
                             ;; import 1 page, i.e., 64KB, of memory created in js
                             (define mem-import '(import "js" "mem" (memory 1)))
                             ;; write string to global memory
                             (define data `(data (i32.const 0) ,wastr-val))
                             (define func-body (append `(func (export ,(string-append "fun_" wastr-val)))
                                                       ;; hardcoded offset implies we print a single string only
                                                       (list 'i32.const 0)
                                                       ;; string length
                                                       (list 'i32.const (string-length wastr-val))
                                                       ;; offset and length are passed to js console.log
                                                       `(call ,($string "log"))))]
                       (list 'module log-import mem-import data func-body))]

              [else (error "We allow only functions to be transpiled into WASM text format")]))

          (define (helper-body expr)
            (type-case R2WASM expr
              [add (lhs rhs)
                   (local [(define lhs-wat (helper-body lhs))
                           (define rhs-wat (helper-body rhs))] 
                   `(i32.add
                             ,lhs-wat
                             ,rhs-wat))]
              [sub (lhs rhs)
                   (local [(define lhs-wat (helper-body lhs))
                           (define rhs-wat (helper-body rhs))] 
                   `(i32.sub
                             ,lhs-wat
                             ,rhs-wat))]
              [mult (lhs rhs)
                    (local [(define lhs-wat (helper-body lhs))
                            (define rhs-wat (helper-body rhs))] 
                    `(i32.mul
                             ,lhs-wat
                             ,rhs-wat))]
              [less (lhs rhs)
                     (local [(define lhs-wat (helper-body lhs))
                             (define rhs-wat (helper-body rhs))] 
                    `(i32.lt_s
                              ,lhs-wat
                              ,rhs-wat))]
              [if0 (c-expr t-expr e-expr)
                   (local [(define t-wat (helper-body t-expr))
                           (define e-wat (helper-body e-expr))
                           (define c-wat (helper-body c-expr))]
                    `(if (result i32)
                         ,c-wat
                         (then ,t-wat)
                         (else ,e-wat)))]
              [num (v) `(i32.const ,v)]
              [id (i) `(get_local ,($string i))]
              [wastr (str) str]
              [call (f-name expr)
                    (local [(define e-wat (helper-body expr))]
                    (values `(call ,($string (symbol->string (id-name f-name)))
                                   ,e-wat)))]
              [else (error "Illegal expression in the body of the function")]))]
    (helper expr)))

;; this is not a valid function, just testing id interp

(test (interp (parse 'x))
      '(param $x i32))

;; interpret functions

;; a nonsensical function
(test (interp (parse '(define (dec x) (dec (- x 1)))))
      '(module
           (export "dec" (func $dec))
         (func $dec (param $x i32) (result i32)
               (call $dec (i32.sub
                           (get_local $x)
                           (i32.const 1)))))
      )

(test (interp (parse '(define (identity x) x)))
      '(module
           (export "identity" (func $identity))
         (func $identity (param $x i32) (result i32)
               (get_local $x))))

(test (interp (parse '(define (adder x y) (+ x y))))
      '(module
          (export "adder" (func $adder))
        (func $adder (param $x i32) (param $y i32) (result i32)
              (i32.add
               (get_local $x)
               (get_local $y)))))

(test (interp (parse '(define (multplr x y) (* x y))))
      '(module
          (export "multplr" (func $multplr))
        (func $multplr (param $x i32) (param $y i32) (result i32)
              (i32.mul
               (get_local $x)
               (get_local $y)))))

(test (interp (parse '(define (add1 x) (+ x 1))))
      '(module
           (export "add1" (func $add1))
         (func $add1 (param $x i32) (result i32)
               (i32.add
                (get_local $x)
                (i32.const 1)))))

(test (interp (parse '(define c-fn 1)))
      '(module
           (export "c-fn" (func $c-fn))
         (func $c-fn (result i32)
               (i32.const 1))))

(test (interp (parse '(define (sillyif n) (if (< n 1) 2 3))))
      '(module
           (export "sillyif" (func $sillyif))
         (func $sillyif (param $n i32) (result i32)
               (if (result i32)
                   (i32.lt_s (get_local $n)
                             (i32.const 1))
                   (then (i32.const 2))
                   (else (i32.const 3))
                   )))
      )

(test (interp (parse '(define (sillyif2 x y z) (if (< x 1) y z))))
      '(module
           (export "sillyif2" (func $sillyif2))
         (func $sillyif2 (param $x i32) (param $y i32) (param $z i32) (result i32)
                (if (result i32)
                   (i32.lt_s (get_local $x)
                             (i32.const 1))
                   (then (get_local $y))
                   (else (get_local $z))
                   )))

      )

;; Behold, The Fibonacci.
(test (interp (parse '(define (fib n) (if (< n 2)
                                          1
                                          (+ (fib (- n 1)) (fib (- n 2)))))))
     '(module
          (export "fib" (func $fib))
        (func $fib (param $n i32) (result i32)
              (if (result i32)
                  (i32.lt_s (get_local $n)
                            (i32.const 2))
                  (then (i32.const 1))
                  (else (i32.add (call $fib (i32.sub (get_local $n)
                                                     (i32.const 1)))
                                 (call $fib (i32.sub (get_local $n)
                                                     (i32.const 2)))))))
        )
     )

;; Factorialy Glorianty
(test (interp (parse '(define (fact n) (if (< n 2)
                                        1
                                       (* n (fact (- n 1)))))))
     '(module (export "fact" (func $fact))
              (func $fact (param $n i32) (result i32)
                    (if (result i32)
                        (i32.lt_s (get_local $n)
                                  (i32.const 2))
                        (then (i32.const 1))
                        (else (i32.mul (get_local $n)
                                       (call $fact (i32.sub (get_local $n)
                                                            (i32.const 1))))))))
      )


(test (interp (parse '(print "abc")))
      '(module
           (import "console" "log" (func $log (param i32 i32)))
           (import "js" "mem" (memory 1))
           (data (i32.const 0) "abc")
         (func (export "fun_abc")
               i32.const 0
               i32.const 3
               call $log))
)

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

