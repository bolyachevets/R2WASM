(module (export "fact" (func $fact))
              (func $fact (param $n i32) (result i32)
                    (if (result i32)
                        (i32.lt_s (get_local $n)
                                  (i32.const 2))
                    (then (i32.const 1))
                    (else (i32.mul (get_local $n)
                                   (call $fact (i32.sub (get_local $n)
                                                        (i32.const 1))))))))
