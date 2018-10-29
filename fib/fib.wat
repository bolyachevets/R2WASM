(module
 (export "fib" (func $fib))
 (func $fib (param $0 i32) (result i32)
  (local $1 i32)
  (local $2 i32)
  (local $3 i32)
  (block $label$0
   (br_if $label$0
    (i32.eqz
     (get_local $0)
    )
   )
   (set_local $3
    (i32.const 1)
   )
   (block $label$1
    (br_if $label$1
     (i32.lt_s
      (get_local $0)
      (i32.const 2)
     )
    )
    (set_local $0
     (i32.add
      (get_local $0)
      (i32.const -1)
     )
    )
    (set_local $2
     (i32.const 0)
    )
    (set_local $3
     (i32.const 1)
    )
    (loop $label$2
     (set_local $3
      (i32.add
       (tee_local $1
        (get_local $3)
       )
       (get_local $2)
      )
     )
     (set_local $2
      (get_local $1)
     )
     (br_if $label$2
      (tee_local $0
       (i32.add
        (get_local $0)
        (i32.const -1)
       )
      )
     )
    )
   )
   (return
    (get_local $3)
   )
  )
  (i32.const 0)
 )
)