(module
 (export "add" (func $add))
 (func $add (param $0 i32) (param $1 i32) (result i32)
  (i32.add
   (get_local $1)
   (get_local $0)
  )
 )
)
