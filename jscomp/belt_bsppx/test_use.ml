let n = Test.create "10" 10
let b = match n with
| None -> assert false
| Some n -> n

let _ = print_endline (match (Test.keyGet b) with
| None -> assert false
| Some key -> key)
