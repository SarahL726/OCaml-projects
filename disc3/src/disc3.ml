(* Part 1: Type inference *)

let f1 a b = a + b;;

let f2 a b = if a then b else a;;

let f3 a b c = if (a +. b) == 0.0 then "Hi" else c;;

(* Part 2: Type definition *)

let tf1 a = if a == "Hi" then 0 else 1;;

let tf2 a b c = if b = c then true else false;;

let tf3 a b = if a == b then match a with (a::_)->a else match b with (b::_)->b;;

(* Part 3: Functions *)

let concat str1 str2 = str1 ^ str2;;

let add_to_float integer flt = float_of_int integer  +. flt;;

let rec fib n = 
  if n = 0 then 0
  else if n = 1 then 1
  else if n = 2 then 1
  else fib(n-1) + fib(n-2)
;;

(* Part 4: Lists *)

let rec add_three lst = match lst with
|[] -> []
| h::t -> 
let new_t = add_three t in h + 3 :: new_t
;;

let rec filter n lst = match lst with
|[] -> []
|h::t -> 
  let new_t = filter n t in
  if h > n then new_t else h::new_t
;;

let rec double lst = failwith "unimplemented";;
