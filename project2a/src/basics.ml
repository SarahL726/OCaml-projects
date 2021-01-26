(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = match tup with (a,b,c)->(c,b,a);;

let abs x = if x < 0 then -x else x;;

let area x y = match x,y with (a,b),(c,d)->abs(a - c) * abs(b - d);;

let volume x y = match x,y with (a,b,c),(d,e,f)->abs(a - d) * abs(b - e) * abs(c - f);;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = if x = 0 then 1 else if x = 1 then 1 else x * factorial(x-1);;

let rec pow x y = if y = 0 then 1 else x * pow x (y-1);;

let rec log x y = if y < x then 0 else 1 + log x (y/x);;

let rec is_prime x = 
  let rec helper num div = match div with
      | 1 -> true    
      | _ -> (num mod div <> 0) && helper num (div-1)
  in match x <= 1 with
    | true-> false
    | false -> helper x (x-1)
;;

let rec next_prime x = match is_prime x with
  | true -> x
  | false -> next_prime (x+1)
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
| [] -> failwith "Out of bounds"
| h::r -> 
    if idx = 0 then h 
    else get (idx-1) r
;;

let larger lst1 lst2 = 
  let rec length_helper lst = match lst with
  | [] -> 0
  | h::r -> 1 + length_helper r in
  if length_helper lst1 > length_helper lst2 then lst1
  else if length_helper lst2 > length_helper lst1 then lst2
  else []
;;

let reverse lst = 
  let rec reverse_helper lst = function
  | [] -> lst
  | h::t -> reverse_helper (h::lst) t in
  reverse_helper [] lst
;;

let rec combine lst1 lst2 = match lst1 with
| h::t -> h::combine t lst2
| []->lst2
;;

let rec rotate shift lst = 
  let rec last = function
  | [] -> assert false 
  | [h] -> h
  | h::r -> last r in
  let rec init = function
  | [] -> []
  | [h] -> []
  | h::r -> h::(init r) in
  let one_rot lst = (last lst)::(init lst) in
  match shift with
  | 0 -> lst
  | _ -> rotate (shift - 1) (one_rot lst)
;;