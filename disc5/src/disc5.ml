(*Provided for your benefit*)
let rec map f xs = match xs with
| [] -> []
| x :: xt -> (f x)::(map f xt)

let rec foldl f a xs = match xs with
| [] -> a
| x :: xt -> foldl f (f a x) xt

let rec foldr f xs a = match xs with
| [] -> a
| x :: xt -> f x (foldr f xt a) 

(********************)
(* Currying Functions and Partial Application *)
(********************)

let mul_n n lst = map (fun x->x * n) lst;;

(* Joins together the strings in xs by separator sep
   e.g. join ["cat"; "dog"; "fish"] "," = "cat,dog,fish". *)
let join xs sep = match xs with
   | [] -> ""
   | h::t -> foldl (^) h (map ((^) sep) t)
;;

(********************)
(* Option Functions *)
(********************)

(* Converts an option to a list. Some is a singleton containing
   the value, while None is an empty list. *)
let list_of_option (o : 'a option) : 'a list = match o with
   | Some x -> [x]
   | None -> []
;;

(* If the pair's key matches k returns the value as an option. Otherwise
   return None. *)
let match_key (k : 'k) (p : ('k * 'v)) : 'v option = 
   match p with
   |(key, v) -> if key = k then Some v else None
;;

(******************)
(*LENGTHLIST FUNCTIONS*)
(******************)

(*This list encodes the idea of a list having multiple elements in a row*)
(*Ex: [1;2;3;4] = Cons(1, 1, Cons(2, 1, Cons(3, 1, Cons(4, 1, Empty))))*)
(*Ex: [1; 1; 1; 1; 2; 2; 2; 3; 3; 4] = Cons(1, 4, Cons(2, 3, Cons(3, 2, Cons(4, 1, Empty))))*)

type 'a lengthlist = 
    Cons of ('a * int * 'a lengthlist)
    | Empty
;;

let rec list_of_lengthlist llst = 
   let rec helper n m =
      if n = 1 then m::[]
      else m::(helper (n-1) m) in
   match llst with
   | Empty -> []
   | Cons (elem, num, lst) -> (helper num elem)@(list_of_lengthlist lst)
;;

let rec map_lengthlist fn llst = match llst with
| Empty -> Empty
| Cons (elem, num, lst) -> Cons (fn elem, num, map_lengthlist fn lst);;

let rec decrement_count llst = match llst with
| Empty -> Empty
| Cons (elem, num, lst) -> 
   if num = 1 then lst
   else Cons (elem, num-1, decrement_count lst);;