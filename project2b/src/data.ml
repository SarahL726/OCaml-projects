open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
  | IntLeaf -> 0
  | IntNode (y, l, r) -> 1 + int_size l + int_size r
;;

let rec int_max t =  match t with
  | IntLeaf -> raise (Invalid_argument("int_max"))
  | IntNode (y, l, r) -> match r with
    | IntLeaf -> y
    | IntNode (_, _, _) -> int_max r
;;

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let pinsert x t = 
  let rec helper x t =
    let (f, tree) = t in match tree with
      | Leaf -> (f, Node(x, Leaf, Leaf))
      | Node (y, l, r) when f x y > 0 -> let (f2, newr) = helper x (f,r) in (f, Node(y, l, newr))
      | Node (y, l, r) when f x y = 0 -> (f, tree)
      | Node (y, l, r) -> let (f2, newl) = helper x (f, l) in (f, Node(y, newl, r)) 
  in helper x t
;;

let pmem x t = 
  let rec helper x t = 
    let (f, tree) = t in match tree with
      | Leaf -> false
      | Node (y, l, r) when f x y > 0 -> let newt = (f, r) in helper x newt
      | Node (y, l, r) when f x y = 0 -> true
      | Node (y, l, r) -> let newt = (f, l) in helper x newt 
  in helper x t    
;;

let pinsert_all lst t = fold (fun t x -> pinsert x t) t lst;;

let rec p_as_list t = 
  let (f, tree) = t in match tree with
    | Leaf -> []
    | Node (y, l, r) -> (p_as_list (f,l))@[y]@(p_as_list (f,r))
  ;;

let pmap f t = 
  let rec helper f t =
    let (f2, tree) = t in match tree with
      | Leaf -> (f2, Leaf)
      | Node (y, l, r) -> (f2, Node(f y, (let (_, left) = helper f (f2, l) in left), (let (_, right) = helper f (f2, r) in right)))
  in 
  let (f2,_) = t in 
  pinsert_all (p_as_list (helper f t)) (empty_ptree f2)
;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = (string * int) list list;;

let empty_table () : lookup_table = [];;

let push_scope (table: lookup_table) : lookup_table = ["end", -10000]::table;;

let pop_scope (table: lookup_table) : lookup_table = match table with
  | [] -> failwith "No scopes remain!"
  | _::t -> t
;;

let add_var name value (table: lookup_table) : lookup_table = match table with
  | [] -> failwith "There are no scopes to add a variable to!"
  | h::t -> match h with
    | [] -> failwith "There are no scopes to add a variable to!"
    | head::tail -> ([(name, value)]@(head::tail))::t
;;

let rec lookup name (table: lookup_table) = match table with
  | [] -> failwith "Variable not found!"
  | h::t -> 
    let rec helper lst n = match lst with
      | [] -> failwith "Variable not found!"
      | h::t -> 
        let (n,v) = h in 
        if n = name then v else helper t name
    in helper h name
;;

(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = match s with
  | Circ {radius = r ; center} -> 3.14 *. r *. r
  | Square {length = l ; upper} -> l *. l
  | Rect {width = w ; height = h ; upper} -> w *. h
;;

let filter f lst = 
  let rec helper f lst = match lst with
    | [] -> []
    | h::t -> if (f h) then h::(helper f t) else helper f t
  in helper f lst
;;
