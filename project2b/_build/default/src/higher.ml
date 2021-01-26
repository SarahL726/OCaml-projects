open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = fold (fun curr tar -> if tar = target then curr+1 else curr) 0 lst;;

let uniq lst = fold (fun r elem -> if count_occ r elem = 0 then elem::r else r) [] lst;;

let assoc_list lst = fold (fun r elem -> (elem, count_occ lst elem)::r) [] (uniq lst);;

let ap fns args = fold (fun r func -> r@(map func args)) [] fns;;
