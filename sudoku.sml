PolyML.print_depth 1000000000;

fun sumOfElements [] = 0
  | sumOfElements (l::ls) = l + sumOfElements ls;

fun replaceOneUnknown ([], _) = []
  | replaceOneUnknown (0::ls, new) = new :: (replaceOneUnknown (ls, new))
  | replaceOneUnknown (l::ls, new) = l :: (replaceOneUnknown(ls, new));



fun oneUnknown [] = [] 
  | oneUnknown (l::ls) = 
    if List.length (List.filter (fn x => x = 0) l) = 1 then
	(replaceOneUnknown (l, 45 - sumOfElements l)) :: (oneUnknown ls)
    else
	l :: (oneUnknown ls);


(*
vToH (v, r)
TYPE: 'a list list * int -> 'a list
PRE: true
POST: a list of elements on position r in each of the lists in v
*)

fun vToH ([], _) = []
  | vToH (v::vs, row) =
    [List.nth(v, row-1)] @ vToH(vs, row);

(*
updateVerticalToHorizontal (h,v)
TYPE: 'a list * 'b list list -> 'b list list
PRE: true
POST: h updated with the corresponding elements in v
*)

fun updateVerticalToHorizontal ([], v) = []
  | updateVerticalToHorizontal (h::hs, v) = 
    vToH(v, 9 - List.length(hs)) :: updateVerticalToHorizontal(hs, v);

(* hToV (h, r)
TYPE: 'a list list * int -> 'a list
PRE: true
POST: a list of elements on position r in each of the lists in h
*)

fun hToV ([], _) = []
  | hToV (h::hs, row) =
    [List.nth(h, row-1)] @ hToV(hs, row);

(*
updateHorizontalToVertical (v,h)
TYPE: 'a list * 'b list list -> 'b list list
PRE: true
POST: v updated with the corresponding elements in h
*)

fun updateHorizontalToVertical([], h) = []
  | updateHorizontalToVertical (v::vs, h) = 
    hToV(h, 9 - List.length(vs)) :: updateHorizontalToVertical(vs, h);





val h = [[0,2,0,4,5,6,7,8,9],
	 [4,5,7,0,8,0,2,3,6],
	 [6,8,9,2,3,7,0,4,0],
	 [0,0,5,3,6,2,9,7,4],
	 [2,7,4,0,9,0,6,5,3],
	 [3,9,6,5,7,4,8,0,0],
	 [0,4,0,6,1,8,3,9,7],
	 [7,6,1,0,4,0,5,2,8],
	 [9,3,8,7,2,5,0,6,0]];

val v = [[0,4,6,0,2,3,0,7,9],
	 [2,5,8,0,7,9,4,6,3],
	 [0,7,9,5,4,6,0,1,8],
	 [4,0,2,3,0,5,6,0,7],
	 [5,8,3,6,9,7,1,4,2],
	 [6,0,7,2,0,4,8,0,5],
	 [7,2,0,9,6,8,3,5,0],
	 [8,3,4,7,5,0,9,2,6],
	 [9,6,0,4,3,0,7,8,0]];

val r = [[0,2,0,4,5,7,6,8,9],[4,5,6,0,8,0,2,3,7],[7,8,9,2,3,6,0,4,0],
	 [0,0,5,2,7,4,3,9,6],[3,6,2,0,9,0,5,7,4],[9,7,4,6,5,3,8,0,0],
	 [0,4,0,7,6,1,9,3,8],[6,1,8,0,4,0,7,2,5],[3,9,7,5,2,8,0,6,0]];
