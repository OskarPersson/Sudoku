PolyML.print_depth 1000000000;

(* REPRESENTATION CONVENTION: Consists of three lists which represents the rows, 
                              the columns and the 3x3-squares of a sudoku puzzle, respectively.
                              An unknown element is represented by 0
   REPRESENTATION INVARIANT: 
*)
datatype Sudoku = Puzzle of int list list * int list list * int list list;

(* REPRESENTATION CONVENTION: Represents a tree of sudoku puzzles where the first element is the node and
                              the second element represents the subtrees
   REPRESENTATION INVARIANT:
*)
datatype SudokuTree = STree of Sudoku * SudokuTree list;
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




fun updateHorizontalToSquare ([]) = []
  | updateHorizontalToSquare (h) = 
    let

	val row1 = List.nth(h, 0)
	val row2 = List.nth(h, 1)
	val row3 = List.nth(h, 2)
	val row4 = List.nth(h, 3)
	val row5 = List.nth(h, 4)
	val row6 = List.nth(h, 5)
	val row7 = List.nth(h, 6)
	val row8 = List.nth(h, 7)
	val row9 = List.nth(h, 8)
	

	val topLeft = List.take(row1, 3) @
		      List.take(row2, 3) @
		      List.take(row3, 3)
	val topMiddle = List.take(List.drop(row1, 3), 3) @ 
			List.take(List.drop(row2, 3), 3) @
			List.take(List.drop(row3, 3), 3)
	val topRight = List.drop(row1, 6) @
		       List.drop(row2, 6) @
		       List.drop(row3, 6)
	val middleLeft = List.take(row4, 3) @
			 List.take(row5, 3) @
			 List.take(row6, 3)
	val middleMiddle = List.take(List.drop(row4, 3), 3) @
			   List.take(List.drop(row5, 3), 3) @
			   List.take(List.drop(row6, 3), 3)
	val middleRight = List.drop(row4, 6) @
			  List.drop(row5, 6) @
			  List.drop(row6, 6)
	val bottomLeft = List.take(row7, 3) @
			 List.take(row8, 3) @
			 List.take(row9, 3)
	val bottomMiddle = List.take(List.drop(row7, 3), 3) @ 
			   List.take(List.drop(row8, 3), 3) @ 
			   List.take(List.drop(row9, 3), 3)
	val bottomRight = List.drop(row7, 6) @ 
			  List.drop(row8, 6) @ 
			  List.drop(row9, 6)
    in
	topLeft :: topMiddle :: topRight :: middleLeft :: middleMiddle :: 
	middleRight :: bottomLeft :: bottomMiddle :: bottomRight :: []
    end
    
fun updateSquareToHorizontal ([]) = []
  | updateSquareToHorizontal (r) = 
    let

	val square1 = List.nth(r, 0)
	val square2 = List.nth(r, 1)
	val square3 = List.nth(r, 2)
	val square4 = List.nth(r, 3)
	val square5 = List.nth(r, 4)
	val square6 = List.nth(r, 5)
	val square7 = List.nth(r, 6)
	val square8 = List.nth(r, 7)
	val square9 = List.nth(r, 8)
	

	val row1 = List.take(square1, 3) @
		      List.take(square2, 3) @
		      List.take(square3, 3)
	val row2 = List.take(List.drop(square1, 3), 3) @ 
			List.take(List.drop(square2, 3), 3) @
			List.take(List.drop(square3, 3), 3)
	val row3 = List.drop(square1, 6) @
		       List.drop(square2, 6) @
		       List.drop(square3, 6)
	val row4 = List.take(square4, 3) @
			 List.take(square5, 3) @
			 List.take(square6, 3)
	val row5 = List.take(List.drop(square4, 3), 3) @
			   List.take(List.drop(square5, 3), 3) @
			   List.take(List.drop(square6, 3), 3)
	val row6 = List.drop(square4, 6) @
			  List.drop(square5, 6) @
			  List.drop(square6, 6)
	val row7 = List.take(square7, 3) @
			 List.take(square8, 3) @
			 List.take(square9, 3)
	val row8 = List.take(List.drop(square7, 3), 3) @ 
			   List.take(List.drop(square8, 3), 3) @ 
			   List.take(List.drop(square9, 3), 3)
	val row9 = List.drop(square7, 6) @ 
			  List.drop(square8, 6) @ 
			  List.drop(square9, 6)
    in
	row1 :: row2 :: row3 :: row4 :: row5 ::
	row6 :: row7 :: row8 :: row9 :: []
    end



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
