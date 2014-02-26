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
datatype SudokuTree = Empty
	 | STree of Sudoku * SudokuTree list;

(* sumOfElements l
   TYPE: int list -> int
   PRE: true
   POST: sum of all elements in l
*)

fun sumOfElements [] = 0
  | sumOfElements (l::ls) = l + sumOfElements ls;


(* replaceOneUnknown (l, n)
   TYPE: int list * int -> int list
   PRE: true
   POST: replaces the 0 in l with n
*)

fun replaceOneUnknown ([], _) = []
  | replaceOneUnknown (0::ls, new) = new :: (replaceOneUnknown (ls, new))
  | replaceOneUnknown (l::ls, new) = l :: (replaceOneUnknown(ls, new));

(* oneUnknown' l
   TYPE: int list list -> int list list
   PRE: l only contains unique numbers 0-9. With maximum of 9 elements
   POST: replaces the 0 in l with the missing element of 1-9
*)

fun oneUnknown' [] = [] 
  | oneUnknown' (l::ls) = 
    if List.length (List.filter (fn x => x = 0) l) = 1 then
	(replaceOneUnknown (l, 45 - sumOfElements l)) :: (oneUnknown' ls)
    else
	l :: (oneUnknown' ls);

(* oneUnknown l
   TYPE: int list list -> int list list
   PRE: l only contains unique numbers 0-9. With maximum of 9 elements
   POST: replaces the 0 in l with the missing element of 1-9
*)

fun oneUnknown l = 
    let
	val newL = oneUnknown' l
    in
	if newL = l then
	    l
	else
	    oneUnknown newL
    end;

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



(* updateHorizontalToSquare l
   TYPE: 'a list list -> 'a list list
   PRE: l contains 9 lists, with 9 elements each
   POST: the lists divided into 3x3-squares
*)

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
    
(* updateSquareToHorizontal l
   TYPE: 'a list list -> 'a list list
   PRE: l contains 9 lists, with 9 elements each
   POST: the 3x3 squares divided into 9 rows
*)

fun updateSquareToHorizontal ([]) = []
  | updateSquareToHorizontal (s) = 
    let

	val square1 = List.nth(s, 0)
	val square2 = List.nth(s, 1)
	val square3 = List.nth(s, 2)
	val square4 = List.nth(s, 3)
	val square5 = List.nth(s, 4)
	val square6 = List.nth(s, 5)
	val square7 = List.nth(s, 6)
	val square8 = List.nth(s, 7)
	val square9 = List.nth(s, 8)
	

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

(* ascii' l
   TYPE: int list -> string
   PRE: l contains 9 elements
   POST: string representation of l
*)

fun ascii' ([]) = ""
  | ascii' (h::hs) =
    let
        val lhs = length(hs)
    in
        if 9 - lhs = 1 then
            "| " ^ Int.toString(h) ^ " " ^ ascii'(hs)
        else if 9 - lhs = 3 orelse 9 - lhs = 6 orelse 9 - lhs = 9 then
            Int.toString(h) ^ " | " ^ ascii'(hs)
	else
	    Int.toString(h) ^ " " ^ ascii'(hs)
    end;

(* ascii s
   TYPE: Sudoku -> unit
   PRE: l contains 9 lists, with 9 elements each
   POST: ()
   SIDE-EFFECTS: prints the ascii representation of s
*)

fun ascii (Puzzle([], _, _)) = ()
  | ascii (Puzzle(h::hs, v, s)) =
    let 
	val lhs = List.length(hs)
    in
	(
	if 9 - lhs = 1 then
	    (print("+-----------------------+\n");
	     print(ascii'(h) ^ "\n"))
	else if 9 - lhs = 9 orelse 9 - lhs = 3 orelse 9 - lhs = 6  then
	    (print(ascii'(h) ^ "\n");
	     print("+-----------------------+\n"))
	else
	    print(ascii'(h) ^ "\n");
	     
	ascii(Puzzle(hs, v, s))
	)
    end

(* duplicatesExists' l
   TYPE: int list -> bool
   PRE: true
   POST: true if duplicates exists in l, false otherwise
*)

fun duplicatesExists' [] = false
  | duplicatesExists' (l::ls) = 
    if List.exists (fn x => x = l andalso x <> 0 ) ls then
	true
    else
	duplicatesExists' ls;


(* duplicatesExists l
   TYPE: int list -> bool
   PRE: true
   POST: true if duplicates exists in l, false otherwise
*)

fun duplicatesExists ([]) = false
  | duplicatesExists (l::ls) = 
    if duplicatesExists'(l) then
	true
    else
	duplicatesExists ls;


(* ====================== http://stackoverflow.com/a/5631165/1523238 ====================== *)

(* interleave x l
   TYPE: 'a -> 'a list -> 'a list list
   PRE: true
   POST: a list of lists where x is moving one step through l each time
*)


fun interleave x [] = [[x]]
  | interleave x (h::t) =
    (x::h::t)::(List.map(fn l => h::l) (interleave x t))

(* permute l
   TYPE: 'a list -> 'a list list
   PRE: true
   POST: permutations of l
   EXAMPLE: permute [1,2,3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]] 
*)
		   
fun permute nil = [[]]
  | permute (h::t) = List.concat( List.map (fn l => interleave h l) (permute t))

(* ======================================================================================== *)


(* notInSquare' (l, n, acc)
   TYPE: int list * int * int list -> int list
   PRE: true
   POST: numbers 1-9 that are missing in l
*)

fun notInSquare' (_, 0, acc) = acc
  | notInSquare' (l, n, acc) = if (List.exists (fn x => x = n) l)  = false then
				  notInSquare'(l, n-1, n::acc)
			      else
				  notInSquare'(l, n-1, acc)

(* notInSquare (l)
   TYPE: int list -> int list
   PRE: true
   POST: numbers 1-9 that are missing in l
*)

fun notInSquare(l) = notInSquare'(l, 9, []);

(*
  squareWithLeastUnknowns' (s, acc, n)
  TYPE: int list list * int list * int -> int list * int
  PRE: true
  POST: the first 3x3 square list in s with the lowest amount of unknown elements, and the position of it
*)


fun squareWithLeastUnknowns' ([], acc, n) = (acc, n)
  | squareWithLeastUnknowns' (s::ss, acc, n) = 
    if (List.length (List.filter (fn x => x = 0) s) < List.length (List.filter (fn x => x = 0) acc) andalso 
       (List.exists (fn x => x = 0) s)) orelse List.length (List.filter (fn x => x = 0) acc) = 0  then

	squareWithLeastUnknowns' (ss, s, 9 - List.length(ss))
    else
	squareWithLeastUnknowns' (ss, acc, n);

(*
  squareWithLeastUnknowns (s)
  TYPE: int list list -> int list * int
  PRE: true
  POST: the first 3x3 square list in s with the lowest amount of unknown elements along with the position of it in s
*)


fun squareWithLeastUnknowns (s) = squareWithLeastUnknowns'(s, List.hd(s), 1);


(* possibleSolutionsForSquare' (s, m)
   TYPE: int list * int list -> int list
   PRE: true
   POST: s with each zero replaced with the next element in m
   EXAMPLE: possibleSolutionsForSquare'([1,0,0,4,5,6,7,8,9], [2,3]) = [1, 2, 3, 4, 5, 6, 7, 8, 9]
*)

fun possibleSolutionsForSquare' ([], _) = []
  | possibleSolutionsForSquare' (l, []) = l
  | possibleSolutionsForSquare' (l::ls, m::ms) = 
    if l = 0 then
	m :: possibleSolutionsForSquare'(ls, ms)
    else
	l :: possibleSolutionsForSquare'(ls, m::ms)

(* possibleSolutionsForSquare (s, missing)
   TYPE: int list * int list list -> int list list
   PRE: true
   POST: possible solutions for the square s
   EXAMPLE: possibleSolutionsForSquare([1,0,0,4,5,6,7,8,9], [[2,3], [3,2]]) = [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 3, 2, 4, 5, 6, 7, 8, 9]]
*)

fun possibleSolutionsForSquare (s, []) = []
  | possibleSolutionsForSquare (s, m::ms) = possibleSolutionsForSquare'(s, m) :: possibleSolutionsForSquare (s, ms);


(* replaceAtPos' (l, n, pos)
   TYPE: 'a list * 'a * int -> 'a list
   PRE: true
   POST: l with the element at position pos replaced with n
*)

fun replaceAtPos' (l, new, 0) = 
    List.take(l, 0) @ [new]  @ List.drop(l, 0)
  | replaceAtPos' (l, new, pos) = 
    List.take(l, pos-1) @ [new]  @ List.drop(l, pos)


(* replaceAtPos (p, n, pos)
   TYPE: Sudoku * int list list * int -> Sudoku list
   PRE: true
   POST: A list with puzzles where the square at position pos in p
         has been replaced with each list in n
*)

fun replaceAtPos (_, [], _) = []
  | replaceAtPos (p as Puzzle(h, v, s), n::ns, pos) = 
    let
	val newS = (replaceAtPos'(s, n, pos))
	val newH = updateSquareToHorizontal (newS)
	val newV = updateHorizontalToVertical (v, newH)
    in
	if not (duplicatesExists(newH)) andalso not (duplicatesExists(newV)) andalso 
	   Puzzle(h, v, s) <> Puzzle(newH, newV, newS) then
	    Puzzle(newH, newV, newS) :: replaceAtPos(p, ns, pos)
	else
	    replaceAtPos(p, ns, pos)
    end;

(* possibleNextSteps p
   TYPE: Sudoku -> Sudoku list
   PRE: true
   POST: possible next steps for the puzzle p
*)

fun possibleNextSteps (p as Puzzle(h, v, s)) = 
    let    
	val (square, squareNumber) = squareWithLeastUnknowns (s)
	val missing = notInSquare (square)
	val permutations = permute(missing)
	val solutionsForSquare = possibleSolutionsForSquare(square, permutations)
    in
	replaceAtPos(p, solutionsForSquare, squareNumber)
    end;

(* oneUnknownOnPuzzle s
   TYPE: Sudoku -> Sudoku
   PRE: true
   POST: s with unknowns replaced on rows, columns and squares
         where there are only one unknown
*)

fun oneUnknownOnPuzzle (Puzzle(h, v, s)) = 
    let
	val newH = oneUnknown h;
	val newV = oneUnknown (updateHorizontalToVertical(v, h))
	val newS = oneUnknown (updateHorizontalToSquare (h))
	val newH = oneUnknown (updateVerticalToHorizontal (h, newV))		      
    in
	if Puzzle(newH, newV, newS) = Puzzle(h, v, s) then
	    Puzzle(newH, newV, newS)
	else
	    oneUnknownOnPuzzle (Puzzle(newH, newV, newS))
    end;

(* sumOfAllElements l
   TYPE: int list list -> int
   PRE: true
   POST: sum of all elements in all lists in l
*)

fun sumOfAllElements [] = 0 
  | sumOfAllElements (l::ls) = sumOfElements(l) + sumOfAllElements(ls);

(* listToTreeList l
   TYPE: Sudoku list -> SudokuTree list
   PRE: true
   POST: a list of trees where the elements in l are the nodes in each tree
*)

fun listToTreeList [] = []
  | listToTreeList (l::ls) = 
    STree(l, []) :: listToTreeList(ls)

(* traversal s
   TYPE: SudokuTree -> Sudoku option
   PRE: true
   POST: some solution to s if there is one, none otherwise.
*)

fun traversal (Empty) = NONE
  | traversal (STree(p as (Puzzle(h, v, s)), [])) = 
    let
	val pp as Puzzle(ph, pv, ps) = oneUnknownOnPuzzle p
	val ppp = possibleNextSteps pp
    in
	if ppp = [] orelse ppp = [pp] then
	    if sumOfAllElements(ph) = 405 then
		SOME pp
	    else
		NONE
	else
	    ( traversal(STree(pp, listToTreeList(ppp))))
    end
  | traversal (STree(p as (Puzzle(h, v, s)), [l as (STree(lp as Puzzle(lph,_,_), []))])) = 
    if possibleNextSteps lp = [] then
	if sumOfAllElements(lph) = 405 then
	    SOME lp
	else
	    NONE
    else
	traversal (l)

  | traversal (STree(p as (Puzzle(h, v, s)), l::ls)) = 
    let
	val lResult = traversal l
    in
	if lResult = NONE then
	    traversal(STree(p, ls))
	else
	    lResult
    end


(* EASY
val h = [[0,2,0,4,5,6,7,8,9],
	 [4,5,7,0,8,0,2,3,6],
	 [6,8,9,2,3,7,0,4,0],
	 [0,0,5,3,6,2,9,7,4],
	 [2,7,4,0,9,0,6,5,3],
	 [3,9,6,5,7,4,8,0,0],
	 [0,4,0,6,1,8,3,9,7],
	 [7,6,1,0,4,0,5,2,8],
	 [9,3,8,7,2,5,0,6,0]];
*)


(* HARD
val h = [[0,0,0,8,4,0,0,0,9],
	 [0,0,1,0,0,0,0,0,5],
	 [8,0,0,0,2,1,4,6,0],
	 [7,0,8,0,0,0,0,9,0],
	 [0,0,0,0,0,0,0,0,0],
	 [0,5,0,0,0,0,3,0,1],
	 [0,2,4,9,1,0,0,0,7],
	 [9,0,0,0,0,0,5,0,0],
	 [3,0,0,0,8,4,0,0,0]];
*)


(* MEDIUM *)
val h = [[0,0,3,0,9,2,0,0,0],
	 [4,0,0,0,3,0,0,1,0],
	 [2,7,0,0,0,0,0,0,0],
	 [0,1,0,3,0,0,0,0,8],
	 [0,5,0,1,6,7,0,3,0],
	 [3,0,0,0,0,8,0,6,0],
	 [0,0,0,0,0,0,0,5,3],
	 [0,3,0,0,8,0,0,0,9],
	 [0,0,0,6,2,0,1,0,0]];

val v = updateHorizontalToVertical(h, h);

val s  = updateHorizontalToSquare(h);

val p = Puzzle(h,v,s)

val t = STree (p, []);

fun timee t = (print (Date.toString(Date.fromTimeLocal(Time.now ())) ^ "\n"); traversal t; print (Date.toString(Date.fromTimeLocal(Time.now ()))));
