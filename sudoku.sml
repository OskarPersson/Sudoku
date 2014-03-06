PolyML.print_depth 1000000000;

(* REPRESENTATION CONVENTION: Consists of three lists which represents the rows, 
                              the columns and the 3x3-squares of a sudoku puzzle, respectively.
                              An unknown element is represented by 0
   REPRESENTATION INVARIANT:  The lists only contains elements with number 0-9
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
   VARIANT: |l|
   EXAMPLE: sumOfElements ([1,2,3,4]) = 10
*)

fun sumOfElements [] = 0
  | sumOfElements (l::ls) = l + sumOfElements ls;


(* replaceOneUnknown (l, n)
   TYPE: int list * int -> int list
   PRE: there is a maximum of one 0 in l
   POST: replaces the 0 in l with n
   VARIANT: |l|
   EXAMPLE: replaceOneUnknown ([1,0,3,4], 9) = [1, 9, 3, 4]
*)

fun replaceOneUnknown ([], _) = []
  | replaceOneUnknown (0::ls, new) = new :: (replaceOneUnknown (ls, new))
  | replaceOneUnknown (l::ls, new) = l :: (replaceOneUnknown(ls, new));





(* oneUnknown l
   TYPE: int list list -> int list list
   PRE: l only contains 9 elements, all unique numbers 0-9
   POST: replaces the 0 in l with the missing element of 1-9
   VARIANT: |l|
   EXAMPLE: oneUnknown [[1, 0, 3, 4, 5, 6, 7, 8, 9 ]] = [[1, 2, 3, 4, 5, 6, 7, 8, 9]]
*)

fun oneUnknown l = 
    let

	(* oneUnknown' l
           TYPE: int list list -> int list list
           PRE: l only contains 9 elements, all unique numbers 0-9
           POST: replaces the 0 in l with the missing element of 1-9
           VARIANT: |l|
           EXAMPLE: oneUnknown' [[1, 0, 3, 4, 5, 6, 7, 8, 9 ]] = [[1, 2, 3, 4, 5, 6, 7, 8, 9]]
	*)

	fun oneUnknown' [] = [] 
	  | oneUnknown' (l::ls) = 
	    if List.length (List.filter (fn x => x = 0) l) = 1 then
		(replaceOneUnknown (l, 45 - sumOfElements l)) :: (oneUnknown' ls)
	    else
		l :: (oneUnknown' ls)

	val newL = oneUnknown' l
    in
	if newL = l then
	    l
	else
	    oneUnknown newL
    end;

(*
verticalHorizontalConverter (l)
TYPE: 'a list list -> 'a list list
PRE: true
POST: if l is the vertical representation then the horizontal representation,
      else if l is the horizontal representation then the vertical representation
*)

fun verticalHorizontalConverter (l) = 
    let
	(* verticalHorizontalConverter' (l, c)
           TYPE: 'a list list * int -> 'a list list
           PRE: c is the length of l -1
           POST: if l is the vertical representation then the horizontal representation,
                 else if l is the horizontal representation then the vertical representation
	   VARIANT: c
        *)

	fun verticalHorizontalConverter' (l, 0) = [(List.foldr (fn (x,y) => List.nth(x, 0)::y) [] l)]
	  | verticalHorizontalConverter' (l, c) = verticalHorizontalConverter'(l, c-1) @ [(List.foldr (fn (x,y) => List.nth(x, c)::y) [] l)];

    in
	verticalHorizontalConverter'(l, List.length(l) -1)
    end;

(* squareHorizontalConverter l
   TYPE: 'a list list -> 'a list list
   PRE: v contains 9 lists, with 9 elements each
   POST: if l is the square representation then the horizontal representation,
         else if l is the horizontal representation then the square representation
*)

fun squareHorizontalConverter ([]) = []
  | squareHorizontalConverter (l) = 
    let

	val row1 = List.nth(l, 0)
	val row2 = List.nth(l, 1)
	val row3 = List.nth(l, 2)
	val row4 = List.nth(l, 3)
	val row5 = List.nth(l, 4)
	val row6 = List.nth(l, 5)
	val row7 = List.nth(l, 6)
	val row8 = List.nth(l, 7)
	val row9 = List.nth(l, 8)
	

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
    end;


(* ascii s
   TYPE: Sudoku -> unit
   PRE: true
   POST: ()
   SIDE-EFFECTS: prints the ascii representation of s
*)

fun ascii (Puzzle([], _, _)) = ()
  | ascii (Puzzle(h::hs, v, s)) =
    let 

	(* ascii' l
           TYPE: int list -> string
           PRE: l contains 9 elements
           POST: string representation of l
	   VARIANT: |l|
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
	    end

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

(* duplicatesExists l
   TYPE: int list list -> bool
   PRE: true
   POST: true if duplicates exists in l, false otherwise
   VARIANT: |l|
   EXAMPLE: duplicatesExists ([[1,2,3], [1,2,2]]) = true
*)

fun duplicatesExists ([]) = false
  | duplicatesExists (l::ls) = 
    let
	(* duplicatesExists' l
           TYPE: int list -> bool
           PRE: true
           POST: true if duplicates exists in l, false otherwise
	   VARIANT: |l|
           EXAMPLE: duplicatesExists' ([1,2,2]) = true
        *)

	fun duplicatesExists' [] = false
	  | duplicatesExists' (l::ls) = 
	    if List.exists (fn x => x = l andalso x <> 0 ) ls then
		true
	    else
		duplicatesExists' ls;
    in	
	if duplicatesExists'(l) then
	    true
	else
	    duplicatesExists ls
    end;

(* ====================== http://stackoverflow.com/a/5631165/1523238 ====================== *)

(* interleave x l
   TYPE: 'a -> 'a list -> 'a list list
   PRE: true
   POST: a list of lists where x is moving one step through l for each list
   VARIANT: |l|
   EXAMPLE: interleave 1 [2,3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1]]
*)


fun interleave x [] = [[x]]
  | interleave x (h::t) =
    (x::h::t)::(List.map(fn l => h::l) (interleave x t))

(* permute l
   TYPE: 'a list -> 'a list list
   PRE: true
   POST: permutations of l
   VARIANT: |l|
   EXAMPLE: permute [1,2,3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]] 
*)
		   
fun permute nil = [[]]
  | permute (h::t) = List.concat( List.map (fn l => interleave h l) (permute t))

(* ======================================================================================== *)

(* notInSquare (l)
   TYPE: int list -> int list
   PRE: true
   POST: numbers 1-9 that are missing in l
   EXAMPLE: notInSquare [2,3] = [1, 4, 5, 6, 7, 8, 9]
*)

fun notInSquare(l) = 
    let 
        (* notInSquare' (l, n, acc)
           TYPE: int list * int * int list -> int list
           PRE: acc is empty
           POST: numbers 1-n that are missing in l
           VARIANT: n
           EXAMPLE: notInSquare' ([2,3], 4, []) = [1, 4]
        *)

	fun notInSquare' (_, 0, acc) = acc
	  | notInSquare' (l, n, acc) = if (List.exists (fn x => x = n) l) = false then
					   notInSquare'(l, n-1, n::acc)
				       else
					   notInSquare'(l, n-1, acc)
    in
	notInSquare'(l, 9, [])
    end;



(*
  squareWithLeastUnknowns (s)
  TYPE: int list list -> int list * int
  PRE: true
  POST: the first 3x3 square list in s with the lowest amount of unknown elements along with the position of it in s
*)


fun squareWithLeastUnknowns (s) = 
    let

	(* squareWithLeastUnknowns' (s, acc, n)
           TYPE: int list list * int list * int -> int list * int
           PRE: s has 9 elements, acc is the head of s and n is 1
           POST: the first 3x3 square list in s with the lowest amount of unknown elements, and the position of it
           VARIANT: s
        *)

	fun squareWithLeastUnknowns' ([], acc, n) = (acc, n)
	  | squareWithLeastUnknowns' (s::ss, acc, n) = 
	    if (List.length (List.filter (fn x => x = 0) s) < List.length (List.filter (fn x => x = 0) acc) andalso 
		(List.exists (fn x => x = 0) s)) orelse List.length (List.filter (fn x => x = 0) acc) = 0  then

		squareWithLeastUnknowns' (ss, s, 9 - List.length(ss))
	    else
		squareWithLeastUnknowns' (ss, acc, n);
    in
    
	squareWithLeastUnknowns'(s, List.hd(s), 1)
    end;

(* possibleSolutionsForSquare (l, m)
   TYPE: int list * int list list -> int list list
   PRE: l with all zeros replaced with permutations in m
   POST: possible solutions for the square l
   VARIANT: |m|
   EXAMPLE: possibleSolutionsForSquare([1,0,0,4,5,6,7,8,9], [[2,3], [3,2]]) = 
            [[1, 2, 3, 4, 5, 6, 7, 8, 9], [1, 3, 2, 4, 5, 6, 7, 8, 9]]
*)

fun possibleSolutionsForSquare (s, []) = []
  | possibleSolutionsForSquare (s, m::ms) = 
    let 
	(* possibleSolutionsForSquare' (l, m)
           TYPE: int list * int list -> int list
           PRE: true
           POST: l with each zero replaced with the next element in m
           VARIANT: |l|, |m|
           EXAMPLE: possibleSolutionsForSquare'([1,0,0,4,5,6,7,8,9], [2,3]) = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        *)

	fun possibleSolutionsForSquare' ([], _) = []
	  | possibleSolutionsForSquare' (l, []) = l
	  | possibleSolutionsForSquare' (l::ls, m::ms) = 
	    if l = 0 then
		m :: possibleSolutionsForSquare'(ls, ms)
	    else
		l :: possibleSolutionsForSquare'(ls, m::ms)
					
    in
	possibleSolutionsForSquare'(s, m) :: possibleSolutionsForSquare (s, ms)
    end;


(* replaceAtPos (p, n, pos)
   TYPE: Sudoku * int list list * int -> Sudoku list
   PRE: true
   POST: A list with puzzles where the square at position pos in p
         has been replaced with each list in n
   VARIANT: |n|
*)

fun replaceAtPos (_, [], _) = []
  | replaceAtPos (p as Puzzle(h, v, s), n::ns, pos) = 
    let

	(* replaceAtPos' (l, n, pos)
           TYPE: 'a list * 'a * int -> 'a list
           PRE: true
           POST: l with the element at position pos replaced with n
           EXAMPLE: replaceAtPos' ([1,2,3], 5, 2) = [1, 5, 3]
        *)
	
	fun replaceAtPos' (l, new, 0) = 
	    List.take(l, 0) @ [new]  @ List.drop(l, 0)
	  | replaceAtPos' (l, new, pos) = 
	    List.take(l, pos-1) @ [new]  @ List.drop(l, pos)
						    
	val newS = (replaceAtPos'(s, n, pos))
	val newH = squareHorizontalConverter (newS)
	val newV = verticalHorizontalConverter (newH)
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
	val newV = oneUnknown (verticalHorizontalConverter (h))
	val newS = oneUnknown (squareHorizontalConverter (h))
	val newH = oneUnknown (verticalHorizontalConverter (newV))
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
   VARIANT: |l|
   EXAMPLE: sumOfAllElements ([[1,2], [3,4]]) = 10
*)

fun sumOfAllElements [] = 0 
  | sumOfAllElements (l::ls) = sumOfElements(l) + sumOfAllElements(ls);

(* listToTreeList l
   TYPE: Sudoku list -> SudokuTree list
   PRE: true
   POST: a list of trees where the elements in l are the nodes in each tree
   VARIANT: |l|
*)

fun listToTreeList [] = []
  | listToTreeList (l::ls) = 
    STree(l, []) :: listToTreeList(ls)

(* traversal s
   TYPE: SudokuTree -> Sudoku option
   PRE: true
   POST: some solution to s if there is one, none otherwise
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
    end;


(* createPuzzleFromHorizontal h
   TYPE: int list list -> Sudoku
   PRE: h is a well defined list of horizontal lists
   POST: A Sudoku puzzle with h as the horizontal lists
*)

fun createPuzzleFromHorizontal h = 
    Puzzle(h, verticalHorizontalConverter (h), squareHorizontalConverter (h));


(* solve s 
   TYPE: Sudoku -> unit
   PRE: true
   POST: none
   SIDE-EFFECTS: prints a solution to s if there is one, 
                 otherwise it says that a solution doesn't exist
*)

fun solve s =  
    let
	val result = traversal(STree(s, []))
    in
	if result = NONE then
	    print "There is no solution to this puzzle\n"
	else
	    ascii(valOf(result))
    end;


(* time
   TYPE: Sudoku -> unit
   PRE: true
   POST: (none)
   SIDE-EFFECTS: prints the timestamp for when the function was called and then when it was done
*)

fun time t = (print (Date.toString(Date.fromTimeLocal(Time.now ())) ^ "\n"); 
	      solve t; 
	      print (Date.toString(Date.fromTimeLocal(Time.now ())) ^ "\n"));
