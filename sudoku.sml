PolyML.print_depth 1000000000;

(* REPRESENTATION CONVENTION: Consists of three lists which represents the rows, 
                              the columns and the 3x3-squares of a sudoku puzzle, respectively.
                              An unknown element is represented by 0
   REPRESENTATION INVARIANT: 
*)

datatype Sudoku = Puzzle of int vector vector * int vector vector * int vector vector;

(* REPRESENTATION CONVENTION: Represents a tree of sudoku puzzles where the first element is the node and
                              the second element represents the subtrees
   REPRESENTATION INVARIANT:
*)
datatype SudokuTree = Empty
	 | STree of Sudoku * SudokuTree list;

(* sumOfElements' v, i
   TYPE: int vector * int -> int
   PRE: true
   POST: sum of all elements in v, starting at index i
*)

fun sumOfElements' (v, i) = 
    if i = (Vector.length v)-1 then
	Vector.sub(v, i)
    else
	Vector.sub(v, i) + sumOfElements'(v, i+1);

(* sumOfElements v
   TYPE: int vector -> int
   PRE: true
   POST: sum of all elements in v
*)

fun sumOfElements (v) = sumOfElements'(v, 0);


(* replaceOneUnknown (v, n)
   TYPE: int vector * int -> int vector
   PRE: true
   POST: replaces the 0 in v with n
*)

fun replaceOneUnknown (v, new) = 
    let
	val (i, j) = valOf((Vector.findi (fn (x, y) => y = 0) v))
    in
	Vector.update(v, i, new)
    end;


(* oneUnknown' v
   TYPE: int vector -> int vector
   PRE: v only contains unique numbers 0-9. With maximum of 9 elements
   POST: replaces the 0 in v with the missing element of 1-9
*)

fun oneUnknown' v = 
    let
	val a = (Vector.findi (fn (x, y) => y = 0) v)
	val (i, j) = if a <> NONE then
			 valOf(Vector.findi (fn (x, y) => y = 0) v)
		     else
			 (0, 0)
	val f = Vector.findi (fn (x, y) => y = 0 andalso x > i) v
    in
	if a <> NONE andalso f = NONE then
	    Vector.update(v, i, 45 - sumOfElements v)
	else
	    v
    end;

(* oneUnknown v
   TYPE: int vector -> int vector
   PRE: v only contains vectors with unique numbers 0-9. With maximum of 9 elements
   POST: replaces the 0s in every vector in v with the missing element of 1-9
*)

fun oneUnknown v = 
    let
	val newV = Vector.map (fn x => oneUnknown' x) v
    in
	if newV = v then
	    v
	else
	    oneUnknown newV
    end;

(*
verticalHorizontalConverter' (v, c)
TYPE: 'a vector vector * int -> 'a list list
PRE: true
POST: if v is the vertical representation then the horizontal representation,
      else if v is the horizontal representation then the vertical representation
*)

fun verticalHorizontalConverter' (v, 0) = 
    [(Vector.foldr (fn (x,y) => Vector.sub(x, 0)::y) [] v)]
  | verticalHorizontalConverter' (v, c) = 
    verticalHorizontalConverter'(v, c-1) @ 
    [(Vector.foldr (fn (x,y) => Vector.sub(x, c)::y) [] v)];
    

(*
verticalHorizontalConverter (v)
TYPE: 'a vector vector -> 'a vector vector
PRE: true
POST: if v is the vertical representation then the horizontal representation,
      else if v is the horizontal representation then the vertical representation
*)

fun verticalHorizontalConverter (v) = 
    Vector.fromList(List.map (fn x => Vector.fromList x) 
			     (verticalHorizontalConverter' (v, (Vector.length (v)) - 1)));


(* squareHorizontalConverter v
   TYPE: 'a vector vector -> 'a vector vector
   PRE: v contains 9 lists, with 9 elements each
   POST: if v is the square representation then the horizontal representation,
         else if v is the horizontal representation then the square representation
*)

fun squareHorizontalConverter v = 
    let

	val row1 = Vector.sub(v, 0)
	val row2 = Vector.sub(v, 1)
	val row3 = Vector.sub(v, 2)
	val row4 = Vector.sub(v, 3)
	val row5 = Vector.sub(v, 4)
	val row6 = Vector.sub(v, 5)
	val row7 = Vector.sub(v, 6)
	val row8 = Vector.sub(v, 7)
	val row9 = Vector.sub(v, 8)
			     

	val topLeft = Vector.sub(row1,0) :: Vector.sub(row1,1) :: Vector.sub(row1,2) ::
		      Vector.sub(row2,0) :: Vector.sub(row2,1) :: Vector.sub(row2,2) ::
		      Vector.sub(row3,0) :: Vector.sub(row3,1) :: [Vector.sub(row3,2)]

	val topMiddle = Vector.sub(row1,3) :: Vector.sub(row1,4) :: Vector.sub(row1,5) ::
			Vector.sub(row2,3) :: Vector.sub(row2,4) :: Vector.sub(row2,5) ::
			Vector.sub(row3,3) :: Vector.sub(row3,4) :: [Vector.sub(row3,5)]
									
	val topRight = Vector.sub(row1,6) :: Vector.sub(row1,7) :: Vector.sub(row1,8) ::
		       Vector.sub(row2,6) :: Vector.sub(row2,7) :: Vector.sub(row2,8) ::
		       Vector.sub(row3,6) :: Vector.sub(row3,7) :: [Vector.sub(row3,8)]
								       
	val middleLeft = Vector.sub(row4,0) :: Vector.sub(row4,1) :: Vector.sub(row4,2) ::
			 Vector.sub(row5,0) :: Vector.sub(row5,1) :: Vector.sub(row5,2) ::
			 Vector.sub(row6,0) :: Vector.sub(row6,1) :: [Vector.sub(row6,2)]
									 
	val middleMiddle = Vector.sub(row4,3) :: Vector.sub(row4,4) :: Vector.sub(row4,5) ::
			   Vector.sub(row5,3) :: Vector.sub(row5,4) :: Vector.sub(row5,5) ::
			   Vector.sub(row6,3) :: Vector.sub(row6,4) :: [Vector.sub(row6,5)]
									   
	val middleRight = Vector.sub(row4,6) :: Vector.sub(row4,7) :: Vector.sub(row4,8) ::
			  Vector.sub(row5,6) :: Vector.sub(row5,7) :: Vector.sub(row5,8) ::
			  Vector.sub(row6,6) :: Vector.sub(row6,7) :: [Vector.sub(row6,8)]
									  
	val bottomLeft = Vector.sub(row7,0) :: Vector.sub(row7,1) :: Vector.sub(row7,2) ::
			 Vector.sub(row8,0) :: Vector.sub(row8,1) :: Vector.sub(row8,2) ::
			 Vector.sub(row9,0) :: Vector.sub(row9,1) :: [Vector.sub(row9,2)]
									 
	val bottomMiddle = Vector.sub(row7,3) :: Vector.sub(row7,4) :: Vector.sub(row7,5) ::
			   Vector.sub(row8,3) :: Vector.sub(row8,4) :: Vector.sub(row8,5) ::
			   Vector.sub(row9,3) :: Vector.sub(row9,4) :: [Vector.sub(row9,5)]
									   
	val bottomRight = Vector.sub(row7,6) :: Vector.sub(row7,7) :: Vector.sub(row7,8) ::
			  Vector.sub(row8,6) :: Vector.sub(row8,7) :: Vector.sub(row8,8) ::
			  Vector.sub(row9,6) :: Vector.sub(row9,7) :: [Vector.sub(row9,8)]
    in
	Vector.fromList(
	    List.map (fn x => Vector.fromList x) 
		     (topLeft :: topMiddle :: topRight :: middleLeft :: middleMiddle :: 
		      middleRight :: bottomLeft :: bottomMiddle :: [bottomRight]))
    end;

(*
ascii p
TYPE: Sudoku -> unit vector
PRE: p is a well defined Sudoku
POST: (none)
SIDE-EFFECTS: prints the ascii representation of p
*)


fun ascii (Puzzle(h, v, r)) = 
    let

	(*
        ascii'' (i,x)
        TYPE: int * int vector -> string
        PRE: true
        POST: x as a string
        *)

	fun ascii'' (i, x) =
	    if i = 0 then
		"| " ^ Int.toString(Vector.sub(x, i)) ^ " " ^ ascii''(i+1, x)
            else if i = 2 orelse i = 5 then
		Int.toString(Vector.sub(x, i)) ^ " | " ^ ascii''(i+1, x)
	    else if i = (Vector.length (x)) -1 then
		Int.toString(Vector.sub(x, i)) ^ " | "
	    else
		Int.toString(Vector.sub(x, i)) ^ " " ^ ascii''(i+1, x)

        (*
        ascii' (i,x)
        TYPE: int * int vector -> unit
        PRE: true
        POST: x as a string
        *)

	fun ascii' (i, x) = 

	    if i = 0 then
		(print("+-----------------------+\n");
		 print(ascii''(0, x) ^ "\n"))
	    else if i = 8 orelse i = 2 orelse i = 5 then
		(print(ascii''(0, x) ^ "\n");
		 print("+-----------------------+\n"))	
	    else			
		print(ascii''(0, x) ^ "\n")
			 
    in
	Vector.mapi ascii' h
    end

(* find (i, x, v)
   TYPE: int * int * int vector -> bool
   PRE: true
   POST: true if x are on multiple positions in v and not 0,
         false otherwise
*)

fun find (i, x, v) =
    let
	val result = Vector.findi (fn (j, y) => y <> 0 andalso i<>j andalso x=y ) v
    in
	if result = NONE then
	    false
	else
	    true
    end;

(* duplicatesExists'' v
   TYPE: int vector -> bool
   PRE: true
   POST: true if an element that is not 0 are on multiple positions in v,
         false otherwise
*)

fun duplicatesExists'' (v) = 
    let
	val result = Vector.mapi (fn (i,x) => find (i,x,v)) v
    in
	Vector.exists (fn x => x = true) result
    end;
	    
(* duplicatesExists' (i,v)
   TYPE: int * int vector vector -> bool
   PRE: true
   POST: true if an element appears more than once in a single vector in v,
         false otherwise
*)
	
fun duplicatesExists' (i,v) = 
    if i = Vector.length v then
	false
    else if duplicatesExists'' (Vector.sub(v, i)) then
	true
    else
	duplicatesExists' (i+1, v);

(* duplicatesExists v
   TYPE: int vector vector -> bool
   PRE: true
   POST: true if an element appears more than once in a single vector in v,
         false otherwise
*)

fun duplicatesExists v = duplicatesExists' (0, v);


(* ====== http://stackoverflow.com/a/5631165/1523238  (converted from lists to vectors)  ====== *)

(* interleave x l
   TYPE: 'a -> 'a list -> 'a list list
   PRE: true
   POST: a list of lists where x is moving one step through l each time
*)

fun interleave x [] = [[x]]
  | interleave x (h::t) =
    (x::h::t)::(List.map(fn l => h::l) (interleave x t))

(* permute' (i, v)
   TYPE: int * 'a vector -> 'a list list
   PRE: i is length of v - 1
   POST: permutations of v
   EXAMPLE: permute' (2, Vector.fromList([1,2,3])) = 
            [[1, 2, 3], [2, 1, 3], [2, 3, 1], [1, 3, 2], [3, 1, 2], [3, 2, 1]] 
*)

fun permute' (i, v) = 
    if i < 0 then
	[[]]
    else
	List.concat( List.map (fn l => interleave (Vector.sub(v, i)) l) (permute' (i-1, v)))

(* permute v
   TYPE: 'a vector -> 'a vector vector
   PRE: true
   POST: permutations of v
   EXAMPLE: permute (Vector.fromList([1,2,3])) = 
            fromList[fromList[3, 2, 1], fromList[2, 3, 1], fromList[2, 1, 3],
            fromList[3, 1, 2], fromList[1, 3, 2], fromList[1, 2, 3]]
*)

fun permute v = 
    Vector.fromList (List.map (fn l => Vector.fromList l) (permute' ((Vector.length v)-1, v)))

(* ======================================================================================== *)


(* notInSquare' (v, n, acc)
   TYPE: int vector * int * int list -> int list
   PRE: true
   POST: numbers 1-9 that are missing in v
*)

fun notInSquare' (_, 0, acc) = acc
  | notInSquare' (v, n, acc) = 
    if (Vector.exists (fn x => x = n) (v))  = false then
	notInSquare'(v, n-1, n::acc)
    else
	notInSquare'(v, n-1, acc)

(* notInSquare (v)
   TYPE: int vector -> int vector
   PRE: true
   POST: numbers 1-9 that are missing in v
*)

fun notInSquare(v) = Vector.fromList(notInSquare'(v, 9, []));

(*
  vectorFilter' f (v, i)
  TYPE: ('a -> bool) -> 'a vector * int -> 'a list
  PRE: true
  POST: v with elements that return false when called to f are removed
*)
fun vectorFilter' _ (_, 0) = []
  | vectorFilter' f (v,i) = 
    if f (Vector.sub(v,i)) then
	(Vector.sub(v,i)) :: vectorFilter' f (v, i-1)
    else
	vectorFilter' f (v, i-1)

(*
  vectorFilter f v
  TYPE: ('a -> bool) -> 'a vector -> 'a vector
  PRE: true
  POST: v with elements that return false when called to f are removed
*)

fun vectorFilter f v = Vector.fromList(vectorFilter' f (v, Vector.length(v)-1))

(*
  squareWithLeastUnknowns' (v, i, acc, n)
  TYPE: int vector vector * int * int vector * int -> int vector * int
  PRE: true
  POST: the first 3x3 square vector in v with the lowest amount of unknown elements,
        and the position of it
*)


fun squareWithLeastUnknowns' (v, i, acc, n) = 
    if i < 0 then
	(acc, n)
    else
	if (Vector.length (vectorFilter (fn x => x = 0) (Vector.sub(v, i))) < 
	    Vector.length (vectorFilter (fn x => x = 0) acc) andalso 
	    (Vector.exists (fn x => x = 0) (Vector.sub(v, i)))) orelse 
	   Vector.length (vectorFilter (fn x => x = 0) acc) = 0  then
	    
	    squareWithLeastUnknowns' (v, i-1, Vector.sub(v, i), i)
	else
	    squareWithLeastUnknowns' (v, i-1, acc, n);

(*
  squareWithLeastUnknowns (v)
  TYPE: int vector vector -> int vector * int
  PRE: true
  POST: the first 3x3 square vector in v with the lowest amount of unknown elements
        along with the position of it in v
*)


fun squareWithLeastUnknowns (v) = squareWithLeastUnknowns'(v, Vector.length(v) -1, Vector.sub(v, 0), 1);


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

(* INSANE *)

(*
val h = [[8,0,0,0,0,0,0,0,0],
	 [0,0,3,6,0,0,0,0,0],
	 [0,7,0,0,9,0,2,0,0],
	 [0,5,0,0,0,7,0,0,0],
	 [0,0,0,0,4,5,7,0,0],
	 [0,0,0,1,0,0,0,3,0],
	 [0,0,1,0,0,0,0,6,8],
	 [0,0,8,5,0,0,0,1,0],
	 [0,9,0,0,0,0,4,0,0]];*)



val h = Vector.fromList(List.map (fn x => Vector.fromList (List.map (fn y => y) x)) h);

val v = verticalHorizontalConverter(h);

val s  = squareHorizontalConverter(h);

val p = Puzzle(h,v,s)

val t = STree (p, []);

fun timee t = (print (Date.toString(Date.fromTimeLocal(Time.now ())) ^ "\n"); traversal t; print (Date.toString(Date.fromTimeLocal(Time.now ()))));



fun diff t = 
    let
	val now = Time.toMilliseconds(Time.now());
    in
	(traversal t; print (Int.toString((Time.toMilliseconds(Time.now())) - now) ^ " milliseconds \n"))
    end
