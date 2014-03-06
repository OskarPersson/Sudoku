(* Medium difficulty*)
val h = [[8,0,0,4,0,6,0,0,7],
	 [0,0,0,0,0,0,4,0,0],
	 [0,1,0,0,0,0,6,5,0],
	 [5,0,9,0,3,0,7,8,0],
	 [0,0,0,0,7,0,0,0,0],
	 [0,4,8,0,2,0,1,0,3],
	 [0,5,2,0,0,0,0,9,0],
	 [0,0,1,0,0,0,0,0,0],
	 [3,0,0,9,0,2,0,0,5]]
(* Very hard difficulty *)
val exh = [[0,7,4,0,0,0,0,9,3],
	   [0,0,5,9,0,0,0,0,4],
	   [0,0,0,0,0,2,0,0,0],
	   [0,2,0,1,0,0,0,0,7],
	   [0,0,8,0,0,0,4,0,0],
	   [1,0,0,0,0,3,0,2,0],
	   [0,0,0,6,0,0,0,0,0],
	   [9,0,0,0,0,7,8,0,0],
	   [5,8,0,0,0,0,1,6,0]]

val v = verticalHorizontalConverter(h);

val exv = verticalHorizontalConverter(exh);

val exs = squareHorizontalConverter(exh);

val s  = squareHorizontalConverter(h);

val p = Puzzle(h,v,s);

val pex = Puzzle(exh, exv, exs)

val texh = STree (pex, [])

val t = STree (p, []);

(* tests
   TYPE: unit -> unit
   PRE: true
   POST: (none)
   SIDE-EFFECTS: Prints the results of 3 test cases
*)

fun tests () = 
    let
	val h2 = [[8,3,5,4,1,6,9,2,7],
		  [2,9,6,8,5,7,4,3,1],
		  [4,1,7,2,9,3,6,5,8],
		  [5,6,9,1,3,4,7,8,2],
		  [1,2,3,6,7,8,5,4,9],
		  [7,4,8,5,2,9,1,6,3],
		  [6,5,2,7,8,1,3,9,4],
		  [9,8,1,3,4,5,2,7,6],
		  [3,7,4,9,6,2,8,1,5]]

	val v2 = verticalHorizontalConverter(h2);

	val s2  = squareHorizontalConverter(h2);

	val exh2 = [[2,7,4,8,5,1,6,9,3],
		    [8,3,5,9,7,6,2,1,4],
		    [6,9,1,4,3,2,7,5,8],
		    [3,2,6,1,4,5,9,8,7],
		    [7,5,8,2,6,9,4,3,1],
		    [1,4,9,7,8,3,5,2,6],
		    [4,1,2,6,9,8,3,7,5],
		    [9,6,3,5,1,7,8,4,2],
		    [5,8,7,3,2,4,1,6,9]]

	val exv2 = verticalHorizontalConverter(exh2);

	val exs2  = squareHorizontalConverter(exh2);

	val solution1 = Puzzle(h2,v2,s2);
	
	val solution2 = Puzzle(exh2, exv2, exs2);
	
	val square = ([0,0,7,4,0,0,6,5,0],3);

	val results = ""
	
	(* Tests if the solution to p is correct*)
	val results = results ^ 
		      (if valOf(traversal t) = solution1 then
			   "Test 1: Successful!"
		       else
			   "Test 1: Failed"
		      ) ^ "\n"
	(* Tests if the solution to pex is correct*)
	val results = results ^ 
		      (if valOf(traversal texh) = solution2 then
			   "Test 2: Successful!"
		       else
			   "Test 2: Failed"
		      ) ^ "\n"


	(* Tests whether the function returns the correct square*)
	val results = results ^ 
		      (if squareWithLeastUnknowns s = square then
			   "Test 3: Successful!"
		       else
			   "Test 3: Failed"
		      ) ^ "\n"
    in
	print results
    end;
