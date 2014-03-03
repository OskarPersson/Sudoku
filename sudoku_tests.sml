fun tests () = 
    let
	val h = [[3,6,3,0,9,2,0,0,0],
		 [4,0,0,0,3,0,0,1,0],
		 [2,7,0,0,0,0,0,0,0],
		 [0,1,0,3,0,0,0,0,8],
		 [0,5,0,1,6,7,0,3,0],
		 [3,0,0,0,0,8,0,6,0],
		 [0,0,0,0,0,0,0,5,3],
		 [0,3,0,0,8,0,0,0,9],
		 [0,0,0,6,2,0,1,0,0]]

	val h = Vector.fromList(List.map (fn x => Vector.fromList (List.map (fn y => y) x)) h);

	val v = verticalHorizontalConverter(h);

	val s  = squareHorizontalConverter(h);

	val p = Puzzle(h,v,s);

	val t = STree (p, []);

	val h2 = [[1,2,9,4,3,8,7,5,6],
		  [6,8,5,7,2,1,9,4,3],
		  [4,7,3,9,5,6,8,1,2],
		  [2,3,8,5,7,4,6,9,1],
		  [5,6,7,1,9,2,3,8,4],
		  [9,1,4,6,8,3,2,7,5],
		  [7,5,6,2,4,9,1,3,8],
		  [8,9,1,3,6,5,4,2,7],
		  [3,4,2,8,1,7,5,6,9]]

	val v2 = verticalHorizontalConverter(h2);

	val s2  = squareHorizontalConverter(h2);

	val solution = Puzzle(h2,v2,s2); 
    in
	valOf(traversal t) = solution
    end
	
