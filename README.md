Sudoku
======

User guidelines
Requirements: PolyML
Instructions: In order to solve the sudoku shown in figure 1, using our application, you must give the program the following input, in the terminal:
> cd /path/to/folder/containing/application
> poly
> use “sudoku.sml”;
> val h = [[8,0,0,4,0,6,0,0,7],
           [0,0,0,0,0,0,4,0,0],
	     [0,1,0,0,0,0,6,5,0],
           [5,0,9,0,3,0,7,8,0],
           [0,0,0,0,7,0,0,0,0],
	     [0,4,8,0,2,0,1,0,3],
           [0,5,2,0,0,0,0,9,0],
           [0,0,1,0,0,0,0,0,0],
           [3,0,0,9,0,2,0,0,5]];
> val s = createPuzzleFromHorizontal h;
> solve s;
Running test-cases: In order to run the test-cases you must give the following input, in the terminal:
> cd /path/to/folder/containing/application
> poly
> use “sudoku.sml”;
> use “sudoku_tests.sml”;
> tests ();
