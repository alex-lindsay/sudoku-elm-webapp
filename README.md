# sudoku-elm-webapp
Using ChatGPT to walk through the creation of a Sudoku builder / worker / solver.
Goals:
* See how much it's able to get me ahead of having to code things by hand.
* See how often I need to make corrections / adjustments.
* See how quickly I am able to get to an MVP.
* see how quickly I am able to get features added.

What does MVP look like:
* Build a standard Sudoku grid 9x9 broken into 3x3 subgrids
* Set up a model which will keep track of the following:
    * The mode that the game is in
        * Initially the Mode can be one of: SetKnown, SetGuess, SetMarks
    * Optionally, what cell is currently active
    * The board state which includes the following information for each cell:
        * Optionally: the value of the cell.
        * Whether or not the value of the cell should be displayed.
        * Optionally: the guess for the cell.
        * A list of marks (which can be 1-9) that the user wants to put on the cell.
* Provide an interface by which includes the following:
    * Select a cell in the grid, tracking in the model which cell is active
    * A group of buttons which can be used to set what the current mode of the game is
        * Initially the game should be in SetKnown mode
    * A group of buttons labeled 1-9 which, when clicked, update the model based on the current Mode
        * If the mode is SetKnown then set the value of the active cell to the number.
        * If the mode is SetGuess then set the guess of the active cell to the number.
        * If the mode is SetMarks then add the number to the list of marks for the active cell, if it isn't already in the list. If it is already in the list, then remove it from the list of marks.
    * Add a button after the 1-9 buttons labeled 'X', which when clicked does the following:
        * If the mode is SetKnown then clear the value of the active cell.
        * If the mode is SetGuess then clear the guess of the active cell.
        * If the mode is SetMarks then clear the list of marks for the active cell. 

Okay. At this point, I have some initial instructions from ChatGPT and some code to start with. So I'm going to set up the elm project, add the code, build it and see what errors I get. We'll then go through some back and forth to get something up and running locally.

I've been using GitHub Copilot to see how it does with my process. It's fascinating that it really is generally very good at shortcutting blocks of code, but you have to have the intelligence to know when it's cutting the wrong corners. It's definitely been VERY helpful at writing unit tests as well.

At this point, I am going to start instituting the auto-solver. Assuming that there are 'known values' which have been entered, the autosolver should cycle through a variety of known techniques for puzzle solving. It can calculate the 'auto marks' for all cells and then use the following techniques to mark guesses:

* Singles: Any cells which have only one pencil mark can be labeled as guesses instead.
* Pairs: Any pair of cells in the same row, col, or block with the same two pencil marks can have those marks removed in all other cells in that row, col, or block accordingly.
* Triples: Any triplet of cells in the same row, col, or block with the same collective three pencil marks can have those marks removed in all other cells in that row, col, or block accordingly.
* Hidden Singles: Only one cell in a row, col, or block has a given value. That cell can be marked as a guess.
* Hidden Pairs: Only two cells in a row, col, or block have two given values. All OTHER pencil marks can be removed from those cells.
* Hidden Triples: Only three cells in a row, col, or block have three given values. Hard to spot, *** this will likely fall out from other rules...
* Pointing Pairs: If there are two pencil marks in a given block in a specific row or col, that row or col cannot have that pencil mark in other blocks.
* Pointing Triple: If there are three pencil marks in a given block in a specific row or col, that row or col cannot have that pencil mark in other blocks.
* X-Wings: If a pencil mark appears only twice in two different rows or cols - but in the same col or row (forming a rectangle), then that pencil mark can be removed from other rows or cols accordingly.
* Y-Wings: Tricky - if three cells form a rectangle AB AC BC, then the fourth corner cannot contain the mark common to the adjacent corners.
* Swordfish: Tricky - if a mark appears only twice in three different rows or columns, in a pattern of AB AC BC, then the pencil marks for that number can be removed from others cells in the three rows or rows.

# `elm-test` in the project directory will run the tests
# `elm-coverage` in the project directory will check the test coverage

# `elm make src/Sudoku.elm --output=sudoku.js && elm-test && rm -rf .coverage/coverage.html && elm-coverage`

# `elm reactor` from the project root will run the dev webserver