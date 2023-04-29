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

# `elm-test` in the project directory will run the tests
# `elm-coverage` in the project directory will check the test coverage

# `elm make src/Sudoku.elm --output=sudoku.js && elm-test && rm -rf .coverage/coverage.html && elm-coverage`

# `elm reactor` from the project root will run the dev webserver