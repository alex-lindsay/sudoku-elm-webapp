module AutosolversTest exposing (..)

import Autosolvers exposing (..)
import Constants exposing (..)
import Expect exposing (..)
import Test exposing (..)
import Updaters exposing (..)

fullHouseTestStringFalse : String
fullHouseTestStringFalse =
    "003456789000000000000000000000000000000000000000000000000000000000000000000000000"


fullHouseTestStringTrue : String
fullHouseTestStringTrue =
    "023456789000000000000000000000000000000000000000000000000000000000000000000000000"


selectedCellIsFullHouseTest : Test
selectedCellIsFullHouseTest =
    let
        fullHouseTestModelFalse = { defaultModel |
            cells = fillBoardValues fullHouseTestStringFalse emptyBoard,
            selectedPos = (1,1)
            }
        
        fullHouseTestModelTrue = { defaultModel |
            cells = fillBoardValues fullHouseTestStringTrue emptyBoard,
            selectedPos = (1,1)
            }

    in
    describe "selectedCellIsFullHouse"
        [ test "returns false when the selected cell is not a full house" 
            (\_ -> Expect.equal False (selectedCellIsFullHouse fullHouseTestModelFalse))

        , test "returns true when the selected cell is a full house" 
            (\_ -> Expect.equal True (selectedCellIsFullHouse fullHouseTestModelTrue))

        ]