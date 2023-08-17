-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################
import Test.Hspec

import Board
    ( buildBoard,
      line,
      validateFEN,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos) )
import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves

sampleBoard :: Board
sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
        it "fen has not 9 rows" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
        it "fen has more than 9 rows" $ do
            validateFEN ",,,,,,,,,/,,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
        it "variable c is undefined" $ do
            validateFEN ",,,,c85,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"`shouldBe` (False :: Bool)
        it "figure is not in scope" $ do
            validateFEN ",,,,w277,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"`shouldBe` (False :: Bool)    
        it "some rows consist of more or less elements" $ do
            validateFEN ",,,,,,,,,,/,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"`shouldBe` (False :: Bool)
        it "valid fen" $ do
            validateFEN ",,,,w85,,,,/,,,,w12,,,,/,,,,,,,,/,,,,b170,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"`shouldBe` (True :: Bool)    

testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
        it "build empty board" $ do
            buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
        it "build one piece board" $ do
            buildBoard ",w28,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Piece White 28,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]    

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
        it "start is target" $ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])
        it "start and target" $ do
            line (Pos 'a' 1) (Pos 'b' 1) `shouldBe` ([(Pos 'a' 1), (Pos 'b' 1) ] :: [Pos])
        it "start and target2" $ do
            line (Pos 'a' 1) (Pos 'a' 2) `shouldBe` ([(Pos 'a' 1), (Pos 'a' 2) ] :: [Pos])    
                

testGameFinished :: Spec
testGameFinished = describe "Module Game: gameFinished ..." $ do
        it "start board finished" $ do
            gameFinished sampleBoard `shouldBe` (True :: Bool)
        it "empty board is finished" $ do
            gameFinished [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
        it "empty board is not finished" $ do
            gameFinished [[Empty,Piece White 56,Empty,Empty,Piece White 170,Empty,Empty,Empty,Piece Black 170],[Empty,Empty,Piece Black 56,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (False :: Bool)    

testIsValidMove :: Spec
testIsValidMove = describe "Module Game: isValidMove ..." $ do
        it "rotation by 1 is always possible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)
        it "rotation by bigger than 7 is impossible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 9) `shouldBe` (False :: Bool)
        it "move of shield is possible" $ do
            isValidMove sampleBoard (Move (Pos 'd' 7) (Pos 'd' 6) 1) `shouldBe` (True :: Bool)
        it "move of probe is not possible(same color blocks)" $ do
            isValidMove sampleBoard (Move (Pos 'c' 8) (Pos 'd' 7) 0) `shouldBe` (False :: Bool) 
        it "move of probe is possible" $ do
            isValidMove sampleBoard (Move (Pos 'c' 8) (Pos 'c' 7) 0) `shouldBe` (True :: Bool)       

testPossibleMoves :: Spec
testPossibleMoves = describe "Module Game: possibleMoves ..." $ do
        it "move shield one step" $ do
            possibleMoves (Pos 'd' 3) (Piece White 1) `shouldContain` ([Move (Pos 'd' 3) (Pos 'd' 4) 0] :: [Move])

testListMoves :: Spec
testListMoves = describe "Module Game: listMoves ..." $ do
        it "game finished" $ do
            listMoves sampleBoard Black `shouldBe` ([] :: [Move])
 