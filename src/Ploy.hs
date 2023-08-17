module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift, testBit, popCount)
import Data.List.Split
import Data.Char
import Data.List



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################
counterCommanderWhite :: Board -> Bool
counterCommanderWhite board = let list = concat board in let counter1 = length (filter (== (Piece White 170)) list) in let counter2 = length (filter (== (Piece White 85)) list) in
  if (counter1 + counter2) < 1 then True
  else False

counterCommanderBlack :: Board -> Bool
counterCommanderBlack board = let list = concat board in let counter1 = length (filter (== (Piece Black 170)) list) in let counter2 = length (filter (== (Piece Black 85)) list) in
  if (counter1 + counter2) < 1 then True
  else False 

oneCellInteger :: Cell -> Int
oneCellInteger Empty = 0
oneCellInteger (Piece p figure) = figure

add :: Int -> Int -> Int
add x y = x + y

pieceWhite :: Cell -> Int
pieceWhite cell = if cell == (Piece White (oneCellInteger cell)) then 1
else 0

counterPieceWhite :: [Cell] -> Int
counterPieceWhite [] = 0
counterPieceWhite (x:xs) = add (pieceWhite x) (counterPieceWhite xs) 

pieceBlack :: Cell -> Int
pieceBlack cell = if cell == (Piece Black (oneCellInteger cell)) then 1
else 0

counterPieceBlack :: [Cell] -> Int
counterPieceBlack [] = 0
counterPieceBlack (x:xs) =  add (pieceBlack x) (counterPieceBlack xs) 


gameFinished :: Board -> Bool
gameFinished board = let list = concat board in
  if counterPieceWhite list <= 1 then True
  else if counterPieceBlack list <= 1 then True
  else if counterCommanderWhite board == True || counterCommanderBlack board == True then True
  else False





-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################
directionOfFigure :: Move -> String
directionOfFigure (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if ord sc1 == ord tc1 && sr1 < tr1 then "Nord"
else if ord sc1 < ord tc1 && sr1 < tr1 then "NordOst"
else if ord sc1 < ord tc1 && sr1 == tr1 then "Ost"
else if ord sc1 < ord tc1 && sr1 > tr1 then "SüdOst"
else if ord sc1 == ord tc1 && sr1 > tr1 then "Süd"
else if ord sc1 > ord tc1 && sr1 > tr1 then "SüdWest"
else if ord sc1 > ord tc1 && sr1 == tr1 then "West"
else if ord sc1 > ord tc1 && sr1 < tr1 then "NordWest"
else "za"

directionToBit :: String -> Int
directionToBit str = if str == "Nord" then 0
else if str == "NordOst" then 1
else if str == "Ost" then 2
else if str == "SüdOst" then 3
else if str == "Süd" then 4
else if str == "SüdWest" then 5
else if str == "West" then 6
else if str == "NordWest" then 7
else 8

isValidRotateWithoutShield :: Move -> Bool
isValidRotateWithoutShield (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if sc1 == tc1 && sr1 == tr1 && r1 < 8 then
  True
else False

isValidMoveShield :: Board -> Move -> Bool
isValidMoveShield board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if abs (ord sc1 - ord tc1) > 1 || abs (sr1 - tr1) > 1 then False
else let str = directionOfFigure (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) in let bitNumber = directionToBit str in 
  if testBit (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) bitNumber == True then 
    if (board!!(9-tr1))!!boardCharToInt tc1 == Empty then True
    else if (oneCellPlayer((board!!(9-tr1))!!boardCharToInt tc1)) == (oneCellPlayer((board!!(9-sr1))!!boardCharToInt sc1)) then False
    else True
  else False 

boardCharToInt :: Char -> Int 
boardCharToInt z = if z == 'a' then 0
else if z == 'b' then 1
else if z == 'c' then 2
else if z == 'd' then 3
else if z == 'e' then 4
else if z == 'f' then 5
else if z == 'g' then 6
else if z == 'h' then 7
else if z == 'i' then 8
else 9

figureRec :: Cell -> Int 
figureRec (Piece p figure) = popCount figure

occupationControl :: Cell -> Bool
occupationControl Empty = True
occupationControl (Piece p figure) = False

oneCellPlayer :: Cell -> Player
oneCellPlayer (Piece p figure) = p

posToRow :: Pos -> Int
posToRow (Pos c r) = r

posToColumn :: Pos -> Char
posToColumn (Pos c r) = c

posToCell :: Board -> [Pos] -> [Cell]
posToCell board [] = []
posToCell board (x:xs) = ((board!!(9-(posToRow x)))!!boardCharToInt(posToColumn x)) : posToCell board xs

isValidMoveCommander :: Board -> Move -> Bool
isValidMoveCommander board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if r1 > 0 then isValidRotateWithoutShield (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if abs (ord sc1 - ord tc1) > 1 || abs (sr1 - tr1) > 1 then False
else let str = directionOfFigure (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) in let bitNumber = directionToBit str in 
  if testBit (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) bitNumber == True then 
    if (board!!(9-tr1))!!boardCharToInt tc1 == Empty then True
    else if (oneCellPlayer((board!!(9-tr1))!!boardCharToInt tc1)) == (oneCellPlayer((board!!(9-sr1))!!boardCharToInt sc1)) then False
    else True
  else False 

isValidMoveProbe :: Board -> Move -> Bool
isValidMoveProbe board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if r1 > 0 then isValidRotateWithoutShield (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if abs (ord sc1 - ord tc1) > 2 || abs (sr1 - tr1) > 2 then False
else let str = directionOfFigure (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) in let bitNumber = directionToBit str in let line1 = line (Pos sc1 sr1) (Pos tc1 tr1) in let line2 = delete (Pos sc1 sr1) line1 in let line3 = delete (Pos tc1 tr1) line2 in let line4 = posToCell board line3 in
  if testBit (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) bitNumber == True then 
    if all occupationControl line4 then 
      if (board!!(9-tr1))!!boardCharToInt tc1 == Empty then True
      else if (oneCellPlayer((board!!(9-tr1))!!boardCharToInt tc1)) == (oneCellPlayer((board!!(9-sr1))!!boardCharToInt sc1)) then False
      else True
    else False  
  else False 

isValidMoveLance :: Board -> Move -> Bool
isValidMoveLance board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if r1 > 0 then isValidRotateWithoutShield (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if abs (ord sc1 - ord tc1) > 3 || abs (sr1 - tr1) > 3 then False
else let str = directionOfFigure (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) in let bitNumber = directionToBit str in let line1 = line (Pos sc1 sr1) (Pos tc1 tr1) in let line2 = delete (Pos sc1 sr1) line1 in let line3 = delete (Pos tc1 tr1) line2 in let line4 = posToCell board line3 in
  if testBit (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) bitNumber == True then 
    if all occupationControl line4 then 
      if (board!!(9-tr1))!!boardCharToInt tc1 == Empty then True
      else if (oneCellPlayer((board!!(9-tr1))!!boardCharToInt tc1)) == (oneCellPlayer((board!!(9-sr1))!!boardCharToInt sc1)) then False
      else True
    else False  
  else False 

isValidMove :: Board -> Move -> Bool
isValidMove board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) = if popCount (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) == 1 then isValidMoveShield board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if popCount (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) == 2 then isValidMoveProbe board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if popCount (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) == 3 then isValidMoveLance board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1)
else if popCount (oneCellInteger((board!!(9-sr1))!!boardCharToInt sc1)) == 4 then isValidMoveCommander board (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) 
else False


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves _ _ = []



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves _ _ = []