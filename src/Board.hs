module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars
import Data.List.Split
import Data.Char




-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################
dizi :: [String]
dizi = map show [1..255]

isElement :: String -> Bool
isElement [] = True
--isElement str = (head [str] == "w" || head [str] == "b") &&  tail [str] `elem` [dizi]
isElement (x:xs) = ((x == 'w' || x == 'b') && (xs `elem` dizi))

isRow :: String -> Bool 
isRow [] = False
isRow str = let roW = splitWhen (== ',') str in all isElement roW

comma :: String -> Bool 
comma str = let number = length (filter (== ',') str) in number == 8

isRowFen :: String -> Bool
isRowFen [] = False
isRowFen str = let fen = splitWhen (== '/') str in 
  all isRow fen && all comma fen

slash :: String -> Bool
slash str = let number = length (filter (== '/') str) in number == 8

validateFEN :: String -> Bool
validateFEN [] = False
validateFEN str = all isRowFen [str] && all slash [str]






-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################
charToPlayer :: Char -> Player
charToPlayer char = if char == 'w' then White else Black

stringToInt :: String -> Int 
stringToInt str = read str :: Int 

oneCell :: String -> Cell
oneCell [] = Empty
oneCell (x:xs) = let p = charToPlayer x in let figure = stringToInt xs in (Piece p figure) 

cellToRow :: String -> [Cell]
cellToRow str = let roW = splitWhen (== ',') str in map oneCell roW

buildBoard :: String -> Board
buildBoard str = let column = splitWhen (== '/') str in map cellToRow column



-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line (Pos ac1 dr1) (Pos ac2 dr2) = if dr1 == dr2 then
  if ord ac1 > ord ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 - 1)) dr1) (Pos ac2 dr2)
  else if ord ac1 < ord ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 + 1)) dr1) (Pos ac2 dr2)
  else [(Pos ac1 dr1)]
else if dr1 > dr2 && ac1 == ac2 then (Pos ac1 dr1) : line (Pos ac1 (dr1 -1)) (Pos ac2 dr2)
else if dr1 < dr2 && ac1 == ac2 then (Pos ac1 dr1) : line (Pos ac1 (dr1 +1)) (Pos ac2 dr2)
else if dr1 > dr2 && ac1 > ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 -1)) (dr1 -1)) (Pos ac2 dr2)
else if dr1 > dr2 && ac1 < ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 +1)) (dr1 -1)) (Pos ac2 dr2)
else if dr1 < dr2 && ac1 > ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 -1)) (dr1 +1)) (Pos ac2 dr2)
else if dr1 < dr2 && ac1 < ac2 then (Pos ac1 dr1) : line (Pos (chr(ord ac1 +1)) (dr1 +1)) (Pos ac2 dr2)
else [] 