{-# OPTIONS -Wall #-}
{-# LANGUAGE ParallelListComp #-}
-- Daniel shaanan
-- 313622268
-- Gal Kolberg
-- 206097925

module MineSweeper
    ( 
      Action,
      Board,
      Col,
      Row,
     createNewMineSweeper,
     typeControl,
     getUpdatedHiddenValsBoard,
     doAction,
     actionValidate,
     toAction,
     showBoard,
     getCols,
     getRows,
     getState,
     typeControlHelper,
     changeStateToWin
    ) where

import Data.Maybe
import Text.Read
import Data.Char 
import Data.List
import Prelude

data Action = Dig Col Row | Flag Col Row
 deriving (Read)

data Cell = Cell { col :: Int, row :: Int, hidden_val :: Char , showen_val :: Char }
 deriving (Show,Eq)

 -- 0 = Lose , 1 = Win , 2 = Playing 
data Board = Board { cell :: [[Cell]], number_of_cols :: Int, number_of_rows :: Int, numOfMines :: Int, numOfFlags::Int , state::Int}
 deriving (Show,Eq)

type Col = Int
type Row = Int
type Index = Int
type Mines = Int
type ShownStr = [Char]
type HiddenStr = [Char]

getState :: Board -> Int
getState (Board _ _ _ _ _ stat) = stat

getCols :: Board -> Col
getCols (Board _ cols _ _ _ _) = cols

getRows :: Board -> Row
getRows (Board _ _ rows _ _ _) = rows

toAction :: [String] -> Action
toAction list 
 | act == "Flag" = Flag intCol intRow
 | otherwise = Dig intCol intRow
 where intCol =  read col' :: Int
       intRow = read row' :: Int
       act = list !! 0
       col' = list !! 1
       row' = list !! 2

actionValidate :: [String] -> Col -> Row -> Bool
actionValidate inputList numCols numRows 
 | (isAction act) && (isInRange (givenCol,givenRow)) == True = True
 | otherwise = False
 where act = inputList !! 0
       givenCol = typeControlHelper (readMaybe (inputList !! 1) :: Maybe Int) 
       givenRow = typeControlHelper (readMaybe (inputList !! 2) :: Maybe Int)
       isAction = (\act' -> act' == "Flag" || act' == "Dig")
       isInRange = (\(_col,_row) -> _col > 0 && _col <= numCols && _row > 0 && _row <= numRows )

typeControl :: [Maybe Int] -> Bool
typeControl list  = (isValidCols && isValidRows && isValidMines)
 where isValidCols = validCols valCol
       isValidRows = validRows valRow 
       isValidMines = validMines valCol valRow valMines 
       valCol = typeControlHelper (list !! 0)
       valRow = typeControlHelper (list !! 1)
       valMines = typeControlHelper (list !! 2)

typeControlHelper :: Maybe Int -> Int
typeControlHelper maybeNum
 | maybeNum == Nothing = 0
 | otherwise = (fromJust maybeNum)

validCols :: Col -> Bool
validCols cols 
 | cols <= 20 && cols >= 10 = True
 | otherwise = False

validRows :: Row -> Bool
validRows rows 
 | rows <= 20 && rows >= 10 = True
 | otherwise = False
  
validMines :: Col -> Row-> Mines -> Bool
validMines cols rows mines
 | (mines < cols * rows) && mines > 0 = True
 | otherwise = False

createNewMineSweeper:: Col -> Row -> Mines -> Board
createNewMineSweeper num_cols num_rows mines = Board (make2Darr [] [] num_cols num_rows mines) num_cols num_rows mines 0 2

make2Darr :: HiddenStr -> ShownStr -> Col -> Row -> Mines -> [[Cell]]
make2Darr boardHiddenString boardShownString num_cols num_rows mines = [makeRow row_index num_cols hiddenlist shownlist| row_index <- [1..num_rows]]
 where hiddenlist = hiddenStringToMakeNewArr boardHiddenString num_cols num_rows mines
       shownlist = shownStringToMakeNewArr boardShownString num_cols num_rows

shownStringToMakeNewArr :: ShownStr -> Col -> Row -> ShownStr
shownStringToMakeNewArr string num_cols num_rows
 | string == [] = take (num_cols * num_rows) (cycle [' '])
 | otherwise = string

hiddenStringToMakeNewArr :: HiddenStr -> Col -> Row -> Mines -> HiddenStr
hiddenStringToMakeNewArr string num_cols num_rows mines 
 | string == [] = shuffleList mines  (makeRandomList (num_cols+num_rows `mod` mines) (num_cols * num_rows)) (spaces_list) (mine_list)
 | otherwise = string
 where spaces_list = take (num_cols * num_rows - mines) (cycle [' ']);
       mine_list = take mines (cycle ['*'])

makeRow :: Col -> Row -> HiddenStr -> ShownStr -> [Cell]
makeRow row_index num_cols shuffled_List shownList = [(Cell col_val row_index hidden shown) | col_val <- [1..num_cols] | hidden <- b | shown <- d]
 where (_,b) = splitAt ((row_index - 1) * num_cols) shuffled_List
       (_,d) = splitAt ((row_index - 1) * num_cols) shownList

myLookUp :: Col -> Row -> Board -> Char
myLookUp _ _ (Board [] _ _ _ _ _) = 'X'
myLookUp col_val row_val (Board (cell':cells) num_cols num_rows m f s) 
 | col_val > num_cols || row_val > num_rows || col_val < 1 || row_val < 1 = 'X'
 | row_val > getRow (head cell') = myLookUp col_val row_val (Board (cells) num_cols num_rows m f s)
 | otherwise = myLookUpHelper col_val row_val cell' 

myLookUpHelper :: Col -> Row -> [Cell] -> Char
myLookUpHelper _ _ [] = 'X'
myLookUpHelper col_val row_val (cl:cells) 
 | col_val > (getCol cl) = myLookUpHelper col_val row_val cells
 | otherwise = (getHiddenVal cl)

myLookUpShown :: Col -> Row -> Board -> Char
myLookUpShown _ _ (Board [] _ _ _ _ _) = 'X'
myLookUpShown col_val row_val (Board (cell':cells) num_cols num_rows m f s) 
 | col_val > num_cols || row_val > num_rows || col_val < 1 || row_val < 1 = 'X'
 | row_val > getRow (head cell') = myLookUpShown col_val row_val (Board (cells) num_cols num_rows m f s)
 | otherwise = myLookUpHelperShown col_val row_val cell'
 
myLookUpHelperShown :: Col -> Row -> [Cell] -> Char
myLookUpHelperShown _ _ [] = 'X'
myLookUpHelperShown col_val row_val (cl:cells) 
 | col_val > (getCol cl) = myLookUpHelperShown col_val row_val cells
 | otherwise = (getShownVal cl)

doAction :: Action -> Board -> Board 
doAction (Dig col' row') (Board cells num_cols num_rows mines flags state')
 | cellShownval == ' ' = digInBoard col' row' (Board cells num_cols num_rows mines flags state')
 | otherwise = (Board cells num_cols num_rows mines flags state')
   where cellShownval = myLookUpShown col' row' board;
         board = (Board cells num_cols num_rows mines flags state');
doAction (Flag col' row') (Board cells num_cols num_rows mines flags state') 
 | cellShownval == '!' = (Board (make2Darr hiddenStr newShownStr2 num_cols num_rows mines) num_cols num_rows mines flags state')
 | (isNumber' cellShownval) == True = board
 | otherwise = (Board (make2Darr hiddenStr newShownStr1 num_cols num_rows mines) num_cols num_rows mines flags state')
   where index =  ((num_cols * (row'-1)) + col');
         cellShownval = myLookUpShown col' row' board; 
         hiddenStr = getHiddenStr (cell board)
         newShownStr1 =  insertToCurrShownStr '!' index (getShownStr (cell board))
         newShownStr2 =  insertToCurrShownStr ' ' index (getShownStr (cell board))
         board = (Board cells num_cols num_rows mines flags state')
         isNumber' = (\ch -> ch == '0' || ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');

changeStateToWin :: Board -> Board 
changeStateToWin (Board cells num_cols num_rows mines flags state')
 | hasWon == True = (Board cells num_cols num_rows mines flags 1)
 | otherwise = (Board cells num_cols num_rows mines flags state')
 where hasWon = isGameWon (Board cells num_cols num_rows mines flags state')

isGameWon :: Board -> Bool
isGameWon (Board cells num_cols num_rows mines _ _)
 | (num_cols * num_rows) - mines == numOfNumbers = True
 | otherwise = False
 where shownStr = getShownStr cells;
       isNumber' = (\ch -> ch == '0' || ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');
       numOfNumbers = length $ filter isNumber' shownStr;

showMines :: HiddenStr -> ShownStr -> ShownStr
showMines [] _ = []
showMines _ [] = []
showMines (hid:hiddenStr) (shown:shownStr) 
 | hid == '*' = ['*'] ++  showMines hiddenStr shownStr
 | otherwise = [shown] ++ showMines hiddenStr shownStr

findAllZeroAndEdgeLeft :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndEdgeLeft col' row' numCol numRow board 
 | myLookUp (col'-1) row' board == '0' = [(col'-1,row')] ++ findAllZeroAndEdgeLeft (col'-1) row' numCol numRow board 
 | isNumber' (myLookUp (col'-1) row' board) == True = [(col'-1,row')]
 | otherwise = []
 where isNumber' = (\ch -> ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');

findAllZeroAndEdgeRight :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndEdgeRight col' row' numCol numRow board 
 | myLookUp (col'+1) row' board == '0' = [(col'+1,row')] ++ findAllZeroAndEdgeRight (col'+1) row' numCol numRow board 
 | isNumber' (myLookUp (col'+1) row' board) == True = [(col'+1,row')]
 | otherwise = []
 where isNumber' = (\ch -> ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');

findAllZeroAndEdgeUp :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndEdgeUp col' row' numCol numRow board 
 | myLookUp col' (row'-1) board == '0' = [(col',row'-1)] ++ findAllZeroAndEdgeUp col' (row'-1) numCol numRow board 
 | isNumber' (myLookUp col' (row'-1) board) == True = [(col',row'-1)]
 | otherwise = []
 where isNumber' = (\ch -> ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');

findAllZeroAndEdgeDown :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndEdgeDown col' row' numCol numRow board 
 | myLookUp col' (row'+1) board == '0' = [(col',row'+1)] ++ findAllZeroAndEdgeDown col' (row'+1) numCol numRow board 
 | isNumber' (myLookUp col' (row'+1) board) == True = [(col',row'+1)]
 | otherwise = []
 where isNumber' = (\ch -> ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');
 
findAllZeroAndEdgeLeftRightFinal :: [(Col,Row)] -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndEdgeLeftRightFinal []  _ _ _ = []
findAllZeroAndEdgeLeftRightFinal (first:list) numCol numRow board = edgeLeft ++ edgeRight ++ findAllZeroAndEdgeLeftRightFinal list numCol numRow board
    where (col',row') = first; 
          edgeLeft = findAllZeroAndEdgeLeft col' row' numCol numRow board;
          edgeRight = findAllZeroAndEdgeRight col' row' numCol numRow board;

findAllZero :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZero col' row' numCol numRow board = up ++ down ++ left ++ right ++ finalUp ++ finalDown
 where up =  (findAllZeroAndEdgeUp col' row' numCol numRow board)
       down = (findAllZeroAndEdgeDown col' row' numCol numRow board)
       left = (findAllZeroAndEdgeLeft col' row' numCol numRow board)
       right = (findAllZeroAndEdgeRight col' row' numCol numRow board)
       finalUp = (findAllZeroAndEdgeLeftRightFinal up numCol numRow board)
       finalDown = (findAllZeroAndEdgeLeftRightFinal down numCol numRow board)

findAlllZeroRec :: [(Col,Row)] -> Col -> Row -> Board -> [(Col,Row)]
findAlllZeroRec []  _ _ _ = []
findAlllZeroRec (first:list) numCol numRow board = findAllZero col' row' numCol numRow board ++ findAlllZeroRec list numCol numRow board
 where (col',row') = first; 

findAllEdgesV2 :: [(Col,Row)] -> Col -> Row -> Board -> [(Col,Row)]
findAllEdgesV2 list numCol numRow board = filter hasMineVal $ nub $ findAllEdgesV3 (filter hasVal0 list) numCol numRow
 where hasVal0 = (\(col',row') -> (myLookUp col' row' board) == '0')
       hasMineVal = (\(col',row') -> (myLookUp col' row' board) /= '*' || (myLookUpShown col' row' board) /= '!')

findAllEdgesV3 :: [(Col,Row)] -> Col -> Row -> [(Col,Row)]
findAllEdgesV3 [] _ _ = []
findAllEdgesV3 ((a,b):val0) numCol numRow  = (listAllNeighbors a b numCol numRow ) ++ findAllEdgesV3 val0 numCol numRow 


findAllZeroAndAroundComplete :: Col -> Row -> Col -> Row -> Board -> [(Col,Row)]
findAllZeroAndAroundComplete col' row' numCol numRow board = findAllEdgesV2 (findAlllZeroRec initList numCol numRow board) numCol numRow board
 where initList =  findAllZero col' row' numCol numRow board

fromListOfRowCol2ShownStr :: ShownStr -> [(Col,Row)] -> Board -> ShownStr
fromListOfRowCol2ShownStr shownstr [] (Board _ _ _ _ _ _) = shownstr
fromListOfRowCol2ShownStr shownstr (elem':colrowList) (Board cells num_cols num_rows mines flags state') = fromListOfRowCol2ShownStr (insertToCurrShownStr cellhiddenval index shownstr) colrowList board
 where (col',row') = elem';
       board = (Board cells num_cols num_rows mines flags state');
       index =  ((num_cols * (row'-1)) + col');
       cellhiddenval = myLookUp col' row' board;

digInBoard :: Col -> Row -> Board -> Board
digInBoard col' row' (Board cells num_cols num_rows mines flags state')
 | myLookUp col' row' board == '0' = (Board (make2Darr hiddenStr showStrForZeroVal num_cols num_rows mines) num_cols num_rows mines flags state')
 | isNumber' (myLookUp col' row' board) == True = (Board (make2Darr hiddenStr shownStr num_cols num_rows mines) num_cols num_rows mines flags state')
 | myLookUp col' row' board == '*' = (Board (make2Darr hiddenStr (showMines hiddenStr shownStr) num_cols num_rows mines)  num_cols num_rows mines flags 0)
 | otherwise = board
 where hiddenStr = (getHiddenStr cells);
       shownStr = (insertToCurrShownStr cellhiddenval index (getShownStr cells));
       board = (Board cells num_cols num_rows mines flags state');
       cellhiddenval = myLookUp col' row' board; 
       index = ((num_cols * (row'-1)) + col');
       isNumber' = (\ch -> ch == '1' ||ch == '2' || ch == '3' || ch == '4' || ch == '5' ||  ch == '6'|| ch == '7' || ch == '8');
       showStrForZeroVal = (fromListOfRowCol2ShownStr  shownStr (findAllZeroAndAroundComplete col' row' num_cols num_rows board) board);

insertToCurrShownStr :: Char -> Index -> ShownStr -> ShownStr
insertToCurrShownStr ch index shownStr = (init a) ++ [ch] ++ b
    where (a,b) = splitAt index shownStr

getShownStr :: [[Cell]] -> ShownStr
getShownStr [] = []
getShownStr  (cellArr:cellMat) = getShownStrHelper cellArr ++ getShownStr cellMat

getShownStrHelper :: [Cell] -> ShownStr
getShownStrHelper [] = []
getShownStrHelper (cell':cellArr) = [(getShownVal cell')] ++ getShownStrHelper cellArr

getHiddenStr :: [[Cell]] -> HiddenStr
getHiddenStr [] = []
getHiddenStr  (cellArr:cellMat) = getHiddenStrHelper cellArr ++ getHiddenStr cellMat

getHiddenStrHelper :: [Cell] -> HiddenStr
getHiddenStrHelper [] = []
getHiddenStrHelper (cell':cellArr) = [(getHiddenVal cell')] ++ getHiddenStrHelper cellArr

getUpdatedHiddenValsBoard :: Board -> Board
getUpdatedHiddenValsBoard (Board cell' cols rows mines' flags state') = (Board (make2Darr (initHiddenValues b1 rows) [] cols rows mines') cols rows mines' flags state')
 where b1 = (Board cell' cols rows mines' flags state')


initHiddenValues :: Board -> Int -> [Char]
initHiddenValues (Board [] _ _ _ _ _) 0 = []
initHiddenValues (Board cells num_cols num_rows mines' flag' state') times = (makeHiddenValuesRow cellArr num_cols num_rows b1) ++ initHiddenValues (Board cells num_cols num_rows mines' flag' state') (times -1) 
 where b1 = (Board cells num_cols num_rows mines' flag' state')
       cellArr = cells !! (num_rows - times)

makeHiddenValuesRow :: [Cell] -> Col -> Row -> Board -> [Char]
makeHiddenValuesRow [] _ _ _ = []
makeHiddenValuesRow (cell':cellsArr) num_cols num_rows board
    | getHiddenVal cell' == '*' = ['*'] ++ makeHiddenValuesRow cellsArr num_cols num_rows board
    | otherwise = show (numOfSurroundMines (getCol cell') (getRow cell') board) ++ makeHiddenValuesRow cellsArr num_cols num_rows board

numOfSurroundMines :: Int -> Int -> Board -> Int
numOfSurroundMines col_index row_index (Board cells num_of_cols num_of_rows mines flags state') = countNumberOfMines (findValOfNeighbors neighborList b1)
 where b1 = (Board cells num_of_cols num_of_rows mines flags state')
       neighborList = (listAllNeighbors col_index row_index num_of_cols num_of_rows)

countNumberOfMines :: [Char] -> Int
countNumberOfMines list = length $ filter (=='*') list

findValOfNeighbors :: [(Int,Int)] -> Board -> [Char]
findValOfNeighbors [] (Board _ _ _ _ _ _) = []
findValOfNeighbors (neigb:neighbors) (Board cells num_of_cols num_of_rows m f s) = [myLookUp (fst neigb) (snd neigb) (Board cells num_of_cols num_of_rows m f s)] ++ findValOfNeighbors neighbors (Board cells num_of_cols num_of_rows m f s)

listAllNeighbors :: Col -> Row -> Col -> Row -> [(Col, Row)] 
listAllNeighbors col_index row_index num_of_cols num_of_rows = filter isOnboard [(col_index - 1, row_index - 1), (col_index - 1, row_index), (col_index-1,row_index + 1), (col_index, row_index - 1), (col_index, row_index + 1), (col_index + 1, row_index - 1), (col_index + 1, row_index),(col_index + 1, row_index + 1)]
 where isOnboard = (\(col_index',row_index') -> col_index' > 0 && col_index' <= num_of_cols && row_index' > 0 && row_index' <= num_of_rows)

makeRandomList :: Int -> Int -> [Int]
makeRandomList _ 0 = []
makeRandomList num times = [(((num * 214013) + 2531011 ) `mod` 4363)] ++ makeRandomList (num+17) (times - 1)

shuffleList :: Int -> [Int] -> [Char] ->[Char] -> [Char]
shuffleList _ _ _ [] = []
shuffleList _ _ [] _  = []
shuffleList _ [] _  _  = []
shuffleList 1 (rand:_) spaces (m:_) = fst (splitAtRandom rand spaces) ++ [m] ++ snd (splitAtRandom rand spaces)
shuffleList num (rand:randomList) spaces (m:mines) = (shuffleList (num - 1) randomList (a ++ [m] ++ b) mines)
 where (a,b) = splitAt (rand `mod` length spaces) spaces

splitAtRandom :: Int -> [Char] -> ([Char],[Char])
splitAtRandom rand spaces = splitAt (rand `mod` length spaces) spaces

getRow :: Cell -> Int
getRow (Cell _ row_val _ _) = row_val

getCol :: Cell -> Int
getCol (Cell col_val _ _ _) = col_val

getHiddenVal :: Cell -> Char
getHiddenVal (Cell _ _ h_val _) = h_val

getShownVal :: Cell -> Char
getShownVal (Cell _ _ _ s_val) = s_val

makeFstLine :: [Char] -> [Char]
makeFstLine [] = []
makeFstLine (x:xs) = "00" ++ [x] ++ " " ++ makeFstLine xs

makeFstLine2 :: Int -> [Char]
makeFstLine2 9 = []
makeFstLine2 num =  "0" ++ show num ++ " " ++ (makeFstLine2 (num - 1))

makeTopLine :: Int -> [Char]
makeTopLine num = "   " ++ makeFstLine x ++ unwords(reverse (words (makeFstLine2 num)))  ++ "\n"
 where x = [a | a <- ['1'..'9']]

showCell :: Cell -> [Char]
showCell (Cell _ _ _ shown_val) = [shown_val]

showCellArr :: [Cell] -> [Char]
showCellArr [] = "\n"
showCellArr (x:xs) = "[" ++ (showCell x) ++ "]" ++ " " ++ (showCellArr xs)

showCell2dArr :: Int -> [[Cell]] -> [Char]
showCell2dArr _ [] = []
showCell2dArr start (val:xs) = take (3 - (digLen start))(cycle ['0']) ++ show start ++ showCellArr val ++ showCell2dArr (start + 1) xs

showBoard :: Board -> [Char]
showBoard (Board cells cols _ _ _ _) = makeTopLine cols ++ showCell2dArr 1 cells

digLen :: Int -> Int
digLen n = length.show $ n