{-# OPTIONS -Wall #-}
{-# LANGUAGE ParallelListComp #-}
-- Daniel shaanan
-- 313622268
-- Gal Kolberg
-- 206097925

module Main where
import Prelude
import MineSweeper
import System.Exit 
import Text.Read

getUserAction :: Col -> Row -> IO Action
getUserAction numCols numRows = do
    userInput <- getLine
    if length (words userInput) /= 3
        then do
            putStrLn "Invalid number of parameters were given"
            getUserAction numCols numRows
            else
                if(actionValidate ((words userInput)) numCols numRows)
                    then return (toAction ((words userInput)))
                    else do
                        putStrLn "Invalid parameters were given, please try again"
                        getUserAction numCols numRows

mainGameLoop ::Board -> IO Bool
mainGameLoop board = do
    putStr (showBoard board)
    putStrLn "what is your next move?"
    act <- getUserAction (getCols board) (getRows board)
    if isPlaying (changeStateToWin (doAction act board))
        then mainGameLoop (doAction act board)
            else
                if((getState (changeStateToWin (doAction act board))) == 1)
                    then do
                        putStr $ showBoard (doAction act board) 
                        putStrLn "you win! all mines cleared"
                        return (False)
                    else do
                        putStr $ showBoard (doAction act board)
                        putStrLn "BOOM! game is over"
                        return (False)
 where isPlaying = (\board' -> (getState board') == 2 )

getInitVals :: IO [String]
getInitVals = do{
    input <- getLine;
    return (words input);
}   

main :: IO ()
main = do
   putStrLn "Enter number of colums and number of rows and number of mines to start the game: (10 <= cols and rows <= 20)"
   initVal <- getInitVals
   if (length initVal /= 3)
        then do
            putStrLn "Invalid number of parameters were given";
            exitFailure;
            else 
                if (typeControl [(readMaybe (initVal !! 0) ::Maybe Int),(readMaybe (initVal !! 1) ::Maybe Int),(readMaybe (initVal !! 2) ::Maybe Int)])
                    then do
                       stam <- mainGameLoop (getUpdatedHiddenValsBoard (createNewMineSweeper (typeControlHelper (readMaybe (initVal !! 0) ::Maybe Int)) (typeControlHelper (readMaybe (initVal !! 1) ::Maybe Int)) (typeControlHelper (readMaybe (initVal !! 2) ::Maybe Int))));
                       putStr (take 0 (show stam)) -- only for warning reasons
                       exitSuccess;
                    else do
                        putStrLn "Invalid parameters were given"
                        exitFailure;