module TwoPlayerMancala where

import MancalaBoard
import Data.List
import Data.Maybe

main :: IO ()
main = do
    welcome
    howToPlay
    putStrLn "---------Game Start---------"
    playGame PlayerA initial

welcome :: IO ()
welcome = do
    putStrLn "---------Welcome to Mancala!---------"
    putStrLn "This game requires two human players."
howToPlay :: IO ()
howToPlay = do
    putStrLn ""
    putStrLn "To make a move, type in the index (0-5) of the pit."
    putStrLn "The index arrangements as they appear on the board are as follows:"
    putStrLn "(store B) [ | 5 | , | 4 | , | 3 | , | 2 | , | 1 | , | 0 | ]"
    putStrLn "          [ | 0 | , | 1 | , | 2 | , | 3 | , | 4 | , | 5 | ] (store A)"
    putStrLn ""
    putStrLn "To quit, type in any character that isn't a number."
playGame :: Player -> MancalaBoard -> IO ()
playGame p board = do
    putStrLn (show board)
    board2 <- moveIO p board
    if gameOver board2 then endGame (winners board2)
        else if (getCurPlayer board2 == p) then do
            putStrLn (show p ++ " got an extra turn!")
            putStrLn ""
            playGame p board2
            else playGame (nextPlayer p) board2
moveIO :: Player -> MancalaBoard -> IO MancalaBoard
moveIO p m = do
    putStrLn ("---------" ++ show p ++"'s Turn---------")
    putStrLn "What pit will you move?"
    mv' <- getLine
    mv <- if p == PlayerB then return (read mv' + 7) else return (read mv')
    if (isAllowedMove mv m) then return (move mv m)
        else do
            putStrLn "That move is not allowed."
            putStrLn ("Legal moves are: "++ (show (allowedMoves m)))
            moveIO p m
            
endGame :: [Player] -> IO ()
endGame ws = do
    putStrLn "---------Game Over---------"
    if null ws then putStrLn "Tie Game!"
        else putStrLn (show (ws!!0) ++ " won the game!")
        
    
    