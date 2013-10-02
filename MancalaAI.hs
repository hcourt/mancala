--Hazel Court
--The AI "Antikythera"
module MancalaAI(aiNextMove) where

import MancalaBoard
import Data.List
import Data.Maybe

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.
aiNextMove :: MancalaBoard -> Move
aiNextMove m = if length (allowedMoves m) == 1 then head (allowedMoves m) else lookahead m 4 -- change to lookahead m 3 if it becomes problematic on your system

--evaluates a mancala board in breadth-first fashion
eval :: Player -> Int -> MancalaBoard -> Int
eval p 0 m = heuristicScore p m
eval p d m = final where
    availBoardsA = getAvailBoardsA p m
    availBoardsB = [move x (lookahead x 0) | x <- availBoardsA, allowedMoves x /= []]
    finalList = (map (eval p (d-1)) availBoardsB)
    final = if null finalList then -1 else maximum $ finalList
    getAvailBoardsA :: Player -> MancalaBoard -> [MancalaBoard]
    getAvailBoardsA p m = if p == getCurPlayer m then concat $ map (getAvailBoardsA p) (map (move m) (allowedMoves m))
        else [m]

--looks ahead a certain number of turns and picks the best move
lookahead :: MancalaBoard -> Int -> Move
lookahead m d = if evaluated == [] then lookahead m (d-1) else nextMove where
    boards = map (move m) (allowedMoves m)
    evaluated = filter (/= -1) $ map (eval (getCurPlayer m) d) boards
    best = maximum evaluated
    bestIndex = head $ elemIndices best evaluated
    nextMove = (allowedMoves m) !! bestIndex

--assembles a heuristic score following the a* algorithm, determined by weighing a captured piece twice as much as the number of pieces on the player's side
heuristicScore :: Player -> MancalaBoard -> Int
heuristicScore p m = 2 * (numCaptured m p) + (sum (playerSide m p))

--call this with initial to play a game between two AIs
simulateGame :: MancalaBoard -> (MancalaBoard -> Move) -> (MancalaBoard -> Move) -> Int -> IO()
-- board -> first player to go, AI 1 -> second player to go, AI2 -> depth -> output
{-
if depth = 0 proceed no steps
if depth = -1 run game forward until end
if depth = k run game forward k steps
-}
simulateGame m _ _ 0 = do
    putStrLn $ "Game Ended at " ++ (show $ getCurPlayer m) ++ "'s turn"
    let winner = if null (whoWins m) then "tie" else show (head (whoWins m)) in putStrLn $ "Winner: " ++ winner
simulateGame m ai1 ai2 (-1) = if isGameOver m then simulateGame m ai1 ai2 0
    else do
        putStrLn $ show m --optional, shows the progress of the game
        let nextMove = ai1 m in simulateGame (move m nextMove) ai2 ai1 (-1)
simulateGame m ai1 ai2 depth = let nextMove = ai1 m in simulateGame (move m nextMove) ai2 ai1 (depth - 1)

--migrated for simulateGame
isGameOver :: MancalaBoard -> Bool
isGameOver mancala = any (==0) (map (length . (allowedMovesFor mancala)) allPlayers)
    