module MancalaBoard (MancalaBoard, Player (PlayerA, PlayerB), initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners, nextPlayer) where

import Data.List as List-- for List.elemIndex
import Data.Maybe as Maybe-- for List.elemIndex
import Data.Char

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player deriving (Eq)

data Player = PlayerA | PlayerB deriving (Eq, Show, Read)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- TODO: uncomment these type declarations and implement the functions
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
--TODO: replace below line with real definition
--getCurPlayer _ = PlayerA
getCurPlayer (MancalaBoardImpl _ p) = p

{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
--TODO: replace below line with real definition
getBoardData (MancalaBoardImpl ints _) = concat $ splitBoard ints
splitBoard :: [Int] -> [[Int]] --splitBoard is a recursive helper function, needed because getBoardData should not be recursive
splitBoard [] = [[]]
splitBoard ints = ((take 6 ints) : splitBoard (tail (drop 6 ints)))


{- return the side of the board for a specified player, including the store at
 - the end -}
--TODO: define this function
playerSide :: Player -> MancalaBoard -> [Int]
playerSide p (MancalaBoardImpl ints _) = take 7 $ drop (indexForFirstPit p) ints


{- return the number of captured pieces in specified player's store -}
--TODO: add type and replace below line with real definition
numCaptured :: Player -> MancalaBoard -> Int
numCaptured p m = last $ playerSide p m


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
--TODO: add type and replace below line with real definition
allowedMoves :: MancalaBoard -> [Int]
allowedMoves (MancalaBoardImpl ints p) = [x | x <- indicesForPlayerSide p, y <- [ints !! x], y > 0]


{- check that a move is valid for the current player -}
--TODO: add type and replace below line with real definition
isAllowedMove :: Int -> MancalaBoard -> Bool
isAllowedMove mv m = mv `elem` (allowedMoves m) where
    f = [((allowedMoves m) !! mv)] ++ [mv]


{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
--TODO: add type and replace below line with real definition
--this description is ambiguious, so I will define move to recieve an already selected and approved move and apply it to the board
--the function will also advance to the next player if the store was not hit last
move :: Int -> MancalaBoard -> MancalaBoard
move mv m@(MancalaBoardImpl ints p) = MancalaBoardImpl (final)  (findNext) where
    findNext = if (ints!!(indexForPlayerStore p)) < (final!!(indexForPlayerStore p)) && (ints!!checkIndex) == (final!!checkIndex) then p else nextPlayer p
    checkIndex = if p == PlayerA then indexForPlayerStore p + 1 else 0
    first = (take (indexForPlayerStore (nextPlayer p)) application)
    nPStore = [numCaptured (nextPlayer p) m]
    last = (drop (indexForPlayerStore (nextPlayer p)) application)
    final = first ++ nPStore ++ last
    --setZero sets the value at the move location to be zero and removes the stores
    setZero = getBoardData (MancalaBoardImpl ((take mv ints) ++ [0] ++ (drop (mv+1) ints)) p)
    --application uses getApply on the list of applicable spaces
    t1 = if p == PlayerA then indexForPlayerStore p else indexForPlayerStore p - 1 -- the initial take (in application) depends on the player
    i1 = if p == PlayerA then mv+1 else mv -- the initial index also depends on the player
    application = getApply i1 (ints!!mv) ((take t1 (setZero)) ++ [numCaptured p m] ++ (drop (indexForPlayerStore p) (setZero)))
    
    --getApply tracks through the list, adding 1 to each element until it reaches the
    --end of the list (where it loops around) or the count ends (where it finishes)
    getApply :: Int -> Int -> [Int] -> [Int]
    getApply _ 0 list = list
    getApply index count list = if index == (length list - 1) then getApply 0 (count - 1) ((take 12 list) ++ [(list !! 12) + 1])
        else getApply (index + 1) (count - 1) ((take (index) list) ++ [(list !! (index)) + 1] ++ (drop (index + 1) list))

{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- TODO: replace below line with real definition
gameOver m@(MancalaBoardImpl ints p) = emptyBoard == (take 6 (getBoardData m)) || emptyBoard == (drop 6 (getBoardData m))
emptyBoard = replicate 6 0


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners m = if (score PlayerA m) == (score PlayerB m) then []
    else if (score PlayerA m) > (score PlayerB m) then [PlayerA]
        else [PlayerB]
--score calculates the raw score of a player by adding their store value
--and the number of pieces on their side.
score :: Player -> MancalaBoard -> Int
score p m@(MancalaBoardImpl ints _)= (numCaptured p m) + (sum (playerSide p m))

---- show

instance Show MancalaBoard where
    show m@(MancalaBoardImpl boardData player) =
            "Current Player: " ++ (show player) ++ "\n"
                ++ (show PlayerB) ++ ": " ++ (show (numCaptured PlayerB m)) ++ " " ++ (boardSep (show (reverse (init(drop 7 boardData))))) ++ "\n"
                ++ (show PlayerA) ++ ":   " ++ (boardSep (show (take 6 boardData))) ++ " " ++ (show (numCaptured PlayerA m)) ++ "\n"

--parses the board in such a way that looks more representative of a board.
boardSep :: String -> String
boardSep list@(b : bs) = if null bs then (b : [])
    else if (isDigit b) then (takeWhile isDigit list ++ " | " ++ boardSep (dropWhile isDigit list))
        else (b : ' ' : '|' : ' ' : boardSep bs)
   
---- testing   

-- easy IO test function; available for any function that takes a MancalaBoard
-- as its last argument.  For functions that take multiple arguments,
-- input the function as a partial function. (e.g. "move 5")

testFunct :: (MancalaBoard -> a) -> IO a
testFunct f = do
    putStr "Current player>"
    p <- getLine
    putStr "Player A's side>"
    a <- getLine
    putStr "Player B's side>"
    b <- getLine
    putStrLn "--"
    putStrLn "Result:"
    return (f (MancalaBoardImpl ((read a) ++ (read b)) (read p)))

--call testAll to check all functions implemented.
testAll :: IO ()
testAll = do
    putStrLn "Testing Functions:"
    sequence_ (map testEach testSet)
testEach :: (String, Bool) -> IO ()
testEach (n, b) = if b then putStrLn (n ++ " passed.") else putStrLn (n ++ " failed.")

--test variables
testSet = [("testGetCurPlayer", testGetCurPlayer), ("testGetBoardData", testGetBoardData), ("testPlayerSide", testPlayerSide), ("testNumCaptured", testNumCaptured), ("testAllowedMoves", testAllowedMoves), ("testIsAllowedMove", testIsAllowedMove), ("testMove", testMove), ("testGameOver", testGameOver), ("testWinners", testWinners)]

testBoard = MancalaBoardImpl [0, 1, 2, 3, 4, 5, 6, 10, 9, 8, 7, 6, 5, 4] PlayerA
testFinishBoard = MancalaBoardImpl [0, 0, 0, 0, 0, 0, 12, 1, 2, 3, 4, 0, 0, 1] PlayerA

testGetCurPlayer = getCurPlayer testBoard == PlayerA
testGetBoardData = getBoardData testBoard == [0, 1, 2, 3, 4, 5, 10, 9, 8, 7, 6, 5]
testPlayerSide = playerSide PlayerA testBoard == [0, 1, 2, 3, 4, 5, 6] && playerSide PlayerB testBoard == [10, 9, 8, 7, 6, 5, 4]
testNumCaptured = numCaptured PlayerA testBoard == 6 && numCaptured PlayerB testBoard == 4
testAllowedMoves = allowedMoves testBoard == [1, 2, 3, 4, 5]
testIsAllowedMove = isAllowedMove 1 testBoard && (isAllowedMove 0 testBoard) == False && (isAllowedMove 6 testBoard) == False && (isAllowedMove 7 testBoard) == False
testMove = move 5 testBoard == MancalaBoardImpl [0, 1, 2, 3, 4, 0, 7, 11, 10, 9, 8, 6, 5, 4] PlayerB && move 3 testBoard == MancalaBoardImpl [0, 1, 2, 0, 5, 6, 7, 10, 9, 8, 7, 6, 5, 4] PlayerA
testGameOver = gameOver testBoard == False && gameOver testFinishBoard == True
testWinners = winners testBoard == [PlayerB] && winners testFinishBoard == [PlayerA]