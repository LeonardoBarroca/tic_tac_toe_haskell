module Lib
    ( createBoard
    , playGame
    ) where

import Data.List (intercalate)

type Player = Int
type Position = (Int, Int)
type Board = [[Maybe Player]]

-- Create an empty Tic-Tac-Toe board
createBoard :: Int -> Board
createBoard size = replicate size (replicate size Nothing)

-- Print the Tic-Tac-Toe board
printBoard :: Board -> IO ()
printBoard board = do
  let rows = map (intercalate " | " . map (maybe " " show)) board
  let separator = replicate (length (head rows) * 2 - 1) '-'
  putStrLn $ intercalate ("\n" ++ separator ++ "\n") rows

-- Update the board with a player's move
makeMove :: Board -> Player -> Position -> Maybe Board
makeMove board player (row, col)
  | row >= 0 && row < size && col >= 0 && col < size && isEmpty (board !! row !! col) =
    Just $ replace2D board row (replace2D (board !! row) col (Just player))
  | otherwise = Nothing
  where
    size = length board
    isEmpty = (== Nothing)
    replace2D lst i val = take i lst ++ [val] ++ drop (i + 1) lst

-- Check if the board is full
isBoardFull :: Board -> Bool
isBoardFull = all (notElem Nothing)

-- Check if a player has won
isWinningMove :: Board -> Player -> Bool
isWinningMove board player =
  any (all (== Just player)) (rows ++ columns ++ diagonals)
  where
    size = length board
    rows = board
    columns = [[board !! row !! col | row <- [0..size-1]] | col <- [0..size-1]]
    diagonals = [[board !! i !! i | i <- [0..size-1]], [board !! i !! (size-1-i) | i <- [0..size-1]]]

-- Play the game
playGame :: Board -> Player -> IO ()
playGame board player = do
  printBoard board
  if isWinningMove board player
    then putStrLn $ "Jogador " ++ show player ++ " venceu!"
    else if isBoardFull board
         then putStrLn "Empate!"
         else do
           move <- getPlayerMove player
           case makeMove board player move of
             Just newBoard -> playGame newBoard (nextPlayer player)
             Nothing -> do
               putStrLn "Jogada inválida, tente novamente."
               playGame board player

-- Get the player's move from the command line
getPlayerMove :: Player -> IO Position
getPlayerMove player = do
  putStrLn $ "Jogador " ++ show player ++ ", faça sua jogada (linha [0,1,2] e coluna [0,1,2]): "
  input <- getLine
  let position = parsePosition input
  case position of
    Just pos -> return pos
    Nothing -> do
      putStrLn "Jogada inválida, tente novamente."
      getPlayerMove player

-- Parse the player's move input
parsePosition :: String -> Maybe Position
parsePosition input = case words input of
  [row, col] -> do
    r <- readMaybe row
    c <- readMaybe col
    return (r, c)
  _ -> Nothing

-- Read a value from a string
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- Determine the next player
nextPlayer :: Player -> Player
nextPlayer 1 = 2
nextPlayer 2 = 1