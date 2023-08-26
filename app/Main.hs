module Main (main) where

import Lib

main :: IO ()
main = do
  let boardSize = 3
  let initialBoard = createBoard boardSize
  putStrLn "Tic-Tac-Toe-2"
  playGame initialBoard 1