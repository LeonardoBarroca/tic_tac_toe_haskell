module Main (main) where

import Lib

main :: IO ()
main = do
  let boardSize = 3
  let initialBoard = createBoard boardSize
  putStrLn "Tic-Tac-Toe"
  playGame initialBoard 1
  putStrLn "Pressione qualquer tecla para encerrar o programa..."
  getChar
  putStrLn "\nEncerrando o programa."