module Main (main) where

import Lib
import Front 

main :: IO ()
main = do
  putStrLn "Tic-Tac-Toe"
  mode <- chooseGameMode
  let boardSize = 3
  let initialBoard = createBoard boardSize

  case mode of
    HumanVsHuman -> playGameGraphics initialBoard 
    HumanVsAI -> playGame initialBoard 1 mode

  putStrLn "Pressione qualquer tecla para encerrar o programa..."
  getChar
  putStrLn "\nEncerrando o programa."

chooseGameMode :: IO PlayerMode
chooseGameMode = do
  putStrLn "Escolha o modo de jogo:"
  putStrLn "1. Humano vs. Humano"
  putStrLn "2. Humano vs. Máquina"
  input <- getLine
  case input of
    "1" -> return HumanVsHuman
    "2" -> return HumanVsAI
    _ -> do
      putStrLn "Opção inválida. Por favor, escolha 1 ou 2."
      chooseGameMode