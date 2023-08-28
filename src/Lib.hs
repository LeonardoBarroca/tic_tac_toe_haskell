module Lib where

import Data.List (intercalate)
import System.Random

type Player = Int
type Position = (Int, Int)
type Board = [[Maybe Player]]

data PlayerMode = HumanVsHumanConsole | HumanVsHuman | HumanVsAI deriving (Eq)

-- Cria o quadro do jogo da velha vazio, dependendo do tamanho. Padrão: 3x3
createBoard :: Int -> Board
createBoard size = replicate size (replicate size Nothing)

-- Imprime o quadro do jogo da velha
printBoard :: Board -> IO ()
printBoard board = do
  let rows = map (intercalate " | " . map (maybe " " show)) board
  let separator = replicate (length (head rows)) '-'
  putStrLn $ intercalate ("\n" ++ separator ++ "\n") rows

-- Atualiza o quadro com o movimento atual
makeMove :: Board -> Player -> Position -> Maybe Board
makeMove board player (row, col)
  | row >= 0 && row < size && col >= 0 && col < size && isEmpty (board !! row !! col) =
    Just $ replace2D board row (replace2D (board !! row) col (Just player))
  | otherwise = Nothing
  where
    size = length board
    isEmpty = (== Nothing)
    replace2D lst i val = take i lst ++ [val] ++ drop (i + 1) lst

-- Verifica se o quadro está cheio
isBoardFull :: Board -> Bool
isBoardFull = all (notElem Nothing)

-- Verifica se o jogador venceu
isWinningMove :: Board -> Player -> Bool
isWinningMove board player =
  any (all (== Just player)) (rows ++ columns ++ diagonals)
  where
    size = length board
    rows = board
    columns = [[board !! row !! col | row <- [0..size-1]] | col <- [0..size-1]]
    diagonals = [[board !! i !! i | i <- [0..size-1]], [board !! i !! (size-1-i) | i <- [0..size-1]]]

-- Função principal do jogo, onde ocorre o fluxo do jogo e uso das outras funções
playGame :: Board -> Player -> PlayerMode -> IO ()
playGame board player mode = do
  printBoard board
  let boardSize = length board
  move <- getPlayerMove player mode board
  case makeMove board player move of
    Just newBoard -> do
      if isWinningMove newBoard player
        then do
          printBoard newBoard
          putStrLn $ "Jogador " ++ show player ++ " venceu!\n"
          playAgain <- askToPlayAgain
          if playAgain
            then playGame (createBoard boardSize) 1 mode
            else putStrLn "Obrigado por jogar!\n"
        else if isBoardFull newBoard
          then do
            printBoard newBoard
            putStrLn "Empate!\n"
            playAgain <- askToPlayAgain
            if playAgain
              then playGame (createBoard boardSize) 1 mode
              else putStrLn "Obrigado por jogar!\n"
          else playGame newBoard (nextPlayer player) mode
    Nothing -> do
      putStrLn "Jogada inválida, tente novamente.\n"
      playGame board player mode

-- Verifica se o jogador quer jogar outra partida
askToPlayAgain :: IO Bool
askToPlayAgain = do
  putStrLn "Quer jogar novamente? (S/N): "
  response <- getLine
  return $ response == "S"

-- Obtém o movimento do jogador (humano ou máquina)
getPlayerMove :: Player -> PlayerMode -> Board -> IO Position
getPlayerMove player mode board
  | mode == HumanVsHumanConsole = getHumanMove player
  | mode == HumanVsAI && player == 1 = getHumanMove player
  | mode == HumanVsAI && player == 2 = do
      putStrLn "Turno da máquina:\n"
      return $ makeAIMove board player
  | otherwise = undefined

-- Obtém o movimento do jogador humano
getHumanMove :: Player -> IO Position
getHumanMove player = do
  putStrLn $ "Jogador " ++ show player ++ ", faça sua jogada (linha [0,1,2] e coluna [0,1,2]). Exemplo: 0 0, 2 1\n"
  input <- getLine
  let position = parsePosition input
  case position of
    Just pos -> return pos
    Nothing -> do
      putStrLn "Jogada inválida, tente novamente.\n"
      getHumanMove player

-- Converte o movimento do jogador
parsePosition :: String -> Maybe Position
parsePosition input = case words input of
  [row, col] -> do
    r <- readMaybe row
    c <- readMaybe col
    return (r, c)
  _ -> Nothing

-- Lê o valor de uma string
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- Determina o próximo jogador
nextPlayer :: Player -> Player
nextPlayer 1 = 2
nextPlayer 2 = 1

-- Realiza o movimento da máquina
makeAIMove :: Board -> Player -> Position
makeAIMove board player = findBestMove board player

-- Encontra o melhor movimento para a máquina
findBestMove :: Board -> Player -> Position
findBestMove board player =
    let possibleMoves = [(row, col) | row <- [0..2], col <- [0..2]]
        validMoves = filter (isValidMove board) possibleMoves
        (randomIndex, newGen) = randomR (0, length validMoves - 1) initialGen
        randomValidMove = validMoves !! randomIndex
    in randomValidMove
  where
    initialGen = mkStdGen 10

-- Valida se o movimento da máquina é válido
isValidMove :: Board -> Position -> Bool
isValidMove board (row, col) =
  row >= 0 && row < size && col >= 0 && col < size && isEmpty (board !! row !! col)
  where
    size = length board
    isEmpty = (== Nothing)

updateBoard :: Board -> Player -> Position -> Maybe Board
updateBoard b p (row, col) =
    case makeMove b p (row, col) of
        Just newBoard -> Just newBoard
        Nothing -> Nothing