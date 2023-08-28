module Front where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib

-- Tipo GameState
data GameState = GameState
    { board         :: Board
    , currentPlayer :: Player
    , winner        :: Maybe Player
    }

-- Tamanho da janela
window :: Display
window = InWindow "Tic-Tac-Toe" (400, 400) (100, 100)

-- Cor de fundo padrão
background :: Color
background = white

-- Tamanho da célula
cellSize :: Float
cellSize = 100

-- Função para desenhar o Board
drawBoard :: GameState -> IO Picture
drawBoard gs = return $ pictures $
    [ drawCell p (row, col) | (row, rowCells) <- indexedRows, (col, p) <- indexedCells rowCells ] ++
    [ line [(0, -cellSize) ,(0, 2* cellSize)]  -- linha vertical direita
    , line [(-cellSize, -cellSize),(-cellSize, 2*cellSize)]  -- linha vertical esquerda
    , line [(-2*cellSize, cellSize), (cellSize, cellSize)]  -- linha horizontal cima
    , line [(-2*cellSize, 0), (cellSize, 0)] -- linha horizontal baixo
    , drawWinner (winner gs)
    ]
  
  where
    indexedRows = zip [0..] (board gs)
    indexedCells rowCells = zip [0..] rowCells
    drawCell :: Maybe Player -> Position -> Picture
    drawCell Nothing _ = Blank
    drawCell (Just 1) (row, col) = translate (fromIntegral col * cellSize - 150) (150 - fromIntegral row * cellSize) $
        pictures
            [ color (playerColor 1) $ rectangleSolid cellSize cellSize
            , drawX (playerColor 1) cellSize
            ]
    drawCell (Just 2) (row, col) = translate (fromIntegral col * cellSize - 150) (150 - fromIntegral row * cellSize) $
        pictures
            [ color (playerColor 2) $ rectangleSolid cellSize cellSize
            , drawO (playerColor 2) cellSize
            ]

-- Cores dos jogadores
playerColor :: Player -> Color
playerColor 1 = blue -- Xis
playerColor 2 = red -- Círculo

-- Função que retorna uma Pictura do jogador que venceu
drawWinner :: Maybe Player -> Picture
drawWinner Nothing = Blank
drawWinner (Just 0) = translate (-160) (-130) $ scale 0.2 0.2 $ Text "Empate!"
drawWinner (Just player) = translate (-160) (-130) $ scale 0.2 0.2 $ Text $ "Jogador " ++ show player ++ " venceu!"

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) gs
    | winner gs == Nothing = 
        case clickToCell (x, y) of
            Just pos -> case updateBoard (board gs) (currentPlayer gs) pos of
                            Just newBoard ->
                                return gs { board = newBoard
                                          , currentPlayer = nextPlayer (currentPlayer gs)
                                          , winner = checkWinner gs
                                          }
                            Nothing -> return gs
            Nothing -> return gs
    | otherwise = return gs
handleInput _ gs = return gs

-- Função para eventos de clique
clickToCell :: (Float, Float) -> Maybe Position
clickToCell (x, y)
    | row >= 0 && row < 3 && col >= 0 && col < 3 = Just (row, col)
    | otherwise = Nothing
  where
    row = floor ((150 - y) / cellSize)
    col = floor ((x + 150) / cellSize)

-- Função para exibição da tela
playGameGraphics :: Board -> IO ()
playGameGraphics initialBoard = playIO window background 30
    initialGameState drawBoard handleInput updateGameState
  where
    initialGameState = GameState initialBoard 1 Nothing

    updateGameState :: Float -> GameState -> IO GameState
    updateGameState _ gs = return gs

-- Função para verificar se algum jogador venceu
checkWinner :: GameState -> Maybe Player
checkWinner gs
    | isWinningMove (board gs) 1 = Just 1
    | isWinningMove (board gs) 2 = Just 2
    | isBoardFull (board gs) = Just 0  -- Empate
    | otherwise = Nothing

-- Desenhar Xis Player 1
drawX :: Color-> Float -> Picture
drawX color size =
    pictures [ line [(size * 0.2, -size * 0.2), (-size * 0.2, size * 0.2)]
             , line [(size * 0.2, size * 0.2), (-size * 0.2, -size * 0.2)]
             ]

-- Desenhar Círculo Player 2
drawO :: Color -> Float -> Picture
drawO color size = thickCircle (size * 0.4) (size * 0.1)


