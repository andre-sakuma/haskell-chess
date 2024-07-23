module Game where

import AI
import Data
import Data (testBoard)
import Moves
import System.Console.ANSI

initialize :: IO GameState
initialize = do
  putStrLn "Como você quer jogar?"
  putStrLn "ia - contra a máquina"
  putStrLn "pvp - contra outro jogador"

  input <- getLine
  if input == "ia"
    then return (GameState initialBoard WhiteP PvAI)
    else
      if input == "pvp"
        then return (GameState testBoard WhiteP PvP)
        else do
          putStrLn "Entrada inválida!"
          initialize

gameLoop :: GameState -> IO ()
gameLoop (GameState board turn mode) = do
  if ended (GameState board turn mode)
    then finish board
    else do
      if mode == PvAI && turn == BlackP
        then do
          let allPossibleMoves = getAllPossibleMoves board turn
          let movement = chooseMove board
          let newBoard = uncurry (move board) movement
          print $ printPosition (getPositionFromSquare (fst movement)) ++ " -> " ++ printPosition (getPositionFromSquare (snd movement))
          gameLoop $ nextTurn $ GameState newBoard turn mode
        else do
          print turn
          printBoard board

          input <- getLine
          if input == "help"
            then do
              printAllPossibleMoves (GameState board turn mode)
              gameLoop (GameState board turn mode)
            else do
              let from = getSquare board (convertStringToPosition $ head $ words input)
              let to = getSquare board (convertStringToPosition $ last $ words input)

              if isInputValid input
                then do
                  if isMoveValid board turn from to
                    then do
                      let newBoard = move board from to
                      gameLoop $ nextTurn $ GameState newBoard turn mode
                    else do
                      putStrLn "Movimento Inválido!"
                      gameLoop (GameState board turn mode)
                else putStrLn "Valor Inválido!"

printAllPossibleMoves :: GameState -> IO ()
printAllPossibleMoves (GameState board turn mode) = do
  let allPossibleMoves = getAllPossibleMoves board turn
  let allPossibleMovesStr = map (\(from, to) -> printSquare from ++ printPosition (getPositionFromSquare from) ++ " -> " ++ printSquare to ++ printPosition (getPositionFromSquare to)) allPossibleMoves
  putStrLn "Todos movimentos possíveis:"
  printArray allPossibleMovesStr

printArray :: [String] -> IO ()
printArray = putStrLn . unlines

printPosition :: Position -> String
printPosition (Position row col) = '(' : colToChar col : rowToChar row : ")"

finish :: Board -> IO ()
finish board = do
  printBoard board
  putStrLn "Fim de jogo!"
  print $ winner board

winner :: Board -> String
winner board =
  if isCheckMate board WhiteP
    then "Preto venceu!"
    else "Branco venceu!"

nextTurn :: GameState -> GameState
nextTurn (GameState board WhiteP mode) = GameState board BlackP mode
nextTurn (GameState board BlackP mode) = GameState board WhiteP mode

isInputValid :: String -> Bool
isInputValid input = not $ isArgsInvalid $ words input

ended :: GameState -> Bool
-- ended (GameState board color) = False
ended (GameState board color _) = isCheckMate board color

isArgsInvalid :: [String] -> Bool
isArgsInvalid args = length args /= 2 || not (isStringValid (head args)) || not (isStringValid (last args))

convertStringToPosition :: String -> Position
convertStringToPosition str = Position (charToRow (last str)) (charToCol (head str))

isStringValid :: String -> Bool
isStringValid str = length str == 2 && elem (head str) validColChars && elem (last str) validRowChars

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "   a  b  c  d  e  f  g  h"
  printBoard' board 0

printBoard' :: Board -> Int -> IO ()
printBoard' board row = do
  if row == 8
    then return ()
    else do
      printRow board row
      printBoard' board (row + 1)

printRow :: Board -> Int -> IO ()
printRow board row = do
  putStr $ show (row + 1) ++ " "
  printRow' board row 0

printRow' :: Board -> Int -> Int -> IO ()
printRow' board row col = do
  if col == 8
    then putStrLn ""
    else do
      let square = getSquare board (Position row col)
      if isSquareEmpty square
        then setSGR []
        else
          if getPieceColorFromPiece (getPieceFromSquare square) == WhiteP
            then setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White]
            else setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Black]

      if even (row + col)
        then setSGR [SetColor Background Dull Green]
        else setSGR [SetColor Background Dull White]
      putStr $ " " ++ printSquare square ++ " "
      setSGR [Reset]
      printRow' board row (col + 1)

printSquare :: Square -> String
printSquare (Square _ Empty) = " "
printSquare (Square _ (Occupied (Piece _ Pawn _))) = "♟"
printSquare (Square _ (Occupied (Piece _ Knight _))) = "♞"
printSquare (Square _ (Occupied (Piece _ Bishop _))) = "♝"
printSquare (Square _ (Occupied (Piece _ Rook _))) = "♜"
printSquare (Square _ (Occupied (Piece _ Queen _))) = "♛"
printSquare (Square _ (Occupied (Piece _ King _))) = "♚"
