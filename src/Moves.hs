module Moves where

import Data
import Data.List (find, nub)
import Data.Maybe (isNothing)

kingHazardPositions :: Color -> Board -> [Position]
kingHazardPositions color board =
  nub $
    concatMap
      ( \square ->
          if isSquareEmpty square
            || (getPieceColorFromPiece (getPieceFromSquare square) == color)
            then []
            else
              filter
                (\s -> not (hasSomethingBlocking board (getSquare board s) square))
                (filterPositionsInsideBoard (pieceCaptures (getPieceFromSquare square) (getPositionFromSquare square)))
      )
      (concat board)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

isInCheck :: Board -> Color -> Bool
isInCheck board color =
  getPositionFromSquare (kingSquare board color) `elem` kingHazardPositions color board

kingSquare :: Board -> Color -> Square
kingSquare board color =
  head $
    filter
      ( \square ->
          not (isSquareEmpty square)
            && getPieceTypeFromPiece (getPieceFromSquare square) == King
            && getPieceColorFromPiece (getPieceFromSquare square) == color
      )
      (concat board)

isCheckMate :: Board -> Color -> Bool
isCheckMate board color =
  kingIsDead board color
    || isInCheck board color
      && all
        (\movement -> isInCheck (uncurry (move board) movement) color)
        (getAllPossibleMoves board color)

kingIsDead :: Board -> Color -> Bool
kingIsDead board color =
  isNothing
    ( find
        ( \square ->
            not (isSquareEmpty square)
              && getPieceTypeFromPiece (getPieceFromSquare square) == King
              && getPieceColorFromPiece (getPieceFromSquare square) == color
        )
        (concat board)
    )

getAllPossibleMoves :: Board -> Color -> [(Square, Square)]
getAllPossibleMoves board turn = concatMap ((\from -> map (\to -> (from, to)) (possibleMoves board turn from)) . getSquare board) allPositions

move :: Board -> Square -> Square -> Board
move board from to
  | isValidSpecialMove board from to = specialMove board from to
  | isSquareEmpty to = movePiece board from to
  | fromColor /= toColor = movePiece board from to
  | otherwise = board
  where
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from
    toColor = getPieceColorFromPiece $ getPieceFromSquare to

specialMove :: Board -> Square -> Square -> Board
specialMove board from to
  | isCastlingMove board from to = castlingMove board from to
  | isEnPassantMove board from to = enPassantMove board from to
  | isPromotionMove from to = promotionMove board from to
  | otherwise = board

promotionMove :: Board -> Square -> Square -> Board
promotionMove board from to =
  insertPiece (movePiece board from to) to (Piece fromColor Queen 0)
  where
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from

insertPiece :: Board -> Square -> Piece -> Board
insertPiece board (Square position _) piece =
  map
    ( map
        ( \square ->
            if getPositionFromSquare square == position
              then Square position (Occupied piece)
              else square
        )
    )
    board

enPassantMove :: Board -> Square -> Square -> Board
enPassantMove board from to =
  movePiece (movePiece board from to) supposedToBeCapturedPawnSquare (Square (getPositionFromSquare supposedToBeCapturedPawnSquare) Empty)
  where
    supposedToBeCapturedPawnSquare = getSquare board (Position (toRow + changeOrientation fromColor (-1)) toCol)
    toPosition = getPositionFromSquare to
    toRow = getRowFromPosition toPosition
    toCol = getColFromPosition toPosition
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from

castlingMove :: Board -> Square -> Square -> Board
castlingMove board from to =
  kingMove (rookMove board from to) (kingSquare board fromColor) rookSideSquare
  where
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from
    toCol = getColFromPosition $ getPositionFromSquare to
    fromCol = getColFromPosition $ getPositionFromSquare from
    fromRow = getRowFromPosition $ getPositionFromSquare from
    rookSideSquare = if toCol > fromCol then getSquare board (Position fromRow 2) else getSquare board (Position fromRow 6)
    rookMove = movePiece
    kingMove = movePiece

movePiece :: Board -> Square -> Square -> Board
movePiece board from to =
  map
    ( map
        ( \square ->
            if getPositionFromSquare square == getPositionFromSquare from
              then Square (getPositionFromSquare square) Empty
              else
                if getPositionFromSquare square == getPositionFromSquare to
                  then Square (getPositionFromSquare square) (Occupied (incrementMovesDone $ getPieceFromSquare from))
                  else square
        )
    )
    board

incrementMovesDone :: Piece -> Piece
incrementMovesDone (Piece color pieceType movesDone) = Piece color pieceType (movesDone + 1)

allPositions = [Position row col | row <- [0 .. 7], col <- [0 .. 7]]

possibleMoves :: Board -> Color -> Square -> [Square]
possibleMoves board color from =
  filter (isMoveValid board color from) (map (getSquare board) allPositions)

isMoveValid :: Board -> Color -> Square -> Square -> Bool
isMoveValid board turn from to =
  not (hasSomethingBlocking board from to)
    && isValidMove board from to
    && canPawnMoveForward board from to
    && isOwnPiece turn from
    && not (tryingToCaptureOwnPiece turn from to)

tryingToCaptureOwnPiece :: Color -> Square -> Square -> Bool
tryingToCaptureOwnPiece turn from to =
  not (isSquareEmpty from || isSquareEmpty to)
    && getPieceColorFromPiece (getPieceFromSquare from) == getPieceColorFromPiece (getPieceFromSquare to)

isOwnPiece :: Color -> Square -> Bool
isOwnPiece turn (Square _ Empty) = False
isOwnPiece turn (Square _ (Occupied (Piece color _ _))) = turn == color

canPawnMoveForward :: Board -> Square -> Square -> Bool
canPawnMoveForward board from to =
  (isSquareEmpty from || isSquareEmpty to)
    || ( (getPieceTypeFromPiece (getPieceFromSquare from) /= Pawn)
           || not isFowardMove
           || (isSquareEmpty to && not (hasSomethingBlocking board from to))
       )
  where
    fromCol = getColFromPosition $ getPositionFromSquare from
    toCol = getColFromPosition $ getPositionFromSquare to
    isFowardMove = toCol == fromCol

hasSomethingBlocking :: Board -> Square -> Square -> Bool
hasSomethingBlocking _ (Square _ (Occupied (Piece _ Knight _))) _ = False
hasSomethingBlocking board from to =
  not (null betweenSquares)
    && not (all isSquareEmpty betweenSquares)
  where
    betweenSquares = getSquaresBetween board from to

getSquaresBetween :: Board -> Square -> Square -> [Square]
getSquaresBetween board (Square (Position fromRow fromCol) _) (Square (Position toRow toCol) _)
  | fromRow == toRow = [getSquare board (Position fromRow col) | col <- [min fromCol toCol + 1 .. max fromCol toCol - 1]]
  | fromCol == toCol = [getSquare board (Position row fromCol) | row <- [min fromRow toRow + 1 .. max fromRow toRow - 1]]
  | otherwise =
      filter
        (isAtSameDiagonal (Position fromRow fromCol) . getPositionFromSquare)
        [ getSquare board (Position row col)
          | row <- [min fromRow toRow + 1 .. max fromRow toRow - 1],
            col <- [min fromCol toCol + 1 .. max fromCol toCol - 1]
        ]

isAtSameDiagonal :: Position -> Position -> Bool
isAtSameDiagonal (Position row1 col1) (Position row2 col2) = abs (row1 - row2) == abs (col1 - col2)

isValidPosition :: Position -> Bool
isValidPosition (Position row col) = row >= 0 && row <= 7 && col >= 0 && col <= 7

isValidMove :: Board -> Square -> Square -> Bool
isValidMove board from to =
  isValidPieceMove from to
    || isValidCaptureMove from to
    || isValidSpecialMove board from to

isValidSpecialMove :: Board -> Square -> Square -> Bool
isValidSpecialMove board from to =
  isCastlingMove board from to
    || isEnPassantMove board from to
    || isPromotionMove from to

isPromotionMove :: Square -> Square -> Bool
isPromotionMove from to =
  not (isSquareEmpty from)
    && isSquareEmpty to
    && fromIsPawn
    && toIsLastRow
    && toIsFromFront
  where
    fromIsPawn = getPieceTypeFromPiece (getPieceFromSquare from) == Pawn
    toPosition = getPositionFromSquare to
    toRow = getRowFromPosition toPosition
    toCol = getColFromPosition toPosition
    fromRow = getRowFromPosition $ getPositionFromSquare from
    fromCol = getColFromPosition $ getPositionFromSquare from
    toIsLastRow = toRow == 0 || toRow == 7
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from
    toIsFromFront = fromCol == toCol && (fromRow + changeOrientation fromColor 1) == toRow

isCastlingMove :: Board -> Square -> Square -> Bool
isCastlingMove board from to =
  not (isSquareEmpty from)
    && fromIsRook
    && kingIsAttoRowNeighbours
    && isKingFirstMove
    && isRookFirstMove
  where
    fromIsRook = getPieceTypeFromPiece (getPieceFromSquare from) == Rook
    fromRow = getRowFromPosition $ getPositionFromSquare from
    toRow = getRowFromPosition $ getPositionFromSquare to
    toCol = getColFromPosition $ getPositionFromSquare to
    toNeighbours = map (getSquare board) (filterPositionsInsideBoard [Position toRow (toCol - 1), Position toRow (toCol + 1)])
    king = kingSquare board (getPieceColorFromPiece $ getPieceFromSquare from)
    kingIsAttoRowNeighbours = king `elem` toNeighbours
    isKingFirstMove = getPieceMovesDoneFromPiece (getPieceFromSquare king) == 0
    isRookFirstMove = getPieceMovesDoneFromPiece (getPieceFromSquare from) == 0

isEnPassantMove :: Board -> Square -> Square -> Bool
isEnPassantMove board from to =
  not (isSquareEmpty from) && fromIsPawn && isFoward && toIsEmpty && validCapture
  where
    fromIsPawn = getPieceTypeFromPiece (getPieceFromSquare from) == Pawn
    toPosition = getPositionFromSquare to
    toRow = getRowFromPosition toPosition
    toCol = getColFromPosition toPosition
    fromRow = getRowFromPosition $ getPositionFromSquare from
    isFoward = toRow == fromRow + changeOrientation fromColor 1
    fromColor = getPieceColorFromPiece $ getPieceFromSquare from
    supposedToBeCapturedPawnSquare = getSquare board (Position (toRow + changeOrientation fromColor (-1)) toCol)
    toIsEmpty = isSquareEmpty to
    validCapture =
      not (isSquareEmpty supposedToBeCapturedPawnSquare)
        && getPieceColorFromPiece (getPieceFromSquare supposedToBeCapturedPawnSquare) /= fromColor
        && getPieceTypeFromPiece (getPieceFromSquare supposedToBeCapturedPawnSquare) == Pawn
        && getPieceMovesDoneFromPiece (getPieceFromSquare supposedToBeCapturedPawnSquare) == 1
        && ( toRow == 2
               || toRow == 5
           )

isValidCaptureMove :: Square -> Square -> Bool
isValidCaptureMove (Square _ Empty) _ = False
isValidCaptureMove _ (Square _ Empty) = False
isValidCaptureMove (Square _ (Occupied (Piece WhiteP _ _))) (Square _ (Occupied (Piece WhiteP _ _))) = False
isValidCaptureMove (Square _ (Occupied (Piece BlackP _ _))) (Square _ (Occupied (Piece BlackP _ _))) = False
isValidCaptureMove (Square position (Occupied piece)) (Square toPos _) =
  elem toPos $ pieceCaptures piece position

filterPositionsInsideBoard :: [Position] -> [Position]
filterPositionsInsideBoard = filter (\(Position row col) -> row >= 0 && row <= 7 && col >= 0 && col <= 7)

pieceCaptures :: Piece -> Position -> [Position]
pieceCaptures (Piece color Pawn _) (Position row col) =
  filterPositionsInsideBoard
    [ Position (row + changeOrientation color 1) (col + 1),
      Position (row + changeOrientation color 1) (col - 1)
    ]
pieceCaptures (Piece color Knight movesDone) (Position row col) = pieceMoves (Piece color Knight movesDone) (Position row col)
pieceCaptures (Piece color Bishop movesDone) (Position row col) = pieceMoves (Piece color Bishop movesDone) (Position row col)
pieceCaptures (Piece color Rook movesDone) (Position row col) = pieceMoves (Piece color Rook movesDone) (Position row col)
pieceCaptures (Piece color Queen movesDone) (Position row col) = pieceMoves (Piece color Queen movesDone) (Position row col)
pieceCaptures (Piece color King movesDone) (Position row col) = pieceMoves (Piece color King movesDone) (Position row col)

isValidPieceMove :: Square -> Square -> Bool
isValidPieceMove (Square _ Empty) _ = False
isValidPieceMove (Square position (Occupied piece)) (Square toPos _) =
  elem toPos $ pieceMoves piece position

pieceMoves :: Piece -> Position -> [Position]
pieceMoves (Piece color Pawn movesDone) (Position row col) =
  filterPositionsInsideBoard (Position (row + changeOrientation color 1) col : ([Position (row + changeOrientation color 2) col | movesDone == 0]))
pieceMoves (Piece color Knight _) (Position row col) =
  filterPositionsInsideBoard
    [ Position (row + changeOrientation color 2) (col + 1),
      Position (row + changeOrientation color 2) (col - 1),
      Position (row + changeOrientation color 1) (col + 2),
      Position (row + changeOrientation color 1) (col - 2),
      Position (row + changeOrientation color (-1)) (col + 2),
      Position (row + changeOrientation color (-1)) (col - 2),
      Position (row + changeOrientation color (-2)) (col + 1),
      Position (row + changeOrientation color (-2)) (col - 1)
    ]
pieceMoves (Piece color Bishop _) (Position row col) =
  filterPositionsInsideBoard
    ( [Position (row + changeOrientation color x) (col + x) | x <- [1 .. 7]]
        ++ [Position (row + changeOrientation color x) (col - x) | x <- [1 .. 7]]
        ++ [Position (row - changeOrientation color x) (col + x) | x <- [1 .. 7]]
        ++ [Position (row - changeOrientation color x) (col - x) | x <- [1 .. 7]]
    )
pieceMoves (Piece color Rook _) (Position row col) =
  filterPositionsInsideBoard
    ( [Position (row + changeOrientation color x) col | x <- [1 .. 7]]
        ++ [Position row (col + x) | x <- [1 .. 7]]
        ++ [Position (row - changeOrientation color x) col | x <- [1 .. 7]]
        ++ [Position row (col - x) | x <- [1 .. 7]]
    )
pieceMoves (Piece color Queen _) (Position row col) =
  filterPositionsInsideBoard
    ( pieceMoves (Piece color Bishop 0) (Position row col)
        ++ pieceMoves (Piece color Rook 0) (Position row col)
    )
pieceMoves (Piece color King _) (Position row col) =
  filterPositionsInsideBoard
    [ Position (row + changeOrientation color 1) (col + 1),
      Position (row + changeOrientation color 1) (col - 1),
      Position (row + changeOrientation color 1) col,
      Position (row - changeOrientation color 1) (col + 1),
      Position (row - changeOrientation color 1) (col - 1),
      Position (row - changeOrientation color 1) col,
      Position row (col + 1),
      Position row (col - 1)
    ]

changeOrientation :: Color -> Int -> Int
changeOrientation WhiteP x = -x
changeOrientation BlackP x = x
