module Data where

data GameState = GameState Board Color Mode

data Mode = PvP | PvAI deriving (Eq)

type Board = [[Square]]

testBoard :: Board
testBoard =
  [ [ Square (Position 0 0) (Occupied (Piece BlackP Rook 0)),
      Square (Position 0 1) Empty,
      Square (Position 0 2) Empty,
      Square (Position 0 3) Empty,
      Square (Position 0 4) (Occupied (Piece BlackP King 0)),
      Square (Position 0 5) Empty,
      Square (Position 0 6) Empty,
      Square (Position 0 7) (Occupied (Piece BlackP Rook 0))
    ],
    [ Square (Position 1 0) Empty,
      Square (Position 1 1) Empty,
      Square (Position 1 2) Empty,
      Square (Position 1 3) Empty,
      Square (Position 1 4) Empty,
      Square (Position 1 5) Empty,
      Square (Position 1 6) Empty,
      Square (Position 1 7) Empty
    ],
    [ Square (Position 2 0) Empty,
      Square (Position 2 1) Empty,
      Square (Position 2 2) Empty,
      Square (Position 2 3) Empty,
      Square (Position 2 4) Empty,
      Square (Position 2 5) Empty,
      Square (Position 2 6) Empty,
      Square (Position 2 7) Empty
    ],
    [ Square (Position 3 0) Empty,
      Square (Position 3 1) Empty,
      Square (Position 3 2) Empty,
      Square (Position 3 3) Empty,
      Square (Position 3 4) Empty,
      Square (Position 3 5) Empty,
      Square (Position 3 6) Empty,
      Square (Position 3 7) Empty
    ],
    [ Square (Position 4 0) Empty,
      Square (Position 4 1) Empty,
      Square (Position 4 2) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 4 3) (Occupied (Piece WhiteP Pawn 1)),
      Square (Position 4 4) Empty,
      Square (Position 4 5) Empty,
      Square (Position 4 6) Empty,
      Square (Position 4 7) Empty
    ],
    [ Square (Position 5 0) Empty,
      Square (Position 5 1) Empty,
      Square (Position 5 2) Empty,
      Square (Position 5 3) Empty,
      Square (Position 5 4) Empty,
      Square (Position 5 5) Empty,
      Square (Position 5 6) Empty,
      Square (Position 5 7) Empty
    ],
    [ Square (Position 6 0) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 1) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 2) Empty,
      Square (Position 6 3) Empty,
      Square (Position 6 4) Empty,
      Square (Position 6 5) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 6) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 7) (Occupied (Piece BlackP Pawn 0))
    ],
    [ Square (Position 7 0) (Occupied (Piece WhiteP Rook 0)),
      Square (Position 7 1) (Occupied (Piece WhiteP Knight 0)),
      Square (Position 7 2) (Occupied (Piece WhiteP Bishop 0)),
      Square (Position 7 3) (Occupied (Piece WhiteP Queen 0)),
      Square (Position 7 4) (Occupied (Piece WhiteP King 0)),
      Square (Position 7 5) (Occupied (Piece WhiteP Bishop 0)),
      Square (Position 7 6) (Occupied (Piece WhiteP Knight 0)),
      Square (Position 7 7) Empty
    ]
  ]

initialBoard :: Board
initialBoard =
  [ [ Square (Position 0 0) (Occupied (Piece BlackP Rook 0)),
      Square (Position 0 1) (Occupied (Piece BlackP Knight 0)),
      Square (Position 0 2) (Occupied (Piece BlackP Bishop 0)),
      Square (Position 0 3) (Occupied (Piece BlackP Queen 0)),
      Square (Position 0 4) (Occupied (Piece BlackP King 0)),
      Square (Position 0 5) (Occupied (Piece BlackP Bishop 0)),
      Square (Position 0 6) (Occupied (Piece BlackP Knight 0)),
      Square (Position 0 7) (Occupied (Piece BlackP Rook 0))
    ],
    [ Square (Position 1 0) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 1) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 2) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 3) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 4) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 5) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 6) (Occupied (Piece BlackP Pawn 0)),
      Square (Position 1 7) (Occupied (Piece BlackP Pawn 0))
    ],
    [ Square (Position 2 0) Empty,
      Square (Position 2 1) Empty,
      Square (Position 2 2) Empty,
      Square (Position 2 3) Empty,
      Square (Position 2 4) Empty,
      Square (Position 2 5) Empty,
      Square (Position 2 6) Empty,
      Square (Position 2 7) Empty
    ],
    [ Square (Position 3 0) Empty,
      Square (Position 3 1) Empty,
      Square (Position 3 2) Empty,
      Square (Position 3 3) Empty,
      Square (Position 3 4) Empty,
      Square (Position 3 5) Empty,
      Square (Position 3 6) Empty,
      Square (Position 3 7) Empty
    ],
    [ Square (Position 4 0) Empty,
      Square (Position 4 1) Empty,
      Square (Position 4 2) Empty,
      Square (Position 4 3) Empty,
      Square (Position 4 4) Empty,
      Square (Position 4 5) Empty,
      Square (Position 4 6) Empty,
      Square (Position 4 7) Empty
    ],
    [ Square (Position 5 0) Empty,
      Square (Position 5 1) Empty,
      Square (Position 5 2) Empty,
      Square (Position 5 3) Empty,
      Square (Position 5 4) Empty,
      Square (Position 5 5) Empty,
      Square (Position 5 6) Empty,
      Square (Position 5 7) Empty
    ],
    [ Square (Position 6 0) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 1) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 2) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 3) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 4) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 5) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 6) (Occupied (Piece WhiteP Pawn 0)),
      Square (Position 6 7) (Occupied (Piece WhiteP Pawn 0))
    ],
    [ Square (Position 7 0) (Occupied (Piece WhiteP Rook 0)),
      Square (Position 7 1) (Occupied (Piece WhiteP Knight 0)),
      Square (Position 7 2) (Occupied (Piece WhiteP Bishop 0)),
      Square (Position 7 3) (Occupied (Piece WhiteP Queen 0)),
      Square (Position 7 4) (Occupied (Piece WhiteP King 0)),
      Square (Position 7 5) (Occupied (Piece WhiteP Bishop 0)),
      Square (Position 7 6) (Occupied (Piece WhiteP Knight 0)),
      Square (Position 7 7) (Occupied (Piece WhiteP Rook 0))
    ]
  ]

data Square = Square Position SquareContent deriving (Eq)

data Position = Position Int Int deriving (Eq)

data SquareContent = Empty | Occupied Piece deriving (Eq)

data Piece = Piece Color PieceType Int deriving (Eq)

data Color = WhiteP | BlackP deriving (Eq)

instance Show Color where
  show WhiteP = "Branco"
  show BlackP = "Preto"

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)

data MoveType = Normal | Capture | Castle | EnPassant | Promotion | Invalid deriving (Eq)

getSquareRelative :: Board -> Position -> Int -> Int -> Square
getSquareRelative board (Position row col) rowOffset colOffset = board !! (row + rowOffset) !! (col + colOffset)

getRowFromPosition :: Position -> Int
getRowFromPosition (Position row _) = row

getColFromPosition :: Position -> Int
getColFromPosition (Position _ col) = col

getPositionFromSquare :: Square -> Position
getPositionFromSquare (Square position _) = position

getPieceFromSquare :: Square -> Piece
getPieceFromSquare (Square _ Empty) = error "Cannot get piece from empty square"
getPieceFromSquare (Square _ (Occupied piece)) = piece

getPieceTypeFromPiece :: Piece -> PieceType
getPieceTypeFromPiece (Piece _ pieceType _) = pieceType

getPieceColorFromPiece :: Piece -> Color
getPieceColorFromPiece (Piece color _ _) = color

getPieceMovesDoneFromPiece :: Piece -> Int
getPieceMovesDoneFromPiece (Piece _ _ movesDone) = movesDone

getSquare :: Board -> Position -> Square
getSquare board (Position row col) = board !! row !! col

isSquareEmpty :: Square -> Bool
isSquareEmpty (Square _ Empty) = True
isSquareEmpty _ = False

validColChars :: [Char]
validColChars = ['a' .. 'h']

validRowChars :: [Char]
validRowChars = ['1' .. '8']

positionToString :: Position -> String
positionToString (Position row col) = [colToChar col, rowToChar row]

charToCol :: Char -> Int
charToCol c = case c of
  'a' -> 0
  'b' -> 1
  'c' -> 2
  'd' -> 3
  'e' -> 4
  'f' -> 5
  'g' -> 6
  'h' -> 7

charToRow :: Char -> Int
charToRow c = case c of
  '1' -> 0
  '2' -> 1
  '3' -> 2
  '4' -> 3
  '5' -> 4
  '6' -> 5
  '7' -> 6
  '8' -> 7

rowToChar :: Int -> Char
rowToChar row = case row of
  0 -> '1'
  1 -> '2'
  2 -> '3'
  3 -> '4'
  4 -> '5'
  5 -> '6'
  6 -> '7'
  7 -> '8'

colToChar :: Int -> Char
colToChar col = case col of
  0 -> 'a'
  1 -> 'b'
  2 -> 'c'
  3 -> 'd'
  4 -> 'e'
  5 -> 'f'
  6 -> 'g'
  7 -> 'h'