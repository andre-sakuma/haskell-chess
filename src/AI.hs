module AI where

import Data
import Moves
import System.CPUTime (getCPUTime)

chooseMove :: Board -> (Square, Square)
chooseMove board = getAllPossibleMoves board BlackP !! half (length (getAllPossibleMoves board BlackP))

half :: Int -> Int
half x = div x 2
