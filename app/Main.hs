module Main where

import Game

main :: IO ()
main = initialize >>= gameLoop
