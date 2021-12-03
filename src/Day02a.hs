{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List

data Movement
  = Forward Int
  | Down Int
  | Up Int

type Position = (Int, Int)

move :: Position -> Movement -> Position
move (horiz, depth) movement =
  case movement of
    Forward x -> (horiz + x, depth)
    Down x    -> (horiz,     depth + x)
    Up x      -> (horiz,     depth - x)

solve :: [Movement] -> Int
solve movements = horiz * depth
  where (horiz, depth) = foldl move (0,0) movements

parseMovement :: String -> Movement
parseMovement (stripPrefix "forward " -> Just n) = Forward $ read n
parseMovement (stripPrefix "down " -> Just n)    = Down $ read n
parseMovement (stripPrefix "up " -> Just n)      = Up $ read n

main =
  do movements <- map parseMovement <$> lines <$> getContents
     print (solve movements)
