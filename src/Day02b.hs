{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List

data Movement
  = Forward Int
  | Down Int
  | Up Int

type Position = (Int, Int, Int)

move :: Position -> Movement -> Position
move (horiz, depth, aim) movement =
  case movement of
    Forward x -> (horiz + x, depth + aim*x, aim)
    Down x    -> (horiz,     depth,         aim + x)
    Up x      -> (horiz,     depth,         aim - x)

solve :: [Movement] -> Int
solve movements = horiz * depth
  where (horiz, depth, _) = foldl move (0,0,0) movements

parseMovement :: String -> Movement
parseMovement (stripPrefix "forward " -> Just n) = Forward $ read n
parseMovement (stripPrefix "down " -> Just n)    = Down $ read n
parseMovement (stripPrefix "up " -> Just n)      = Up $ read n

main =
  do movements <- map parseMovement <$> lines <$> getContents
     print (solve movements)
