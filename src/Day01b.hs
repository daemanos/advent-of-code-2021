module Main where

windows :: Int -> [a] -> [[a]]
windows _   [] = []
windows len xs = (take len xs):(windows len $ rest xs)
  where rest xs = if length xs > len then tail xs else []

solve :: [Int] -> Int
solve [_] = 0
solve (x:xs) = increases
  where (_, increases) = foldl f (x, 0) xs
        f (last, incs) next = (next, if last < next then incs + 1 else incs)

main =
  do xs <- map read <$> lines <$> getContents
     print (solve $ map sum $ windows 3 xs)
