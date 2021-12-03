module Main where

solve :: [Int] -> Int
solve [_] = 0
solve (x:xs) = increases
  where (_, increases) = foldl f (x, 0) xs
        f (last, incs) next = (next, if last < next then incs + 1 else incs)

main =
  do increases <- solve <$> map read <$> lines <$> getContents
     print increases
