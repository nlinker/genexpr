
module T113316.Main where

main :: IO ()
main = do
  line <- getLine
  let [n, k] = convert line :: [Integer]
  print $ calc n k

calc :: Integer -> Integer -> Integer
calc n k =
  if n `mod` 2 == 1
    then min (k + 1) n
    else min (k + 1) (n `div` 2)

convert :: Read a => String -> [a]
convert = map read . words