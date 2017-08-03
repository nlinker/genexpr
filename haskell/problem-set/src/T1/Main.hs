module T1.Main where

main :: IO ()
main = do
  as <- getLine
  let a = read as :: Integer
  bs <- getLine
  let b = read bs :: Integer
  print (a + b)