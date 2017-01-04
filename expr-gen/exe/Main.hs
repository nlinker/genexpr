module Main where

import Run
import Control.Monad.Random

main :: IO ()
main = run

-- size :: Num a => [a] -> Int
size :: Num a => [t] -> a
size xs = loop xs 0
  where
    loop [] acc = acc
    loop (_ : xs) acc = loop xs (acc + 1)
