module Main where

import Lib

main :: IO ()
main = do
  let rang p = return 1
  path <- someAlgorithm (Point 1 1) (Point 2 2) rang
  putStrLn $ "path = " <> show path
