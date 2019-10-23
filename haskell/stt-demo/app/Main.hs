module Main where

import Lib

main :: IO ()
main = do
  path <- someAlgorithm (Point 1 1) (Point 2 2)
  putStrLn $ "path = " <> show path
