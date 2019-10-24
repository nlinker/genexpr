module Main where

import Lib
import Control.Monad (filterM)

main :: IO ()
main = do
  let src = Point 1 1
  let dst = Point 2 2
  let rang p = return
  solver <- buildMoveSolver [Point 1 2, Point 2 1]  
  path <- aStarFind solver src dst (return . (== dst))
  putStrLn $ "path = " <> show path

buildMoveSolver :: [Point] -> IO (AStarSolver IO Point)
buildMoveSolver walls = do
  let n = 5 
  let p2i (Point i j) = i * n + j
  let i2p k = Point (k `div` n) (k `mod` n)
  return $ AStarSolver {neighbors = neighbors, distance = distance, heuristic = heuristic, p2i = p2i, i2p = i2p}
  where
    neighbors p0 = do
      let isAccessible p =
            if p `elem` walls
              then return False
              else return True
      let neighs = map (movePoint p0) [U, D, L, R]
      filterM isAccessible neighs
    distance np p0 = return $ fromEnum (np /= p0)
    heuristic (Point i1 j1) (Point i2 j2) = return $ abs (i1 - i2) + abs (j1 - j2)
