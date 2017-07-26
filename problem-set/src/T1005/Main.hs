{-# LANGUAGE RecordWildCards #-}

module T1005.Main where

import Control.Applicative ((<$>))
import Control.Exception   (handle, throwIO)
import Control.Monad       (forM)
import Data.Char           (isSpace)
import System.IO.Error     (isEOFError)

-- http://informatics.mccme.ru/moodle/mod/statements/view3.php?chapterid=1005#1

data Route = Route
  { ki    :: Integer -- how many elements in stops
  , stops :: [(Integer, Integer)] -- train route, [(town number, time)]
  } deriving (Eq, Show)

data Ctx = Ctx
  { n   :: Integer -- total number of towns
  , e   :: Integer -- destination
  , m   :: Integer -- the number of routes
  , rts :: [Route] -- time table for the trains
  } deriving (Eq, Show)

main :: IO ()
main = do
  ctx <- parseCtx
  print $ calc ctx

calc :: Ctx -> Integer
calc _ = -1

floydWarshall :: (Ord t, Num t) => Int -> Int -> [[t]] -> (t, [Int])
floydWarshall start end graph = (dist, [start] ++ route ++ [end])
  where
    dist = shortest (start, end, length graph) graph
    route = path (start, end, length graph) graph

-- Calculates the value of shortest route
shortest :: (Ord s, Num s) => (Int, Int, Int) -> [[s]] -> s
shortest (i, j, 0) g = g !! (i - 1) !! (j - 1) -- Transition value from graph
shortest (i, j, k) g =
  min (shortest (i, j, k - 1) g) $ (shortest (i, k, k - 1) g) + (shortest (k, j, k - 1) g)

-- Reconstructs the shortest path
path :: (Ord a, Num a) => (Int, Int, Int) -> [[a]] -> [Int]
path (_, _, 0) _ = []
path (i, j, k) g
  | direct < step = path (i, j, k - 1) g
  | otherwise = (path (i, k, k - 1) g) ++ [k] ++ (path (k, j, k - 1) g)
  where
    direct = shortest (i, j, k - 1) g
    step = (shortest (i, k, k - 1) g) + (shortest (k, j, k - 1) g)

-- stuff for parsing
getWord :: IO String
getWord =
  handle handleEOF $ do
    c <- getChar
    if isSpace c
      then return []
      else (c :) <$> getWord
  where
    handleEOF e =
      if isEOFError e
        then return []
        else throwIO e

readWord :: Read a => IO a
readWord = getWord >>= readIO

readList :: Read a => String -> [a]
readList = map read . words

parseRoute :: IO Route
parseRoute = do
  ki <- readWord
  stops <-
    forM [1 .. ki] $ \_ -> do
      x <- readWord
      y <- readWord
      return (x, y)
  return $ Route {..}

parseCtx :: IO Ctx
parseCtx = do
  n <- readWord
  e <- readWord
  m <- readWord
  rts <- mapM (\_ -> parseRoute) [1 .. m]
  return $ Ctx {..}
