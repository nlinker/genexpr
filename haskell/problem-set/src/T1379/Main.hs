{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module T1379.Main where

import Control.Applicative ((<$>))
import Control.Exception   (handle, throwIO)
import Data.Char           (isSpace)
import Debug.Trace
import System.IO.Error     (isEOFError)

-- http://informatics.mccme.ru/moodle/mod/statements/view.php?chapterid=1379#1
-- frame is corners alternating with sides
type Frame = [Piece]

data Piece
  = C -- corner
  | S Int -- side without corners
  deriving (Eq, Show)

data Ctx = Ctx
  { x  :: Int
  , y  :: Int
  , ts :: [Int]
  }

main :: IO ()
main = do
  ctx <- parseCtx
  -- let ctx = ctx1
  let ans = calc ctx
  mapM_ (putStrLn . toStr) ans
  where
    toStr :: Bool -> String
    toStr True  = "yes"
    toStr False = "no"

-- expected: yes yes no yes yes no no no yes
ctx0 = Ctx {x = 20, y = 12, ts = [2, 3, 4, 5, 6, 7, 8, 9, 10]}
-- expected: [yes], [yes], [yes]
ctx1 = Ctx {x = 10, y = 7, ts = [5]}
ctx2 = Ctx {x = 11, y = 6, ts = [5]}
ctx3 = Ctx {x = 12, y = 5, ts = [5]}

calc :: Ctx -> [Bool]
calc ctx@Ctx {..} = map (calcOne ctx) ts

calcOne :: Ctx -> Int -> Bool
calcOne ctx@Ctx {..} t =
  let f0 = frame0 x y in
  let f1 = frame1 x y in
  divides && (hasTile t f0 || hasTile t f1)
  where
    -- quick check, the number of cells in frame
    -- should be divisible by the tile length
    divides = (2 * x + 2 * y - 4) `mod` t == 0

frame0 :: Int -> Int -> [Piece]
frame0 x y =
  let x2 = x - 2 in
  let y2 = y - 2 in
  [C, S x2, C, S y2, C, S x2, C, S y2]

frame1 :: Int -> Int -> [Piece]
frame1 x y =
  let x2 = x - 2 in
  let y2 = y - 2 in
  [S x2, C, S y2, C, S x2, C, S y2, C]

hasTile :: Int -> [Piece] -> Bool
hasTile t = tile
  where
--    tile ts | trace ("tile " ++ show ts) False = undefined
    tile [] = True
    tile (S 0:ts) = tile ts
    tile (S s:C:ts)   | s == (t - 1) = tile ts
    tile (C:S s:C:ts) | s == (t - 2) = tile ts
    tile (C:S s:ts)   | s >= t - 1   = tile $ S ((s + 1) `mod` t) : ts
    tile (S s:ts)     | s >= t       = tile $ S (s `mod` t) : ts
    tile _ = False

parseCtx :: IO Ctx
parseCtx = do
  x <- readWord
  y <- readWord
  n <- readWord :: IO Int
  ts <- mapM parseTile [1 .. n]
  return Ctx {..}
  where
    parseTile _ = readWord

readWord :: Read a => IO a
readWord = getWord >>= readIO

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
