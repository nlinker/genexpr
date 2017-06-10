{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Sokoban where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import Helper (str)
import Data.Vector (Vector)

mainS :: IO ()
mainS = undefined

data Direction = U | D | L | R
  deriving (Eq, Show)

data Point = Point
  { x :: Integer
  , y :: Integer
  } deriving (Eq, Show)

data Cell =
    Worker Direction
  | Hole
  | Box
  | Wall
  | Empty
  | BoxOnHole
  | WorkerOnHole Direction
  deriving (Eq, Show)

data Level = Level
 { cells        :: Vector (Vector Cell)
 , charPosition :: Point
 } deriving (Eq, Show)

delta :: Direction -> Point
delta U = Point   0  (-1)
delta D = Point   0    1
delta L = Point (-1)   0
delta R = Point   1    0

parseCell :: Char -> Maybe Cell
parseCell '@' = Just (Worker U)
parseCell '+' = Just (WorkerOnHole U)
parseCell ' ' = Just Empty
parseCell '#' = Just Wall
parseCell '.' = Just Hole
parseCell '$' = Just Box
parseCell '*' = Just BoxOnHole
parseCell _   = Nothing

parseLevel :: B.ByteString -> Maybe Level
parseLevel s = do
  let lns = C.lines s
  let chars = map C.unpack lns
  let ijs = [[(i, j) | i <- [0 ..]] | j <- [0 ..]]
  let elems :: [((Integer, Integer), Char)]
      elems = concat $ zipWith zip ijs chars
  Nothing


level :: B.ByteString
level = [str|
    #####
   ## . #
   # . $#
  ##$#..#
  #@$ * #
  # $   #
  ###   #
    #####
|]
