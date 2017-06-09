module Sokoban where

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
    Char Direction
  | Hole
  | Stone
  | Wall
  | Empty
  | StoneOverHole
  | CharOverHole Direction
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
