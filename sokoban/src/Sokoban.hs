module Sokoban where

mainS :: IO ()
mainS = undefined

data Direction = U | D | L | R
  deriving (Eq, Show)

data Cell =
    Char Direction
  | Hole
  | Stone
  | Wall
  | Empty
  | StoneOverHole
  | CharOverHole Direction
  deriving (Eq, Show)

delta :: Direction -> (Integer, Integer)
delta U = (0, -1)
delta D = (0, 1)
delta L = (-1, 0)
delta R = (1, -1)

