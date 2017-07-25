module Main where

data Route = Route
  { ki :: Integer
  , stops :: [(Integer, Integer)]
  } deriving (Show, Read)

data Ctx = Cts
  { n :: Integer
  , e :: Integer
  , m :: Integer -- the number of routes
  , routes :: [Route]
  } deriving (Show, Read)

handle :: String -> String
handle _ = show (-1 :: Integer)

main :: IO ()
main = interact handle