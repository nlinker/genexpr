{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Run where

import           Development.GitRev (gitHash, gitBranch, gitDirty)
import           Data.Version       (showVersion)
import qualified Paths_expr_gen as Paths

run :: IO ()
run =
  putStrLn versionOutput

versionOutput :: String
versionOutput =
  "version: " ++ full
  where
    isInGit = $(gitHash) /= "UNKNOWN"
    full = if isInGit
      then "version: " ++ showVersion Paths.version ++ "\n" ++
           "rev: " ++ $(gitHash) ++ (if $(gitDirty) then " (dirty)" else "") ++ "\n" ++
           "branch: " ++ $(gitBranch)
      else "version: " ++ showVersion Paths.version

-- merge sort, just function for tests

msort :: Ord a => [a] -> [a]
msort xs = merging [ [x] | x <- xs ]

merging :: Ord a => [[a]] -> [a]
merging []   = []
merging [xs] = xs
merging xss  = merging (sweep xss)

sweep :: Ord a => [[a]] -> [[a]]
sweep []          = []
sweep [xs]        = [xs]
sweep (xs:ys:xss) = merge xs ys : sweep xss

merge :: Ord a => [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys
