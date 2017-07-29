{-# LANGUAGE RecordWildCards #-}

module T1005.Main where

import Control.Applicative ((<$>))
import Control.Exception   (handle, throwIO)
import Control.Monad       (forM)
import Data.Char           (isSpace)
import System.IO.Error     (isEOFError)

import qualified Data.Map as M

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

heapTest :: IO ()
heapTest = do
  let h1 = Empty :: Heap Integer
  let h2 = foldr insert h1 [99 .. 1]
  let h3 = insert 1 h2
  let h4 = insert 2 h3
  let (m1, h5) = extractMin h4
  let (m2, h6) = extractMin h5
  let (m3, h7) = extractMin h6
  let (m4, h8) = extractMin h7
  print h1
  print h2
  print h3
  print h4
  print (m1, h5)
  print (m2, h6)
  print (m3, h7)
  print (m4, h8)

-- | The basic heap type
data Heap a
  = Empty
  | Heap a [Heap a]
  deriving (Show)

extractMin :: Ord a => Heap a -> (a, Heap a)
extractMin h =
  case findMin h of
    Just m -> (m, deleteMin h)
    Nothing -> error "heap is empty: invalid extractMin"

isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty _     = False

findMin :: Ord a => Heap a -> Maybe a
findMin (Heap h _) = Just h
findMin Empty      = Nothing

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Heap x hs1) h2@(Heap y hs2)
  | x < y = Heap x (h2 : hs1)
  | otherwise = Heap y (h1 : hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []         = Empty
mergePairs [h]        = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap _x hs) = mergePairs hs
deleteMin Empty        = error "heap is empty: invalid operation deleteMin"

-- dijkstra algorithm
type Node = (Int, Int)

-- | Labeled node, path and tree
type LNode a = (Node, a)
newtype LPath a = LP { unLPath :: [LNode a] }
type LRTree a = [LPath a]
data Graph a = Graph [a] (M.Map a [a])

--expand :: Real b => b -> LPath b -> Context a b -> [H.Heap (LPath b)]
--expand d p (_,_,_,s) = map (\(l,v)->H.unit ((v,l+d):p)) s
--
--dijkstra :: Heap (LPath b) -> Graph a b -> LRTree b
--dijkstra h g | isEmpty h || isEmpty g = []
--dijkstra h g =
--    case match v g of
--         (Just c,g')  -> p:dijkstra (H.mergeAll (h':expand d p c)) g'
--         (Nothing,g') -> dijkstra h' g'
--    where (p@((v,d):_),h') = splitMin h
--
--spTree :: Real b => Node -> Graph a b -> LRTree b
--spTree v = dijkstra (H.unit [(v,0)])
--
--spLength :: Real b => Node -> Node -> Graph a b -> b
--spLength s t = getDistance t . spTree s
--
--sp :: Real b => Node -> Node -> Graph a b -> Path
--sp s t = map fst . getLPath t . spTree s
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
