{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module T1005.Main where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Exception   (handle, throwIO)
import Control.Monad       (foldM, forM)
import Data.Char           (isSpace)
import Data.Foldable       (foldl')
import Data.Function       (on)
import Data.List           (sortBy)
import Data.Maybe          (mapMaybe, fromJust)
import Debug.Trace
import Prelude             hiding (exp, lookup, null)
import System.IO.Error     (isEOFError)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Prelude               as P (foldr, null)

-- http://informatics.mccme.ru/moodle/mod/statements/view3.php?chapterid=1005#1
type TimeTable = M.Map (Int, Int) Int
data Ctx = Ctx
  { n  :: Int -- total number of towns
  , e  :: Int -- destination
  , m  :: Int -- the number of routes
  , tt :: TimeTable -- time table for the trains
  } deriving (Eq, Show)

newtype Node = Node Int deriving (Eq, Ord, Show)
newtype Dist = Dist Int deriving (Eq, Ord, Show)

-- Graph is the nodes and adjacency lists for all nodes
data Graph = Graph
  { nodes :: S.Set Node
  , arcs  :: Arcs
  } deriving (Show)

type Arcs = M.Map Node [(Node, Dist)]
type Path = M.Map Node (Node, Dist)
type Explored = S.Set Node
type PrioQueue = PSQ Node Dist

main :: IO ()
main = do
  -- ctx <- parseCtx
  let ctx = Ctx
        { n = 5
        , e = 3
        , m = 4
        , tt = M.fromList
          [ ((1,1),5),((1,2),10)
          , ((2,2),10),((2,4),15)
          , ((3,2),35),((3,3),20),((3,4),17),((3,5),0)
          , ((4,1),2),((4,3),40),((4,4),45)
          ]
        }
  len <- calc ctx
  print len

--undefined

-- useful functions
-- mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- compare :: Ord a => a -> a -> Ordering
-- curry :: ((a, b) -> c) -> a -> b -> c

calc :: (Monad m) => Ctx -> m Path
calc ctx@Ctx {..} = do
    -- use monad to make the syntax nicer
    let towns = [1 .. n]
    let routes = [1 .. m]
    let nodes = S.fromList $ map Node [1 .. m * n]
    let arcs = buildArcs
    let g = Graph nodes arcs
    traceShowM g
    dijkstra g (Node 16)
  where
    buildArcs = do
      let mkEmpty x = (Node x, [] :: [(Node, Dist)])
      let arcs0 = M.fromList $ map mkEmpty [1 .. m * n]
      -- apply row and column updates
      let arcs1 = foldl' updateArcsR arcs0 [1 .. m]
      let arcs2 = foldl' updateArcsC arcs1 [1 .. n]
      arcs2
    -- go by rows and update arcs
    updateArcsR as i = foldl' updateArcR as $ diffs $ buildRow ctx i
    -- go by columns and update arcs
    updateArcsC as j = foldl' updateArcC as $ diffs $ buildCol ctx j

-- update arcs using horizontal cell pair
updateArcR as (n1, n2, d) =
  let n1s = fromJust $ M.lookup n1 as
  in M.insert n1 ((n2, d) : n1s) as

-- update arcs using vertical cell pair
updateArcC as (n1, n2, d) =
    if d == Dist 0 then
      -- insert two arcs, back and forward
      let n1s = fromJust $ M.lookup n1 as in
      let n2s = fromJust $ M.lookup n2 as in
      M.insert n1 ((n2, d) : n1s) $
        M.insert n2 ((n1, d) : n2s) as
    else
      let n1s = fromJust $ M.lookup n1 as in
      M.insert n1 ((n2, d) : n1s) as

-- assumes the list is sorted by weights
-- [((9,15),(14,17)),((14,17),(19,45))] -> [(Node 9,Node 14,Dist 2),(Node 14,Node 19,Dist 28)]
diffs :: [(Int, Int)] -> [(Node, Node, Dist)]
diffs xs =
  let diffPair (n1, w1) (n2, w2) = (Node n1, Node n2, Dist $ w2 - w1)
  in zipWith diffPair xs (tail xs)

-- return absolute node number
-- e.g. for matrix 4x5
-- cell (4,2) will be 17
-- cell (3,5) will be 15
cell :: Ctx -> Int -> Int -> Int
cell ctx i j = (i - 1) * n ctx + j

buildRow :: Ctx -> Int -> [(Int, Int)]
buildRow ctx@Ctx{..} i =
  let select j = (\w -> (cell ctx i j, w)) `fmap` M.lookup (i, j) tt
  in sortBy (compare `on` snd) $ mapMaybe select [1 .. n]

buildCol :: Ctx -> Int -> [(Int, Int)]
buildCol ctx@Ctx{..} j =
  let select i = (\w -> (cell ctx i j, w)) `fmap` M.lookup (i, j) tt
  in sortBy (compare `on` snd) $ mapMaybe select [1 .. m]

-------------------------------------------------------
--------------- Dijkstra Algorithm---------------------
-------------------------------------------------------
-- some test for the algorithm
kickDijkstra :: IO ()
kickDijkstra = do
  let initial = Node 16
  g <- loadGraph "dijkstra.txt"
  traceShowM g
  p <- dijkstra g initial
  traceShowM p

infinity :: Dist
infinity = Dist 999999999

dijkstra :: (Monad m) => Graph -> Node -> m Path
dijkstra g initial = do
  let rest = S.delete initial $ nodes g
  let inf cell = cell :-> infinity
  let heap = fromList $ (initial :-> Dist 0) : map inf (S.toList rest)
  let path = M.empty :: Path
  mainLoop g heap S.empty M.empty

mainLoop :: (Monad m) => Graph -> PrioQueue -> Explored -> Path -> m Path
mainLoop g heap exp path = do
  let bind' = findMin heap
  case bind' of
    Nothing ->
      return path
    Just (mn :-> md) ->
      -- warning: causes (Node 14,(Node 15,Dist 17)) which is wrong
      -- if md == infinity then return path else
      do
        -- found minimal node with the distance
        let heap1 = deleteMin heap
        let as' = M.lookup mn (arcs g)
        let (heap2, path2) = case as' of
              -- lookup into arcs in g for min node failed
              -- Nothing -> error "inconsistent data"
              Nothing -> (heap1, path)
              Just as -> (updateHeap heap1 md as, updatePath path mn as)
        let exp2 = S.insert mn exp
        traceShowM $ "node=(" ++ show mn ++ "," ++ show md ++ "\theap2=" ++ show heap2
        mainLoop g heap2 exp2 path2

updatePath :: Path -> Node -> [(Node, Dist)] -> Path
updatePath path minNode = foldl' yo path
  where
    yo :: Path -> (Node, Dist) -> Path
    yo p (n, d) = M.insert n (minNode, d) p

-- updateHeap takes minDist to newly explored node,
-- and list of all edges from the new node
updateHeap :: PrioQueue -> Dist -> [(Node, Dist)] -> PrioQueue
updateHeap heap minDist = foldl' go heap
  where
    -- e is the old distance was in the queue
    up d e =
      let Dist md = minDist in
      let Dist nd = d in
      min e $ Dist (md + nd)
    go :: PrioQueue -> (Node, Dist) -> PrioQueue
    go h (n, d) = adjust (up d) n h

-- Example file
--1	2:10	3:20
--2	4:15	5:50
--3	4:30
--4	5:30
--5	6:5
--6	7:2
--7
loadGraph :: String -> IO Graph
loadGraph path = do
  ls <- (map (BS.split '\t') . BS.lines) `fmap` BS.readFile path
  let g = Graph S.empty M.empty
  foldM processLine g ls
  where
    conv (x, y) = (Node x, Dist y)
    processLine :: (Monad m) => Graph -> [BS.ByteString] -> m Graph
    processLine g (x:xs) = do
      let an = Node $ convert x
      let as = map (conv . splitter) xs
      let updateArcs old (n, d) =
            case M.lookup an old of
              Just ns -> M.insert an ((n, d):ns) old
              Nothing -> M.insert an [] old
      let updateDists old (n, d) = M.insert (an, n) d old
      let nodes1 = S.insert an (nodes g)
      let arcs1 = foldl' updateArcs (M.insert an [] $ arcs g) as
      return $ Graph nodes1 arcs1
    processLine g [] = error "processLine"

convert :: BS.ByteString -> Int
convert = maybe (error "can't read Int") fst . BS.readInt

splitter :: BS.ByteString -> (Int, Int)
splitter c = (convert x, convert y)
  where
    (x:y:_) = BS.split ':' c

-----------------------------------------------------------
------------------- stuff for parsing ---------------------
-----------------------------------------------------------
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

-- carry the route number
parseRoute :: Int -> IO (Int, [(Int,Int)])
parseRoute ri = do
  (ki :: Int) <- readWord
  stops <-
    forM [1 .. ki] $ \_ -> do
      x <- readWord
      y <- readWord
      return (x, y)
  return (ri, stops)

-- example input
--5 3 4
--2	1 5	2 10
--2	2 10	4 15
--4	2 35	3 20	4 17	5 0
--3	1 2	3 40	4 45
parseCtx :: IO Ctx
parseCtx = do
  n <- readWord
  e <- readWord
  m <- readWord
  rawTts <- mapM parseRoute [1 .. m]
  let tt = foldl' updateTt M.empty rawTts
  return Ctx {..}
  where
    -- updateTt :: TimeTable -> (Int, [(Int, Int)]) -> TimeTable
    updateTt tt (ri, stops) = foldl' (updateTtRow ri) tt stops
    -- updateTtRow :: Int -> TimeTable -> (Int, Int) -> TimeTable
    updateTtRow ri tt (i, t) = M.insert (ri, i) t tt


-------------------------------------------------------
--------------- Priority Queue ------------------------
-------------------------------------------------------
-- the priority queue is adapted from
-- http://hackage.haskell.org/package/PSQueue-1.1/docs/src/Data-PSQueue.html

-- | @k :-> p@ binds the key @k@ with the priority @p@.
data Binding k p = k :-> p deriving (Eq, Ord, Show, Read)

infix 0 :->

key  :: Binding k p -> k
key  (k :-> _) =  k

prio :: Binding k p -> p
prio (_ :-> p) =  p

data PSQ k p = Void | Winner k p (LTree k p) k

instance (Show k, Show p, Ord k, Ord p) => Show (PSQ k p) where
  show = show . toAscList
  --show Void = "[]"
  --show (Winner k1 p lt k2) = "Winner "++show k1++" "++show p++" ("++show lt++") "++show k2

-- | /O(1)/ The number of bindings in a queue.
size :: PSQ k p -> Int
size Void              = 0
size (Winner _ _ lt _) = 1 + size' lt

-- | /O(1)/ True if the queue is empty.
null :: PSQ k p -> Bool
null Void        = True
null (Winner {}) = False

-- | /O(log n)/ The priority of a given key, or Nothing if the key is not
-- bound.
lookup :: (Ord k, Ord p) => k -> PSQ k p -> Maybe p
lookup k q =
  case tourView q of
    Null -> fail "PSQueue.lookup: Empty queue"
    Single k' p
      | k == k'   -> return p
      | otherwise -> fail "PSQueue.lookup: Key not found"
    tl `Play` tr
      | k <= maxKey tl -> lookup k tl
      | otherwise      -> lookup k tr

empty :: (Ord k, Ord p) => PSQ k p
empty = Void

-- | O(1) Build a queue with one binding.
singleton :: (Ord k, Ord p) => k -> p -> PSQ k p
singleton k p =  Winner k p Start k

-- | /O(log n)/ Insert a binding into the queue.
insert :: (Ord k, Ord p) => k -> p -> PSQ k p -> PSQ k p
insert k p q =
  case tourView q of
    Null -> singleton k p
    Single k' p' ->
      case compare k k' of
        LT -> singleton k  p  `play` singleton k' p'
        EQ -> singleton k  p
        GT -> singleton k' p' `play` singleton k  p
    tl `Play` tr
      | k <= maxKey tl -> insert k p tl `play` tr
      | otherwise      -> tl `play` insert k p tr

-- | /O(log n)/ Remove a binding from the queue.
delete :: (Ord k, Ord p) => k -> PSQ k p -> PSQ k p
delete k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k == k'   -> empty
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> delete k tl `play` tr
      | otherwise      -> tl `play` delete k tr

-- | /O(log n)/ Adjust the priority of a key.
adjust ::  (Ord p, Ord k) => (p -> p) -> k -> PSQ k p -> PSQ k p
adjust f = adjustWithKey (\_ p -> f p)

-- | /O(log n)/ Adjust the priority of a key.
adjustWithKey :: (Ord k, Ord p) => (k -> p -> p) -> k -> PSQ k p -> PSQ k p
adjustWithKey f k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k == k'   -> singleton k' (f k p)
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> adjustWithKey f k tl `unsafePlay` tr
      | otherwise      -> tl `unsafePlay` adjustWithKey f k tr

-- | /O(log n)/ The expression (@update f k q@) updates the
-- priority @p@ bound @k@ (if it is in the queue). If (@f p@) is 'Nothing',
-- the binding is deleted. If it is (@'Just' z@), the key @k@ is bound
-- to the new priority @z@.

update :: (Ord k, Ord p) => (p -> Maybe p) -> k -> PSQ k p -> PSQ k p
update f = updateWithKey (\_ p -> f p)

-- | /O(log n)/. The expression (@updateWithKey f k q@) updates the
-- priority @p@ bound @k@ (if it is in the queue). If (@f k p@) is 'Nothing',
-- the binding is deleted. If it is (@'Just' z@), the key @k@ is bound
-- to the new priority @z@.

updateWithKey :: (Ord k, Ord p) => (k -> p -> Maybe p) -> k -> PSQ k p -> PSQ k p
updateWithKey f k q =
  case tourView q of
    Null -> empty
    Single k' p
      | k==k' -> case f k p of
                  Nothing -> empty
                  Just p' -> singleton k p'
      | otherwise -> singleton k' p
    tl `Play` tr
      | k <= maxKey tl -> updateWithKey f k tl `unsafePlay` tr
      | otherwise      -> tl `unsafePlay` updateWithKey f k tr

-- | /O(n)/ The keys of a priority queue
keys :: (Ord k, Ord p) => PSQ k p -> [k]
keys = map key . toList

-- | /O(n log n)/ Build a queue from a list of bindings.
fromList :: (Ord k, Ord p) => [Binding k p] -> PSQ k p
fromList = P.foldr (\(k:->p) q -> insert k p q) empty

-- | /O(n)/ Build a queue from a list of bindings in order of
-- ascending keys. The precondition that the keys are ascending is not checked.
fromAscList :: (Ord k, Ord p) => [Binding k p] -> PSQ k p
fromAscList = fromDistinctAscList . stripEq
  where stripEq []     = []
        stripEq (x:xs) = stripEq' x xs
        stripEq' x' []     = [x']
        stripEq' x' (x:xs)
          | x' == x   = stripEq' x' xs
          | otherwise = x' : stripEq' x xs

-- | /O(n)/ Build a queue from a list of distinct bindings in order of
-- ascending keys. The precondition that keys are distinct and ascending is not checked.
fromDistinctAscList :: (Ord k, Ord p) => [Binding k p] -> PSQ k p
fromDistinctAscList = foldm unsafePlay empty . map (\(k:->p) -> singleton k p)

-- Folding a list in a binary-subdivision scheme.
foldm :: (a -> a -> a) -> a -> [a] -> a
foldm f e x
  | P.null  x             = e
  | otherwise             = fst (rek (length x) x)
  where
    rek 1 (a : as)    = (a, as)
    rek n as          = (a1 `f` a2, as2)
      where
        m         = n `div` 2
        (a1, as1) = rek (n - m) as
        (a2, as2) = rek m       as1

-- | /O(n)/ Convert a queue to a list.
toList :: (Ord k, Ord p) => PSQ k p -> [Binding k p]
toList = toAscList

-- | /O(n)/ Convert a queue to a list in ascending order of keys.
toAscList :: (Ord k, Ord p) => PSQ k p -> [Binding k p]
toAscList q  = seqToList (toAscLists q)

toAscLists :: (Ord k, Ord p) => PSQ k p -> Sequ (Binding k p)
toAscLists q = case tourView q of
  Null         -> emptySequ
  Single k p   -> singleSequ (k :-> p)
  tl `Play` tr -> toAscLists tl <> toAscLists tr

-- | /O(n)/ Convert a queue to a list in descending order of keys.
toDescList :: (Ord k, Ord p) => PSQ k p -> [ Binding k p ]
toDescList q = seqToList (toDescLists q)

toDescLists :: (Ord k, Ord p) => PSQ k p -> Sequ (Binding k p)
toDescLists q = case tourView q of
  Null         -> emptySequ
  Single k p   -> singleSequ (k :-> p)
  tl `Play` tr -> toDescLists tr <> toDescLists tl


-- | /O(1)/ The binding with the lowest priority.
findMin :: (Ord k, Ord p) => PSQ k p -> Maybe (Binding k p)
findMin Void             = Nothing
findMin (Winner k p t m) = Just (k :-> p)

-- | /O(log n)/ Remove the binding with the lowest priority.
deleteMin :: (Ord k, Ord p) => PSQ k p -> PSQ k p
deleteMin Void             = Void
deleteMin (Winner k p t m) = secondBest t m

-- | /O(log n)/ Retrieve the binding with the least priority, and the rest of
-- the queue stripped of that binding.
minView :: (Ord k, Ord p) => PSQ k p -> Maybe (Binding k p, PSQ k p)
minView Void             = Nothing
minView (Winner k p t m) = Just ( k :-> p , secondBest t m )

secondBest :: (Ord k, Ord p) => LTree k p -> k -> PSQ k p
secondBest Start _m                  = Void
secondBest (LLoser _ k p tl m tr) m' = Winner k p tl m `play` secondBest tr m'
secondBest (RLoser _ k p tl m tr) m' = secondBest tl m `play` Winner k p tr m'


-- | /O(r(log n - log r)/ @atMost p q@ is a list of all the bindings in @q@ with
-- priority less than @p@, in order of ascending keys.
-- Effectively,
-- @  atMost p' q = filter (\\(k:->p) -> p<=p') . toList  @
atMost :: (Ord k, Ord p) => p -> PSQ k p -> [Binding k p]
atMost pt q = seqToList (atMosts pt q)

atMosts :: (Ord k, Ord p) => p -> PSQ k p -> Sequ (Binding k p)
atMosts _pt Void  = emptySequ
atMosts pt (Winner k p t _) =  prune k p t
  where
  prune _k _p _t
    | p > pt         = emptySequ
    | otherwise      = traverse k p t
  traverse _k _p Start                     = singleSequ (k :-> p)
  traverse _k _p (LLoser _ k' p' tl _m tr) = prune k' p' tl <> traverse k p tr
  traverse _k _p (RLoser _ k' p' tl _m tr) = traverse k p tl <> prune k' p' tr

-- | /O(r(log n - log r))/ @atMostRange p (l,u) q@ is a list of all the bindings in
-- @q@ with a priority less than @p@ and a key in the range @(l,u)@ inclusive.
-- Effectively,
-- @ atMostRange p' (l,u) q = filter (\\(k:->p) -> l<=k && k<=u ) . 'atMost' p' @
atMostRange :: (Ord k, Ord p) => p -> (k, k) -> PSQ k p -> [Binding k p]
atMostRange pt (kl, kr) q = seqToList (atMostRanges pt (kl, kr) q)

atMostRanges :: (Ord k, Ord p) => p -> (k, k) -> PSQ k p -> Sequ (Binding k p)

atMostRanges _pt _range Void = emptySequ
atMostRanges pt range@(kl, kr) (Winner k p t _) = prune k p t
  where
  prune _k _p _t
    | p > pt    = emptySequ
    | otherwise = traverse k p t
  traverse _k _p Start
    | k `inrange` range = singleSequ (k :-> p)
    | otherwise         = emptySequ
  traverse _k _p (LLoser _ k' p' tl m tr) =
    guard (kl <= m) (prune k' p' tl) <> guard (m <= kr) (traverse k p tr)
  traverse _k _p (RLoser _ k' p' tl m tr) =
    guard (kl <= m) (traverse k p tl) <> guard (m <= kr) (prune k' p' tr)

inrange :: (Ord a) => a -> (a, a) -> Bool
a `inrange` (l, r)  =  l <= a && a <= r

------- Internals -----

type Size = Int
data LTree k p = Start
               | LLoser {-# UNPACK #-}!Size !k !p (LTree k p) !k (LTree k p)
               | RLoser {-# UNPACK #-}!Size !k !p (LTree k p) !k (LTree k p)

size' :: LTree k p -> Size
size' Start                = 0
size' (LLoser s _ _ _ _ _) = s
size' (RLoser s _ _ _ _ _) = s

left, right :: LTree a b -> LTree a b

left  Start                  =  error "left: empty loser tree"
left  (LLoser _ _ _ tl _ _ ) =  tl
left  (RLoser _ _ _ tl _ _ ) =  tl

right Start                  =  error "right: empty loser tree"
right (LLoser _ _ _ _  _ tr) =  tr
right (RLoser _ _ _ _  _ tr) =  tr

maxKey :: PSQ k p -> k
maxKey Void                =  error "maxKey: empty queue"
maxKey (Winner _k _p _t m) =  m

lloser, rloser :: k -> p -> LTree k p -> k -> LTree k p -> LTree k p
lloser k p tl m tr =  LLoser (1 + size' tl + size' tr) k p tl m tr
rloser k p tl m tr =  RLoser (1 + size' tl + size' tr) k p tl m tr

--balance factor
omega :: Int
omega = 4

lbalance, rbalance ::
  (Ord k, Ord p) => k-> p -> LTree k p -> k -> LTree k p -> LTree k p

lbalance k p l m r
  | size' l + size' r < 2     = lloser        k p l m r
  | size' r > omega * size' l = lbalanceLeft  k p l m r
  | size' l > omega * size' r = lbalanceRight k p l m r
  | otherwise               = lloser        k p l m r

rbalance k p l m r
  | size' l + size' r < 2     = rloser        k p l m r
  | size' r > omega * size' l = rbalanceLeft  k p l m r
  | size' l > omega * size' r = rbalanceRight k p l m r
  | otherwise               = rloser        k p l m r

lbalanceLeft  k p l m r
  | size' (left r) < size' (right r) = lsingleLeft  k p l m r
  | otherwise                      = ldoubleLeft  k p l m r

lbalanceRight k p l m r
  | size' (left l) > size' (right l) = lsingleRight k p l m r
  | otherwise                      = ldoubleRight k p l m r

rbalanceLeft  k p l m r
  | size' (left r) < size' (right r) = rsingleLeft  k p l m r
  | otherwise                      = rdoubleLeft  k p l m r

rbalanceRight k p l m r
  | size' (left l) > size' (right l) = rsingleRight k p l m r
  | otherwise                      = rdoubleRight k p l m r

lsingleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3)
  | p1 <= p2  = lloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
  | otherwise = lloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3
lsingleLeft _ _ _ _ Start = error "lsingleLeft"

lsingleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rloser k2 p2 (lloser k1 p1 t1 m1 t2) m2 t3

rsingleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  rloser k1 p1 (rloser k2 p2 t1 m1 t2) m2 t3
rsingleLeft _ _ _ _ Start = error "rsingleLeft"

rsingleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rloser k2 p2 (rloser k1 p1 t1 m1 t2) m2 t3

lsingleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k2 p2 t1 m1 (lloser k1 p1 t2 m2 t3)
lsingleRight _ _ Start _ _ = error "lsingleRight"

lsingleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)

rsingleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)
rsingleRight k1 p1 Start m2 t3 = error "rsingleRight"

rsingleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3
  | p1 <= p2  = rloser k1 p1 t1 m1 (lloser k2 p2 t2 m2 t3)
  | otherwise = rloser k2 p2 t1 m1 (rloser k1 p1 t2 m2 t3)

ldoubleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  lsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)
ldoubleLeft k1 p1 t1 m1 Start = error "ldoubleLeft"

ldoubleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  lsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

ldoubleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3
ldoubleRight k1 p1 Start m2 t3 = error "ldoubleRight"

ldoubleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  lsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

rdoubleLeft k1 p1 t1 m1 (LLoser _ k2 p2 t2 m2 t3) =
  rsingleLeft k1 p1 t1 m1 (lsingleRight k2 p2 t2 m2 t3)
rdoubleLeft k1 p1 t1 m1 Start = error "rdoubleLeft"

rdoubleLeft k1 p1 t1 m1 (RLoser _ k2 p2 t2 m2 t3) =
  rsingleLeft k1 p1 t1 m1 (rsingleRight k2 p2 t2 m2 t3)

rdoubleRight k1 p1 (LLoser _ k2 p2 t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 (lsingleLeft k2 p2 t1 m1 t2) m2 t3
rdoubleRight k1 p1 Start m2 t3 = error "rdoubleRight"

rdoubleRight k1 p1 (RLoser _ k2 p2 t1 m1 t2) m2 t3 =
  rsingleRight k1 p1 (rsingleLeft k2 p2 t1 m1 t2) m2 t3

play :: (Ord k, Ord p) => PSQ k p -> PSQ k p -> PSQ k p

Void `play` t' = t'
t `play` Void  = t

Winner k p t m  `play`  Winner k' p' t' m'
  | p <= p'   = Winner k  p  (rbalance k' p' t m t') m'
  | otherwise = Winner k' p' (lbalance k  p  t m t') m'

unsafePlay :: (Ord k, Ord p) => PSQ k p -> PSQ k p -> PSQ k p

Void `unsafePlay` t' =  t'
t `unsafePlay` Void  =  t

Winner k p t m  `unsafePlay`  Winner k' p' t' m'
  | p <= p'   = Winner k  p  (rbalance k' p' t m t') m'
  | otherwise = Winner k' p' (lbalance k  p  t m t') m'

data TourView k p = Null | Single k p | PSQ k p `Play` PSQ k p

tourView :: (Ord k) => PSQ k p -> TourView k p

tourView Void                  =  Null
tourView (Winner k p Start _m) =  Single k p

tourView (Winner k p (RLoser _ k' p' tl m tr) m') =
  Winner k  p  tl m `Play` Winner k' p' tr m'

tourView (Winner k p (LLoser _ k' p' tl m tr) m') =
  Winner k' p' tl m `Play` Winner k  p  tr m'

-- Hughes's efficient sequence type --

emptySequ  :: Sequ a
singleSequ :: a -> Sequ a
(<>)       :: Sequ a -> Sequ a -> Sequ a
seqFromList   :: [a] -> Sequ a
seqFromListT  :: ([a] -> [a]) -> Sequ a
seqToList     :: Sequ a -> [a]

infixr 5 <>

newtype Sequ a  =  Sequ ([a] -> [a])

emptySequ = Sequ id
singleSequ a = Sequ (\as -> a : as)
Sequ x1 <> Sequ x2 = Sequ (x1 . x2)
seqFromList as = Sequ (\as' -> as ++ as')
seqFromListT = Sequ
seqToList (Sequ x) = x []

instance Show a => Show (Sequ a) where
    showsPrec d a = showsPrec d (seqToList a)

guard :: Bool -> Sequ a -> Sequ a
guard False _as = emptySequ
guard True  as  = as


-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
    return = Identity
    m >>= k  = k (runIdentity m)
