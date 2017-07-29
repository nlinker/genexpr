{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module T1005.Main where

import Debug.Trace
import Control.Applicative ((<$>), (<*>), pure, Applicative)
import Control.Exception   (handle, throwIO)
import Control.Monad       (forM, foldM)
import Data.Char           (isSpace)
import Data.Foldable       (foldl')
import Prelude             hiding (lookup, null)
import System.IO.Error     (isEOFError)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Prelude               as P (foldr, null)

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

-------------------------------------------------------
--------------- Dijkstra Algorithm---------------------
-------------------------------------------------------
-- Graph node is the pair of vertices (city, route)
-- Graph is the nodes and adjacency lists for all nodes
--data Node = Node Int Int  -- x y weight
--data Graph v = Graph [v] (M.Map v [v])
newtype Node = Node Int deriving (Eq, Ord, Show)
newtype Dist = Dist Int deriving (Eq, Ord, Show)

data Graph = Graph
  { nodes :: S.Set Node
  , arcs :: M.Map Node [Node]
  , dists :: M.Map (Node, Node) Dist
  } deriving (Show)

data Arc = Arc Node Dist deriving (Show)
type Path = M.Map Node Node
type Explored = S.Set Node
type PrioQueue = PSQ Node Dist

kickDijkstra :: IO Path
kickDijkstra = do
  g <- getGraph "dijkstra.txt"
  -- initial node 1
  let initial = Node 1
  let rest = S.delete initial $ nodes g
  let infinity node = node :-> Dist 999999999
  let heap = fromList $ (initial :-> Dist 0) : map infinity (S.toList rest)
  path <- mainLoop g heap S.empty M.empty
  print path
  undefined

mainLoop :: (Monad m) => Graph -> PrioQueue -> Explored -> Path -> m Path
mainLoop g heap exp path = do
  let bind' = findMin heap
  case bind' of
    Nothing ->
      return path
    Just (mn :-> md) -> do
      -- found minimal node with the distance
      let heap1 = deleteMin heap
      let as' = M.lookup mn (arcs g)
      let path1 = M.insert mn path
      undefined
--      let exp1 = S.insert mn exs
--      mainLoop g exp1 hp2 res1

-- updatePrioQueue takes minDist to newly explored node,
-- and list of all edges from the new node
updatePrioQueue :: [(Node, Dist)] -> Dist -> PrioQueue -> PrioQueue
updatePrioQueue nodes minDist heap = foldl' go heap nodes
  where
    up d p =
      let Dist md = minDist in
      let Dist nd = d in
      min p $ Dist (md + nd)
    go :: PrioQueue -> (Node, Dist) -> PrioQueue
    go hp (n, d) = adjust (up d) n hp


--mainLoop :: (Monad m) => Graph -> Explored -> PrioQueue -> Result -> m Result
--mainLoop g exs hp res = do
--  let bind' = findMin hp
--  case bind' of
--  where
--    Graph arcs = g
--    theArcs :: [Arc]
--    theArcs =
--    -- filter out newEdges that point to explored
--    notExplored (node, _) = not (IS.member node exs)
--    newCuttingEdges = filter notExplored (arcs !! i :: Arcs)
--    --newCuttingEdges = newEdges

-- foldl' :: (a -> b -> a) -> a -> [b] -> a

getGraph :: String -> IO Graph
getGraph path = do
  ls <- (map (BS.split '\t') . BS.lines) `fmap` BS.readFile path
  let g = Graph S.empty M.empty M.empty
  foldM processLine g ls
  where
    conv (x, y) = (Node x, Dist y)
    processLine :: (Monad m) => Graph -> [BS.ByteString] -> m Graph
    processLine g (x:xs) = do
      let an = Node $ convert x
      let as = map (conv . splitter) xs
      let updateArcs old (n, _) =
            case M.lookup an old of
              Just ns -> M.insert an (n:ns) old
              Nothing -> M.insert an [] old
      let updateDists old (n, d) = M.insert (an, n) d old
      let nodes1 = S.insert an (nodes g)
      let arcs1 = foldl' updateArcs (M.insert an [] $ arcs g) as
      let dists1 = foldl' updateDists (dists g) as
      return $ Graph nodes1 arcs1 dists1
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

parseRoute :: IO Route
parseRoute = do
  ki <- readWord
  stops <-
    forM [1 .. ki] $ \_ -> do
      x <- readWord
      y <- readWord
      return (x, y)
  return Route {..}

parseCtx :: IO Ctx
parseCtx = do
  n <- readWord
  e <- readWord
  m <- readWord
  rts <- mapM (const parseRoute) [1 .. m]
  return Ctx {..}

-------------------------------------------------------
--------------- Priority Queue ------------------------
-------------------------------------------------------

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
  prune k p t
    | p > pt         = emptySequ
    | otherwise      = traverse k p t
  traverse k p Start                     = singleSequ (k :-> p)
  traverse k p (LLoser _ k' p' tl _m tr) = prune k' p' tl <> traverse k p tr
  traverse k p (RLoser _ k' p' tl _m tr) = traverse k p tl <> prune k' p' tr

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
  prune k p t
    | p > pt    = emptySequ
    | otherwise = traverse k p t
  traverse k p Start
    | k `inrange` range = singleSequ (k :-> p)
    | otherwise         = emptySequ
  traverse k p (LLoser _ k' p' tl m tr) =
    guard (kl <= m) (prune k' p' tl) <> guard (m <= kr) (traverse k p tr)
  traverse k p (RLoser _ k' p' tl m tr) =
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