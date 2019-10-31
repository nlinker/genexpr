module Debug where

import Control.Monad.ST.Strict (runST, ST)
import Data.StateRef (readRef, modifyRef, newRef)

class DebugMonad m where
    pdebug :: String -> m ()

instance DebugMonad (ST s) where
    pdebug _ = return ()

instance DebugMonad IO where
    pdebug = putStrLn

test :: Num a => a -> ST s a
test initV = do
    v <- newRef initV
    modifyRef v (+1)
    pdebug "debug"
    readRef v

-- testR :: a -> a
testR v = runST $ test v

