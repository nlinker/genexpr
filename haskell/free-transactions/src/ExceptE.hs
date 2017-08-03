module ExceptE where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

-- | source: http://codereview.stackexchange.com/a/108680
-- Since you need to abort your computation early and
-- return a value different from the final output,
-- the ExceptT transformer would be useful.

data E a
  = E1
  | E2
  | E3 a

data L
data R

f :: ExceptT (E L) IO ()
f = do
  liftIO (undefined :: IO Bool) >>= flip unless (throwE E1)
  liftIO (undefined :: IO Bool) >>= flip unless (throwE E2)
  liftIO (undefined :: IO (Either L R)) >>=
    either (throwE . E3) (liftIO . (undefined :: R -> IO ()))

-- If we want, we can then convert ExceptT (E L) IO () to IO (Maybe (E L)):

f' :: IO (Maybe (E L))
f' = either Just (const Nothing) <$> runExceptT f

-- If could be further polished by using helper functions
-- from Control.Conditional or a similar library, and by
-- extending the functions, for which undefined fills in, to
-- work within any MonadIO (if that's possible).
