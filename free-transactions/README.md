# How to make transactions in Free monads

Typical DSL algebra looks like this:
```
type Action n v i = FreeT (ActionF n v i)

data ActionF n v i a =
  NewNode v (n -> a) |
  GetValue n (v -> a) |
  SetValue n v a |
  GetRoot (n -> a) |
  GetTargets n i ([n] -> a) |
  AddTarget n n a |
  RemoveTarget n n a |
  Remove n a |
  GetStats ((Int, Int, Int) -> a)
  deriving (Functor)

makeFree ''ActionF
```
But if we extend this DSL with `Transact Action` variant, it does not typecheck.

There is a variant to use GADTs to encode the algebra:
```
-- from https://github.com/dhess/free-experiments/blob/3e878ec743df9045ea9b76c85f8184e38d0807e2/src/RetryTransTH.hs
data RetryF m next where
  Output    :: String -> next -> RetryF m next
  Input     :: Read a => (a -> next) -> RetryF m next
  WithRetry :: (RetryT m) m a -> (a -> next) -> RetryF m next
  Retry     :: RetryF m next

-- data (Read a) => RetryF m a next = Output String next
--                                  | Input (a -> next)
--                                  | WithRetry ((RetryT m) m a) (a -> next)
--                                  | Retry

-- type RetryT m = FreeT (RetryF m a)
type RetryT m = FreeT (RetryF m)

instance Functor (RetryF m) where
  fmap f (Output s x) = Output s (f x)
  fmap f (Input g) = Input (f . g)
  fmap f (WithRetry block g) = WithRetry block (f . g)
  fmap _ Retry = Retry
```
which works and the example is here.
