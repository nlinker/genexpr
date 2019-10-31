# stt-demo

This is demo to show the working example to use 
- `STT` (from `STMonadTrans` package)
- `PrimMonad` and `PrimState` (from `primitive` package)

Plus memoization is used for the step function^

```
buildAlg :: forall m . MonadState Integer m => Point -> m (Alg m Point)
buildAlg dst = return $ Alg { ... }
  where
    stepCache = makeCache

    step :: Direction -> Point -> m Point
    step d p = return $ memo2 stepCache go d p
    --           here!  ^^^^^                    
      where
        go d (Point i j) =
          case d of
            U -> Point (i - 1) (j)
            D -> Point (i + 1) (j)
            L -> Point (i) (j - 1)
            R -> Point (i) (j + 1)

    stopCond :: Point -> m Bool
    stopCond p = return $ dst == p
``` 

Profiling can be done according to
https://stackoverflow.com/questions/32123475/profiling-builds-with-stack