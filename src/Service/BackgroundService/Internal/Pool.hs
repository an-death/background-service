module Service.BackgroundService.Internal.Pool (poolCreate, withResource) where

import qualified Data.Pool as Pool
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO as UnliftIO

-- UnliftIO wrappers for Data.Pool
--
--

poolCreate :: MonadUnliftIO m => m a -> (a -> m ()) -> Int -> Int -> Double -> m (Pool.Pool a)
poolCreate create free maxResources timeout resourceTTL =
  UnliftIO.withRunInIO $ \run -> do
    let config =
          Pool.PoolConfig
            { createResource = run create,
              freeResource = run . free,
              poolCacheTTL = resourceTTL,
              poolMaxResources = maxResources,
              poolTimeoutConfig =
                Just
                  Pool.TimeoutConfig
                    { acquireResourceTimeout = timeout,
                      timeoutLabel = "BackgroundServicePool"
                    }
            }
     in Pool.newPool config

withResource :: MonadUnliftIO m => Pool.Pool a -> (a -> m r) -> m r
withResource pool action = UnliftIO.withRunInIO $ \run -> Pool.withResource pool (run . action)
