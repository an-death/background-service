module Service.BackgroundService.Internal.ForkClosable
  ( forkClosable,
    ForkClosable (..),
  )
where

import Control.Monad.STM (check)
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Concurrent as UnliftIO
import qualified UnliftIO.Exception as UnliftIO
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)

data ForkClosable m = ForkClosable
  { stop :: !(m ()),
    status :: !(m Bool),
    threadId :: !UnliftIO.ThreadId
  }

forkClosable :: MonadUnliftIO m => m () -> m (ForkClosable m)
forkClosable action = do
  (status, wrap) <- newStatus
  threadId <- UnliftIO.forkIO (wrap action)
  pure ForkClosable {stop = stop threadId status, threadId = threadId, status = atomically $ readTVar status}
  where
    stop threadId status = do
      UnliftIO.killThread threadId
      atomically $ readTVar status >>= check . not

newStatus :: MonadUnliftIO m => m (TVar Bool, m a -> m a)
newStatus = do
  status' <- newTVarIO True
  pure $ (status', wrap status')
  where
    wrap status' action =
      UnliftIO.finally action (atomically $ writeTVar status' True)
