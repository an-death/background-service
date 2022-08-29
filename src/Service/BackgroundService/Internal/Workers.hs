module Service.BackgroundService.Internal.Workers
  ( workerSupervisor,
    Worker (..),
  )
where

import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Pool (Pool)
import Service.BackgroundService.Internal.ForkClosable (ForkClosable (..), forkClosable)
import qualified Service.BackgroundService.Internal.Pool as Pool
import qualified UnliftIO.Concurrent as UnliftIO
import qualified UnliftIO.Timeout as UnliftIO

data Worker m a = Worker
  { workerId :: !UnliftIO.ThreadId,
    workerCommunicate :: a -> m (),
    workerKill :: m (),
    workerStatus :: m Bool
  }

workerSupervisor :: MonadUnliftIO m => UnliftIO.Chan a -> Int -> Int -> (a -> m ()) -> m (Worker m a)
workerSupervisor jobsChan workerCount workerTimeout handler = do
  ForkClosable {status, stop, threadId} <- forkClosable workerSupervisor'

  pure
    Worker
      { workerId = threadId,
        workerKill = stop,
        workerStatus = status,
        workerCommunicate = UnliftIO.writeChan jobsChan
      }
  where
    workerSupervisor' = do
      -- Create worker pool to control count of workers
      workerPool <- newWorkerPool (newWorker handler workerTimeout) workerCount workerTimeout

      forever do
        job <- UnliftIO.readChan jobsChan
        Pool.withResource workerPool (flip workerCommunicate job)

newWorkerPool :: MonadUnliftIO m => m (Worker m a) -> Int -> Int -> m (Pool (Worker m a))
newWorkerPool create count workTimeout =
  let ttl :: Double = fromIntegral $ workTimeout * 3
      acquireTimeout = workTimeout
   in Pool.poolCreate create workerKill count acquireTimeout ttl

newWorker :: MonadUnliftIO m => (a -> m ()) -> Int -> m (Worker m a)
newWorker worker timeout = do
  workerChan <- UnliftIO.newChan

  ForkClosable {threadId, status, stop} <- forkClosable $ forever do
    job <- UnliftIO.readChan workerChan
    UnliftIO.timeout timeout $ worker job

  pure
    Worker
      { workerId = threadId,
        workerKill = stop,
        workerStatus = status,
        workerCommunicate = UnliftIO.writeChan workerChan
      }
