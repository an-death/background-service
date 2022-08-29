module Service.BackgroundService.BackgroundWorkers
  ( BackgroundWorkersConfig (..),
    backgroundWorkers,
    BackgroundWorkers (..),
  )
where

import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadIO (liftIO), MonadUnliftIO)
import qualified Data.Text as T
import qualified Service.BackgroundService.Internal.Workers as W
import qualified UnliftIO.Concurrent as UnliftIO

data BackgroundWorkersConfig = BackgroundWorkersConfig
  { backgroundServiceName :: !T.Text,
    backgroundWorkerCount :: !Int,
    backgroundWorkerTimeout :: !Int
  }

data BackgroundWorkers m = BackgroundWorkers
  { communicate :: IO () -> m (),
    stop :: m (),
    status :: m Bool
  }

backgroundWorkers :: MonadUnliftIO m => BackgroundWorkersConfig -> m (BackgroundWorkers m)
backgroundWorkers BackgroundWorkersConfig {..} = do
  jobsChan <- UnliftIO.newChan
  workerSupervisor <- W.workerSupervisor jobsChan backgroundWorkerCount backgroundWorkerTimeout worker
  statusVar <- UnliftIO.newMVar False

  let communicate job = UnliftIO.writeChan jobsChan job
  let stop = W.workerKill workerSupervisor >> UnliftIO.putMVar statusVar True

  pure BackgroundWorkers {communicate, stop, status = UnliftIO.takeMVar statusVar}
  where
    worker ioJob =
      void $ liftIO $ ioJob
