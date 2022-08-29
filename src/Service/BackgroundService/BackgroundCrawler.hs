module Service.BackgroundService.BackgroundCrawler
  ( backgroundCrawler,
    BackgroundCrawlerConfig (..),
    BackgroundCrawler (..),
  )
where

import Control.Monad (forever)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Service.BackgroundService.BackgroundCrawler.Crawler (Crawler, initBackgroundCrawlers, iterateBackgroundCrawlers)
import Service.BackgroundService.Internal.ForkClosable (ForkClosable (..), forkClosable)
import qualified Service.BackgroundService.Internal.Workers as W
import qualified UnliftIO
import qualified UnliftIO.Concurrent as UnliftIO

data BackgroundCrawlerConfig = BackgroundCrawlerConfig
  { crawlerPeriodicity :: Int,
    crawlerTimeout :: Int,
    crawlerWorkerCount :: Int,
    crawlerWorkerTimeout :: Int
  }

data BackgroundCrawler m = BackgroundCrawler
  { crawlerServiceId :: !UnliftIO.ThreadId,
    crawlerServiceStop :: m (),
    crawlerServiceStatus :: m Bool
  }

backgroundCrawler :: MonadUnliftIO m => BackgroundCrawlerConfig -> [Crawler m a] -> (a -> m ()) -> m (BackgroundCrawler m)
backgroundCrawler BackgroundCrawlerConfig {..} crawlers worker = do
  ForkClosable {threadId, status, stop} <- forkClosable do
    UnliftIO.bracket initBackgroundService closeBackgroundService \(_, crawler) -> do
      forever do
        UnliftIO.threadDelay crawlerPeriodicity
        iterateBackgroundCrawlers crawler

  pure BackgroundCrawler {crawlerServiceId = threadId, crawlerServiceStop = stop, crawlerServiceStatus = status}
  where
    closeBackgroundService (workers, _) = do
      W.workerKill workers

    initBackgroundService = do
      jobsChan <- UnliftIO.newChan
      workerSupervisor <- W.workerSupervisor jobsChan crawlerWorkerCount crawlerWorkerTimeout worker
      crawler <- initBackgroundCrawlers jobsChan crawlers
      pure (workerSupervisor, crawler)
