module Service.BackgroundService.BackgroundCrawler.Crawler
  ( initBackgroundCrawlers,
    Crawler (..),
    BackgroundCrawlers (..),
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text as T
import qualified UnliftIO

data Crawler m a = Crawler
  { crawlerName :: !T.Text,
    runCrawler :: !(m [a]),
    crawlerTimeout :: !Int
  }

newtype BackgroundCrawlers m = BackgroundCrawlers {iterateBackgroundCrawlers :: m ()}

initBackgroundCrawlers :: MonadUnliftIO m => UnliftIO.Chan a -> [Crawler m a] -> m (BackgroundCrawlers m)
initBackgroundCrawlers jobsChan crawlers =
  pure $
    BackgroundCrawlers
      { iterateBackgroundCrawlers = forM_ crawlers (runCrawler' jobsChan)
      }

runCrawler' :: MonadUnliftIO m => UnliftIO.Chan a -> Crawler m a -> m ()
runCrawler' jobsChan Crawler {..} = do
  UnliftIO.tryAny task >>= \case
    Left _e -> pure () -- FIXME (@asimuskov): log error
    Right _ -> pure ()
  where
    task = do
      jobs <- UnliftIO.timeout crawlerTimeout runCrawler
      case jobs of
        Nothing -> pure ()
        Just jobsToDo -> forM_ jobsToDo (UnliftIO.writeChan jobsChan)
