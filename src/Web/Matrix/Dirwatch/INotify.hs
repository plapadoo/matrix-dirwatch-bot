module Web.Matrix.Dirwatch.INotify
  ( watchDirectoryRecursive
  , recursiveSubdirs
  , eventFilePath
  , stopWatch
  , Monitor
  , watchRecursiveBuffering
  , transformPath
  , makeRelative
  , unbuffer
  , NotifyEvent(..)
  , Event(..)
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.Chan      (Chan, newChan, readChan,
                                               writeChan)
import           Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan,
                                               writeTChan)
import           Control.Monad                (filterM, forM, forM_, forever,
                                               liftM, return, void)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Loops          (unfoldM)
import           Control.Monad.STM            (atomically)
import           Data.Bool                    (Bool (..))
import           Data.Function                (($), (.))
import           Data.Functor                 ((<$>))
import           Data.List                    (concat, drop, length)
import           Data.Maybe                   (Maybe (..))
import           Prelude                      ((*))
import           System.Directory             (doesDirectoryExist,
                                               listDirectory)
import           System.FilePath              (FilePath, joinPath, splitPath,
                                               (</>))
import           System.INotify               (Event (..),
                                               EventVariety (AllEvents),
                                               INotify, addWatch, initINotify,
                                               killINotify)
import           System.IO                    (IO)

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative relativeTo = joinPath . drop (length (splitPath relativeTo)) . splitPath

data Monitor = Monitor
  { monitorStop    :: Chan ()
  , monitorStopped :: Chan ()
  }

listDirectoryFull :: MonadIO m => FilePath -> m [FilePath]
listDirectoryFull dir = do
  list <- liftIO (listDirectory dir)
  return $ (dir </>) <$> list

recursiveSubdirs
  :: MonadIO m
  => FilePath -> m [FilePath]
recursiveSubdirs dir = do
  files <- listDirectoryFull dir
  dirs <- filterM (liftIO . doesDirectoryExist) files
  liftM concat $
    forM dirs $ \innerDir -> do
      subdirs <- recursiveSubdirs innerDir
      return (innerDir : subdirs)

data NotifyEvent =
  NotifyEvent FilePath
              Event

transformPath :: (FilePath -> FilePath) -> NotifyEvent -> NotifyEvent
transformPath f (NotifyEvent path event) = NotifyEvent (f path) event

watchDirectoryRecursive'
  :: MonadIO m
  => INotify -> FilePath -> (NotifyEvent -> IO ()) -> m ()
watchDirectoryRecursive' inotify fp cb = do
  subDirs <- recursiveSubdirs fp
  forM_ (fp : subDirs) $ \dir -> do
    -- liftIO $ putStrLn $ "Watching directory: " <> dir
    liftIO $
      void $
      addWatch inotify [AllEvents] dir (\event -> cb (NotifyEvent dir event))

watchDirectoryRecursive
  :: MonadIO m
  => FilePath -> (NotifyEvent -> IO ()) -> m Monitor
watchDirectoryRecursive fp cb = do
  inotify <- liftIO $ initINotify
  stopChan <- liftIO newChan
  stoppedChan <- liftIO newChan
  let innerCb (NotifyEvent dir event) =
        case event of
          Created True path -> do
            -- liftIO $ putStrLn $ "directory was created:" <> (dir </> path)
            watchDirectoryRecursive' inotify (dir </> path) innerCb
            cb $ NotifyEvent (dir </> path) event
          _ -> cb $ NotifyEvent dir event
  liftIO $
    void $
    forkIO $ do
      watchDirectoryRecursive' inotify fp innerCb
      -- liftIO $ putStrLn "waiting"
      liftIO $ void $ readChan stopChan
      liftIO $ killINotify inotify
      liftIO $ writeChan stoppedChan ()
  return (Monitor stopChan stoppedChan)

stopWatch
  :: MonadIO m
  => Monitor -> m ()
stopWatch m = do
  liftIO $ writeChan (monitorStop m) ()
  liftIO $ void $ readChan (monitorStopped m)

data RecursiveWatcher =
  RecursiveWatcher (TChan NotifyEvent)

watchRecursiveBuffering
  :: MonadIO m
  => FilePath -> m RecursiveWatcher
watchRecursiveBuffering fb = do
  chan <- liftIO newTChanIO
  void $ watchDirectoryRecursive fb (\e -> atomically (writeTChan chan e))
  return (RecursiveWatcher chan)

unbuffer
  :: MonadIO m
  => RecursiveWatcher -> ([NotifyEvent] -> IO ()) -> m ()
unbuffer (RecursiveWatcher tchan) cb =
  forever $ do
    liftIO $ threadDelay (5000 * 1000)
    packet <- liftIO $ atomically (unfoldM (tryReadTChan tchan))
    liftIO $ cb packet

eventFilePath :: Event -> Maybe FilePath
eventFilePath (Accessed _ fp)   = fp
eventFilePath (Modified _ fp)   = fp
eventFilePath (Attributes _ fp) = fp
eventFilePath (Closed _ fp _)   = fp
eventFilePath (Opened _ fp)     = fp
eventFilePath (MovedOut _ fp _) = Just fp
eventFilePath (MovedIn _ fp _)  = Just fp
eventFilePath (MovedSelf _)     = Nothing
eventFilePath (Created _ fp)    = Just fp
eventFilePath (Deleted _ fp)    = Just fp
eventFilePath _                 = Nothing
