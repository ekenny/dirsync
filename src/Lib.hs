module Lib
    ( syncConfiguredPaths
    ) where


import Config
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import System.Posix.Files
import System.Posix.Types
import System.FilePath.Posix


syncConfiguredPaths :: MonadIO m => Maybe DirSyncConfig -> m ()
syncConfiguredPaths Nothing = liftIO . print $ "Unable to read configuration"
syncConfiguredPaths (Just (DirSyncConfig dms)) =
  do
    mapM syncPathConfig dms
    return ()

syncPathConfig :: MonadIO m => DirMapping -> m()
syncPathConfig dm@(DirMapping{source=src, destination=dest, ignore=ig}) = do
  fs <- liftIO.getFileStatus $ src
  unless (shouldSkip ig src) (syncItem' (isDirectory fs) src dest (shouldSkip ig))

shouldSkip :: IgnoreList -> FilePath  -> Bool
shouldSkip il f = fn `elem` il
  where fn = takeFileName f

syncItem' :: MonadIO m => Bool -> FilePath -> FilePath -> (FilePath -> Bool)-> m ()
syncItem' isDir src dest skip | isDir == False = copyIfNewer src dest
                              | isDir == True =
  do
    liftIO . print $ (src,  dest)
    liftIO $ createDirectoryIfMissing True {- create parents -} dest
    absPaths <- liftIO . getAbsoluteItems $ src
    syncDirectoryContent absPaths dest skip

getAbsoluteItems :: FilePath -> IO [FilePath]
getAbsoluteItems dir = do
    paths <- listDirectory dir
    return $ map (combine dir) $ paths

syncDirectoryContent :: MonadIO m => [FilePath] -> FilePath -> (FilePath -> Bool) -> m ()
syncDirectoryContent [] _ _ = return ()
syncDirectoryContent (fp:fps) dest skip = do
  fs <- liftIO.getFileStatus $ fp
  unless (skip fp) (syncItem' (isDirectory fs) fp destFile skip)
  syncDirectoryContent fps dest skip
  where
    destFile = dest </> (takeFileName fp)

copyIfNewer :: MonadIO m => FilePath -> FilePath -> m ()
copyIfNewer s t = do
  n <- liftIO $ sourceNewer s t
  if n then do
      liftIO $ copyFile s t
      liftIO.print $ ("copied", s, t)
    else return ()

sourceNewer :: FilePath -> FilePath -> IO Bool
sourceNewer src target = do
   targetExists <- fileExist target
   if targetExists
     then extantTargetIsOlder src target
     else return True

extantTargetIsOlder :: FilePath -> FilePath -> IO Bool
extantTargetIsOlder src target =
  do
    mt1 <- getModTime src
    mt2 <- getModTime target
    return (mt1 >= mt2)

getModTime :: FilePath -> IO EpochTime
getModTime fp = do
  fs <- getFileStatus fp
  return (modificationTime fs)
