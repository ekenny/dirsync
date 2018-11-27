module Lib
    ( syncConfiguredPaths
    ) where


import Config
import Control.Monad.IO.Class
import System.Directory
import System.Posix.Files
import System.FilePath.Posix

syncConfiguredPaths :: MonadIO m => Maybe DirSyncConfig -> m ()
syncConfiguredPaths Nothing = liftIO . print $ "Unable to read configuration"
syncConfiguredPaths (Just (DirSyncConfig dms)) =
  do
    mapM syncPathConfig dms
    return ()

syncPathConfig :: MonadIO m => DirMapping -> m()
syncPathConfig dm@(DirMapping{source=src, destination=dest}) = do
  liftIO . print $ ["working on", dm] 
  fs <- liftIO.getFileStatus $ src
  syncItem' (isDirectory fs) src dest

syncItem' :: MonadIO m => Bool -> FilePath -> FilePath -> m ()
syncItem' isDir src dest | isDir == False = liftIO . print $ [src,  dest]
                         | isDir == True =
  do
    paths <- liftIO . listDirectory $ src
    syncDirectoryContent absPaths (dest </> (takeFileName src))
    where absPaths = mapM (combine (takeDirectory src)) paths


syncDirectoryContent :: MonadIO m => [FilePath] -> FilePath -> m ()
syncDirectoryContent [] _ = return ()
syncDirectoryContent (fp:fps) dest = do
  fs <- liftIO.getFileStatus $ fp
  syncItem' (isDirectory fs) fp destFile
  syncDirectoryContent fps dest
  where
    destFile = dest </> (takeFileName fp)


{-
targetIsUpToDate :: MonadIO m => FilePath -> FilePath -> m Bool
targetIsUpToDate src target = do
   exists <- fileExist target
   return (exists && updts <= updtt)
      where updts = modificationTime $ getFileStatus src
            updtt modificationTime $ getFileStatus target
-}
