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


syncConfiguredPaths :: MonadIO m => Maybe DirSyncConfig -> m Integer
syncConfiguredPaths Nothing = do liftIO . print $ "Unable to read configuration"; return 0
syncConfiguredPaths (Just (DirSyncConfig dms)) =
  do
    fileCounts <- mapM syncPathConfig dms
    return (sum fileCounts)

syncPathConfig :: MonadIO m => DirMapping -> m Integer
syncPathConfig dm@DirMapping{source=src, destination=dest, ignore=ig} = do
  fs <- liftIO.getFileStatus $ src
  if shouldSkip ig src
    then return 0
    else syncItem' (isDirectory fs) src dest (shouldSkip ig) 0

shouldSkip :: IgnoreList -> FilePath  -> Bool
shouldSkip il f = fn `elem` il
  where fn = takeFileName f

syncItem' :: MonadIO m => Bool -> FilePath -> FilePath -> (FilePath -> Bool) -> Integer -> m Integer
syncItem' isDir src dest skip cnt | not isDir = do 
                                              copyIfNewer src dest 
                                              return (cnt + 1)
                                  | isDir =
  do
    liftIO . print $ (src,  dest)
    liftIO $ createDirectoryIfMissing True dest {- True means create parents -} 
    absPaths <- liftIO . getAbsoluteItems $ src
    syncDirectoryContent absPaths dest skip cnt

getAbsoluteItems :: FilePath -> IO [FilePath]
getAbsoluteItems dir = do
    paths <- listDirectory dir
    return $ map (combine dir) paths

syncDirectoryContent :: MonadIO m => [FilePath] -> FilePath -> (FilePath -> Bool) -> Integer -> m Integer
syncDirectoryContent [] _ _ _ = return 0
syncDirectoryContent (fp:fps) dest skip cnt = do
  fs <- liftIO.getFileStatus $ fp
  newCnt <- if skip fp
               then return 0
               else syncItem' (isDirectory fs) fp destFile skip cnt
  syncDirectoryContent fps dest skip newCnt
  where
    destFile = dest </> takeFileName fp

copyIfNewer :: MonadIO m => FilePath -> FilePath -> m ()
copyIfNewer s t = do
  n <- liftIO $ sourceNewer s t
  when n $ do
      liftIO $ copyFile s t
      liftIO.print $ ("copied", s, "->", t)
  
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
