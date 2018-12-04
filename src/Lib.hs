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


syncConfiguredPaths :: Maybe DirSyncConfig ->  IO Integer
syncConfiguredPaths Nothing = do liftIO . print $ "Unable to read configuration"; return 0
syncConfiguredPaths (Just (DirSyncConfig dms)) =
  do
    fileCounts <- mapM syncPathConfig dms
    return (sum fileCounts)

syncPathConfig :: DirMapping -> IO Integer
syncPathConfig dm@DirMapping{source=src, destination=dest, ignore=ig} = do
  fs <- getFileStatus src
  if shouldSkip ig src
    then return 0
    else syncItem' (isDirectory fs) src dest (shouldSkip ig) 0

shouldSkip :: IgnoreList -> FilePath  -> Bool
shouldSkip il f = fn `elem` il
  where fn = takeFileName f

syncItem' :: Bool -> FilePath -> FilePath -> (FilePath -> Bool) -> Integer -> IO Integer
syncItem' isDir src dest skip cnt | not isDir = copyIfNewer src dest cnt 
                                  | isDir =
  do
    --print (src,  dest)
    createDirectoryIfMissing True dest {- True means create parents -} 
    absPaths <- getAbsoluteItems src
    syncDirectoryContent absPaths dest skip cnt

getAbsoluteItems :: FilePath -> IO [FilePath]
getAbsoluteItems dir = do
    paths <- listDirectory dir
    return $ map (combine dir) paths

syncDirectoryContent :: [FilePath] -> FilePath -> (FilePath -> Bool) -> Integer -> IO Integer
syncDirectoryContent [] _ _ c = return c
syncDirectoryContent (fp:fps) dest skip cnt = do
  fs <- getFileStatus fp
  newCnt <- if skip fp
               then return cnt
               else syncItem' (isDirectory fs) fp destFile skip cnt
  syncDirectoryContent fps dest skip newCnt
  where
    destFile = dest </> takeFileName fp

copyIfNewer :: FilePath -> FilePath -> Integer -> IO Integer
copyIfNewer s d cnt = do
  newer <- sourceNewer s d
  if newer then do
      copyFile s d
      print ("copied", s, "->", d)
      return (cnt + 1)
  else return cnt
  
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
