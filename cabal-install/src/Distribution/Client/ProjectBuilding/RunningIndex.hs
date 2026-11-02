-- | The running 'InstalledPackageIndex' of a project build.
--
-- See Note [Per-project InstalledPackageIndex] in "Distribution.Client.ProjectBuilding".
module Distribution.Client.ProjectBuilding.RunningIndex
  ( RunningIndex
  , newRunningIndex
  , readRunningIndex
  , insertRunningIndex
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)

-- | An 'InstalledPackageIndex' shared by all the units of a project build.
--
-- It holds the units registered by this run of @cabal-install@, and the
-- contents of the package databases the plan touches. The latter are read in
-- bulk, with one @ghc-pkg dump@ per database, the first time the index is read
-- (see 'readRunningIndex'), so a run that never reads it never pays for them.
data RunningIndex = RunningIndex
  { runningIndexInitial :: MVar (Maybe (IO InstalledPackageIndex))
  -- ^ The deferred bulk read of the package databases, until it has run.
  , runningIndexVar :: TVar InstalledPackageIndex
  }

-- | A new index, given the bulk read to populate it with on first use.
newRunningIndex :: IO InstalledPackageIndex -> IO RunningIndex
newRunningIndex readInitial =
  RunningIndex <$> newMVar (Just readInitial) <*> newTVarIO mempty

-- | Read the index, first populating it from the package databases if that
-- has not been done yet.
--
-- Concurrent readers wait for the one performing the bulk read. If the read
-- fails, it is attempted again by the next reader.
readRunningIndex :: RunningIndex -> IO InstalledPackageIndex
readRunningIndex RunningIndex{runningIndexInitial, runningIndexVar} = do
  modifyMVar_ runningIndexInitial $ \minitial -> case minitial of
    Nothing -> return Nothing
    Just readInitial -> do
      initial <- readInitial
      -- Units registered by this run take precedence over the bulk read.
      atomically $ modifyTVar' runningIndexVar (PackageIndex.merge initial)
      return Nothing
  readTVarIO runningIndexVar

-- | Record a unit that this run of @cabal-install@ has just registered.
insertRunningIndex :: InstalledPackageInfo -> RunningIndex -> IO ()
insertRunningIndex ipkg RunningIndex{runningIndexVar} =
  atomically $ modifyTVar' runningIndexVar (PackageIndex.insert ipkg)
