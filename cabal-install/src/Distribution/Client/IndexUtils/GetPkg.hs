{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Distribution.Client.IndexUtils.GetPkg (
  getSourcePackages,
  getSourcePackagesAtIndexState,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.IndexUtils.ActiveRepos
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.Types
import Distribution.Verbosity

import Distribution.Types.Dependency
import Distribution.Types.PackageName (PackageName)
import Distribution.Version
         ( VersionRange, intersectVersionRanges )
import Distribution.Simple.Utils
         ( die', warn, info )
import Distribution.Client.Setup
         ( RepoContext(..) )


import           Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex

import qualified Data.Map as Map
import Distribution.Utils.Generic (fstOf3)


import Distribution.Client.IndexUtils (IndexStateInfo(..), Index (RepoIndex), readIndexTimestamp, readRepoIndex)

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'SourcePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
getSourcePackages :: Verbosity -> RepoContext -> IO SourcePackageDb
getSourcePackages verbosity repoCtxt =
    fstOf3 <$> getSourcePackagesAtIndexState verbosity repoCtxt Nothing Nothing

-- | Variant of 'getSourcePackages' which allows getting the source
-- packages at a particular 'IndexState'.
--
-- Current choices are either the latest (aka HEAD), or the index as
-- it was at a particular time.
--
-- Returns also the total index where repositories'
-- RepoIndexState's are not HEAD. This is used in v2-freeze.
--
getSourcePackagesAtIndexState
    :: Verbosity
    -> RepoContext
    -> Maybe TotalIndexState
    -> Maybe ActiveRepos
    -> IO (SourcePackageDb, TotalIndexState, ActiveRepos)
getSourcePackagesAtIndexState verbosity (repoContextRepos -> []) _ _ = do
      -- In the test suite, we routinely don't have any remote package
      -- servers, so don't bleat about it
      warn (verboseUnmarkOutput verbosity) $
        "No remote package servers have been specified. Usually " ++
        "you would have one specified in the config file."
      return (SourcePackageDb {
        packageIndex       = mempty,
        packagePreferences = mempty
      }, headTotalIndexState, ActiveRepos [])
getSourcePackagesAtIndexState verbosity repoCtxt i@(Just _) mb_activeRepos = do
  getSourcePackagesAtIndexState' verbosity repoCtxt i mb_activeRepos
getSourcePackagesAtIndexState verbosity repoCtxt i@Nothing mb_activeRepos = do
  getSourcePackagesAtIndexState' verbosity repoCtxt i mb_activeRepos

getSourcePackagesAtIndexState'
  :: Verbosity
  -> RepoContext
  -> Maybe TotalIndexState
  -> Maybe ActiveRepos
  -> IO (SourcePackageDb, TotalIndexState, ActiveRepos)
getSourcePackagesAtIndexState' verbosity repoCtxt mb_idxState mb_activeRepos = do
  let activeRepos :: ActiveRepos
      activeRepos = fromMaybe defaultActiveRepos mb_activeRepos

  pkgss <- repoPkgss verbosity repoCtxt mb_idxState
  pkgss' <- case organizeByRepos activeRepos rdRepoName pkgss of
    Right x  -> return x
    Left err -> warn verbosity err >> return (map (, CombineStrategyMerge) pkgss)

  let activeRepos' :: ActiveRepos
      activeRepos' = ActiveRepos
          [ ActiveRepo (rdRepoName rd) strategy
          | (rd, strategy) <- pkgss'
          ]

  let totalIndexState :: TotalIndexState
      totalIndexState = makeTotalIndexState IndexStateHead $ Map.fromList
          [ (n, IndexStateTime ts)
          | (RepoData n ts _idx _prefs, _strategy) <- pkgss'
          -- e.g. file+noindex have nullTimestamp as their timestamp
          , ts /= nullTimestamp
          ]

  let addIndex
          :: PackageIndex UnresolvedSourcePackage
          -> (RepoData, CombineStrategy)
          -> PackageIndex UnresolvedSourcePackage
      addIndex acc (RepoData{}, CombineStrategySkip) = acc
      addIndex acc (RepoData _ _ idx _, CombineStrategyMerge) = PackageIndex.merge acc idx
      addIndex acc (RepoData _ _ idx _, CombineStrategyOverride) = PackageIndex.override acc idx

  let pkgs :: PackageIndex UnresolvedSourcePackage
      pkgs = foldl' addIndex mempty pkgss'

  -- Note: preferences combined without using CombineStrategy
  let prefs :: Map PackageName VersionRange
      prefs = Map.fromListWith intersectVersionRanges
          [ (name, range)
          | (RepoData _n _ts _idx prefs', _strategy) <- pkgss'
          , Dependency name range _ <- prefs'
          ]

  _ <- evaluate pkgs
  _ <- evaluate prefs
  _ <- evaluate totalIndexState
  return (SourcePackageDb {packageIndex = pkgs, packagePreferences = prefs}, totalIndexState, activeRepos')

describeState :: RepoIndexState -> [Char]
describeState IndexStateHead        = "most recent state"
describeState (IndexStateTime time) = "historical state as of " ++ prettyShow time

repoPkgss :: Verbosity -> RepoContext -> Maybe TotalIndexState -> IO [RepoData]
repoPkgss verbosity repoCtxt mb_idxState = for (repoContextRepos repoCtxt) (repoPkgs verbosity repoCtxt mb_idxState)

repoPkgs :: Verbosity -> RepoContext -> Maybe TotalIndexState -> Repo -> IO RepoData
repoPkgs verbosity repoCtxt mb_idxState r = do
  let rname :: RepoName
      rname = repoName r

  info verbosity ("Reading available packages of " ++ unRepoName rname ++ "...")

  idxState <- case mb_idxState of
    Just totalIdxState -> do
      let idxState = lookupIndexState rname totalIdxState
      info verbosity $ "Using " ++ describeState idxState ++
        " as explicitly requested (via command line / project configuration)"
      return idxState
    Nothing -> do
      mb_idxState' <- readIndexTimestamp verbosity (RepoIndex repoCtxt r)
      case mb_idxState' of
        Nothing -> do
          info verbosity "Using most recent state (could not read timestamp file)"
          return IndexStateHead
        Just idxState -> do
          info verbosity $ "Using " ++ describeState idxState ++
            " specified from most recent cabal update"
          return idxState

  unless (idxState == IndexStateHead) $
      case r of
        RepoLocalNoIndex {} -> warn verbosity "index-state ignored for file+noindex repositories"
        RepoRemote {} -> warn verbosity ("index-state ignored for old-format (remote repository '" ++ unRepoName rname ++ "')")
        RepoSecure {} -> pure ()

  let idxState' = case r of
        RepoSecure {} -> idxState
        _             -> IndexStateHead

  (pis,deps,isi) <- readRepoIndex verbosity repoCtxt r idxState'
  explainRepoFromIndexState verbosity idxState' rname isi
  pure $ RepoData {rdRepoName = rname , rdTimeStamp = isiMaxTime isi, rdIndex = pis, rdPreferences = deps }

explainRepoFromIndexState :: Verbosity -> RepoIndexState -> RepoName -> IndexStateInfo -> IO ()
explainRepoFromIndexState verbosity idxState rname isi = do
  case idxState of
    IndexStateHead -> do
        info verbosity ("index-state("++ unRepoName rname ++") = " ++ prettyShow (isiHeadTime isi))
        return ()
    IndexStateTime ts0 -> do
        when (isiMaxTime isi /= ts0) $
            if ts0 > isiMaxTime isi
                then die' verbosity $
                                "Stopping this command as the requested index-state=" ++ prettyShow ts0
                            ++ " is newer than (" ++ prettyShow (isiMaxTime isi)
                            ++ "), the most recent state of '" ++ unRepoName rname
                            ++ "'. You could try 'cabal update' to bring down a later state or request an earlier timestamp for index-state."
                else info verbosity $
                                "Requested index-state " ++ prettyShow ts0
                            ++ " does not exist in '"++ unRepoName rname ++"'!"
                            ++ " Falling back to older state ("
                            ++ prettyShow (isiMaxTime isi) ++ ")."
        info verbosity ("index-state("++ unRepoName rname ++") = " ++
                          prettyShow (isiMaxTime isi) ++ " (HEAD = " ++
                          prettyShow (isiHeadTime isi) ++ ")")

-- auxiliary data used in getSourcePackagesAtIndexState
data RepoData = RepoData
    { rdRepoName    :: RepoName
    , rdTimeStamp   :: Timestamp
    , rdIndex       :: PackageIndex UnresolvedSourcePackage
    , rdPreferences :: [Dependency]
    }