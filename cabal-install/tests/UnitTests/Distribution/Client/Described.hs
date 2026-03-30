{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.Described where

#if !MIN_VERSION_base(4,20,0)
import Distribution.Client.Compat.Prelude
#endif
import Test.QuickCheck.Instances.Cabal ()
import UnitTests.Distribution.Client.ArbitraryInstances ()
import UnitTests.Distribution.Client.DescribedInstances ()
import Prelude ()

import Distribution.Described (testDescribed)
import Test.Tasty (TestTree, testGroup)

import Distribution.Client.BuildReports.Types (InstallOutcome, Outcome)
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepos)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState, TotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types (RepoName)
import Distribution.Client.Types.AllowNewer (RelaxDepSubject, RelaxDeps, RelaxedDep)

tests :: TestTree
tests = testGroup "Described"
#if MIN_VERSION_base(4,20,0)
    [ testDescribed Timestamp
    , testDescribed RepoIndexState
    , testDescribed TotalIndexState
    , testDescribed RepoName
    , testDescribed ActiveRepos
    , testDescribed RelaxDepSubject
    , testDescribed RelaxedDep
    , testDescribed RelaxDeps
    , testDescribed UserConstraint
    , testDescribed InstallOutcome
    , testDescribed Outcome
    ]
#else
    [ testDescribed (Proxy :: Proxy Timestamp)
    , testDescribed (Proxy :: Proxy RepoIndexState)
    , testDescribed (Proxy :: Proxy TotalIndexState)
    , testDescribed (Proxy :: Proxy RepoName)
    , testDescribed (Proxy :: Proxy ActiveRepos)
    , testDescribed (Proxy :: Proxy RelaxDepSubject)
    , testDescribed (Proxy :: Proxy RelaxedDep)
    , testDescribed (Proxy :: Proxy RelaxDeps)
    , testDescribed (Proxy :: Proxy UserConstraint)
    , testDescribed (Proxy :: Proxy InstallOutcome)
    , testDescribed (Proxy :: Proxy Outcome)
    ]
#endif
