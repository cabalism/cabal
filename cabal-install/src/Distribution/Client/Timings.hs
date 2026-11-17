-----------------------------------------------------------------------------

-- | The lines the @timings@ setting prints: one per unit of work, naming the
-- work, its subject and how long it took.
--
-- > [timing] <scope>:<step> <subject> <seconds>s [<note>]
--
-- The scope is @plan@ for a planning step, @check@ for a step that decides
-- what work there is, and @pkg@ for a phase of one package, and the step is
-- the name within it, so the lines narrow from the left: @[timing]@ is every
-- timed unit of work, @[timing] pkg:@ every package phase, @[timing] pkg:build@
-- every package build, and the subject column picks out one package. The
-- subject is the package for a package phase and @-@ otherwise, so the time is
-- always the fourth column. The note at the end is optional: what a step
-- covered, or @(failed)@ for a package phase that threw.
--
-- The package phases print one for each configure, build, haddock and install
-- of a package. Planning prints one for each step it performs, which is none
-- on a run that reuses its cached plan. The check lines are the status check
-- of every run and the installed package index read of a run that builds.
module Distribution.Client.Timings
  ( timingMessage
  , timed
  , timedPhase
  , countNote
  , packagesNote
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Utils (notice)

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

-- | The line for one timed unit of work. See the module header for the
-- format.
timingMessage
  :: String
  -- ^ The scope: @plan@, @check@ or @pkg@.
  -> String
  -- ^ The step within the scope.
  -> String
  -- ^ The subject, a package id or @-@.
  -> Double
  -- ^ Elapsed seconds.
  -> String
  -- ^ A note, or empty.
  -> String
timingMessage scope step subject elapsed note =
  unwords $
    [ "[timing]"
    , scope ++ ":" ++ step
    , subject
    , -- Print milliseconds (3 decimal places)
      printf "%.3fs" elapsed
    ]
      ++ [note | not (null note)]

-- | Run an action and, when timings are on, print how long it took. The note
-- may depend on the result, so a step can report the size of what it
-- produced. The note is forced before the clock stops, so a count over a
-- lazily built result is part of the time.
timed
  :: MonadIO m
  => Verbosity
  -> Bool
  -- ^ Whether timings are on.
  -> String
  -- ^ The scope.
  -> String
  -- ^ The step.
  -> String
  -- ^ The subject, a package id or @-@.
  -> (a -> String)
  -- ^ The note, from the result.
  -> m a
  -> m a
timed verbosity enabled scope step subject note action
  | not enabled = action
  | otherwise = do
      t0 <- liftIO getMonotonicTime
      r <- action
      let n = note r
      t1 <- length n `seq` liftIO getMonotonicTime
      liftIO . notice verbosity $ timingMessage scope step subject (t1 - t0) n
      return r

-- | 'timed' for a step with no package of its own, in the @plan@ or @check@
-- scope.
timedPhase
  :: MonadIO m
  => Verbosity
  -> Bool
  -- ^ Whether timings are on.
  -> String
  -- ^ The scope, @plan@ or @check@.
  -> String
  -- ^ The step.
  -> (a -> String)
  -- ^ The note, from the result.
  -> m a
  -> m a
timedPhase verbosity enabled scope step = timed verbosity enabled scope step "-"

-- | The note for a step that read or produced a number of things, named in
-- the plural: @(47 packages)@.
countNote :: String -> [a] -> String
countNote things xs = "(" ++ show (length xs) ++ " " ++ things ++ ")"

-- | 'countNote' for packages, the usual case.
packagesNote :: [a] -> String
packagesNote = countNote "packages"
