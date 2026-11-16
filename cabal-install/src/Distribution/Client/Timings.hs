-----------------------------------------------------------------------------

-- | The lines the @timings@ setting prints: one per unit of work, naming the
-- work, its subject and how long it took.
--
-- > [timing] <scope>:<step> <subject> <seconds>s [<note>]
--
-- The scope is @plan@ for a planning step and @pkg@ for a phase of one
-- package, and the step is the name within it, so the lines narrow from the
-- left: @[timing]@ is every timed unit of work, @[timing] pkg:@ every package
-- phase, @[timing] pkg:build@ every package build, and the subject column picks
-- out one package. The subject is the package for a package phase and @-@ for
-- a planning step, so the time is always the fourth column. The note at the
-- end is optional: what a planning step covered, or @(failed)@ for a package
-- phase that threw.
--
-- The package phases print one for each configure, build, haddock and install
-- of a package. Planning prints one for each step it performs, which is none
-- on a run that reuses its cached plan.
module Distribution.Client.Timings
  ( timingMessage
  , timedPhase
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
  -- ^ The scope, @plan@ or @pkg@.
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

-- | Run an action and, when timings are on, print how long it took as a
-- planning step. The note may depend on the result, so the step can report
-- the size of what it produced. The note is forced before the clock stops,
-- so a count over a lazily built result is part of the time.
timedPhase
  :: MonadIO m
  => Verbosity
  -> Bool
  -- ^ Whether timings are on.
  -> String
  -- ^ The step.
  -> (a -> String)
  -- ^ The note, from the result.
  -> m a
  -> m a
timedPhase verbosity enabled step note action
  | not enabled = action
  | otherwise = do
      t0 <- liftIO getMonotonicTime
      r <- action
      let n = note r
      t1 <- length n `seq` liftIO getMonotonicTime
      liftIO . notice verbosity $ timingMessage "plan" step "-" (t1 - t0) n
      return r
