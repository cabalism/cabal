-----------------------------------------------------------------------------

-- | The lines the @build-timings@ setting prints: one per unit of work,
-- naming the work, its subject and how long it took.
--
-- The package build phases print one for each configure, build, haddock and
-- install of a package. Planning prints one for each step it performs, which
-- is none on a run that reuses its cached plan.
module Distribution.Client.BuildTimings
  ( buildTimingMessage
  , timedPhase
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Utils (notice)

import Control.Monad.IO.Class (MonadIO, liftIO)

import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

-- | The line for one timed unit of work.
--
-- > [build-timings] <phase> <subject> <seconds>s [<note>]
--
-- The subject is the package for a package phase. A planning step has no
-- package, so its subject is @-@ and the note says what it covered, such as
-- the number of packages. A failed package phase has the note @(failed)@.
buildTimingMessage :: String -> String -> Double -> String -> String
buildTimingMessage phase subject elapsed note =
  unwords $
    [ "[build-timings]"
    , phase
    , subject
    , -- Print milliseconds (3 decimal places)
      printf "%.3fs" elapsed
    ]
      ++ [note | not (null note)]

-- | Run an action and, when build-timings is on, print how long it took as a
-- planning step. The note may depend on the result, so the step can report
-- the size of what it produced. The note is forced before the clock stops,
-- so a count over a lazily built result is part of the time.
timedPhase
  :: MonadIO m
  => Verbosity
  -> Bool
  -- ^ Whether build-timings is on.
  -> String
  -- ^ The step.
  -> (a -> String)
  -- ^ The note, from the result.
  -> m a
  -> m a
timedPhase verbosity enabled phase note action
  | not enabled = action
  | otherwise = do
      t0 <- liftIO getMonotonicTime
      r <- action
      let n = note r
      t1 <- length n `seq` liftIO getMonotonicTime
      liftIO . notice verbosity $ buildTimingMessage phase "-" (t1 - t0) n
      return r
