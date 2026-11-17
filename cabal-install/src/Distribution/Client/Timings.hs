-----------------------------------------------------------------------------

-- | The lines the @timings@ setting prints: one per unit of work, naming the
-- work, its subject and how long it took.
--
-- > <seconds>s [timing] <scope>:<step> <subject> [<note>]
--
-- The seconds come first, right-aligned, so the column lines up and the
-- biggest times stand out, and @sort -n@ ranks the lines. The scope is @plan@
-- for a planning step, @check@ for a step that decides what work there is,
-- and @pkg@ for a phase of one package, and the step is the name within it,
-- so a grep narrows from the left: @[timing]@ is every timed unit of work,
-- @[timing] pkg:@ every package phase, @[timing] pkg:build@ every package
-- build, and the subject column picks out one package. The subject is the
-- package for a package phase and @-@ otherwise. The note at the end is
-- optional: what a step covered, or @(failed)@ or @(skipped)@ for a package
-- phase.
--
-- The package phases print one for each download, unpack, configure, build,
-- haddock and install of a package. Planning prints one for each step it
-- performs, which is none on a run that reuses its cached plan. The check
-- lines are the status check of every run and the installed package index
-- read of a run that builds.
--
-- The lines print at any verbosity level, so that @-v0 --timings@ prints the
-- timings and nothing else.
module Distribution.Client.Timings
  ( printTiming
  , timed
  , timedPhase
  , countNote
  , packagesNote
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Utils (noticeNoWrap)
import Distribution.Verbosity (VerbosityFlags (vLevel), VerbosityLevel (Normal), modifyVerbosityFlags)

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)

-- | Print the line for one timed unit of work. See the module header for the
-- format. The line prints whatever the verbosity level, with the level's
-- flags kept, so output markers and timestamps still apply to it. It is not
-- wrapped, which would trim the padding that aligns the seconds and could
-- break a line with a long package id.
printTiming
  :: Verbosity
  -> String
  -- ^ The scope: @plan@, @check@ or @pkg@.
  -> String
  -- ^ The step within the scope.
  -> String
  -- ^ The subject, a package id or @-@.
  -> Double
  -- ^ Elapsed seconds.
  -> String
  -- ^ A note, or empty.
  -> IO ()
printTiming verbosity scope step subject elapsed note =
  noticeNoWrap (modifyVerbosityFlags atLeastNormal verbosity) $
    timingMessage scope step subject elapsed note
  where
    atLeastNormal flags = flags{vLevel = max Normal (vLevel flags)}

-- | The line for one timed unit of work.
timingMessage :: String -> String -> String -> Double -> String -> String
timingMessage scope step subject elapsed note =
  unwords $
    [ -- Milliseconds, right-aligned so that the column lines up for times
      -- below 10,000 s.
      printf "%8.3fs" elapsed
    , "[timing]"
    , scope ++ ":" ++ step
    , subject
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
      liftIO $ printTiming verbosity scope step subject (t1 - t0) n
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
