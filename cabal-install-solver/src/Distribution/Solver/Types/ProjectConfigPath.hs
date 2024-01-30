{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ParallelListComp #-}
module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , showProjectConfigPath
    , nullProjectConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList)
import Prelude ()
import Data.List.NonEmpty (toList)

-- | Path to a configuration file, being either "the project" root or an import,
-- built up from the root to the leaf. The root is the last element and the leaf
-- is the first element.
newtype ProjectConfigPath = ProjectConfigPath (NonEmpty FilePath)
    deriving (Eq, Show, Generic)

-- | Renders the path with ancestors above and unindented, like this:
-- @
-- +-- <ROOT>
--  +-- ...
--   +-- ...
--    +-- <LEAF>
-- @
showProjectConfigPath :: ProjectConfigPath -> String
showProjectConfigPath (ProjectConfigPath xs) =
    unlines
        [ (nTimes i (showChar ' ') . showString "+-- " . showString x) ""
        | x <- reverse $ toList xs
        | i <- [0..]
        ]

-- | Apply a function @n@ times to a given value.
-- SEE: GHC.Utils.Misc
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f

projectConfigPathRoot :: ProjectConfigPath -> FilePath
projectConfigPathRoot (ProjectConfigPath xs) = last xs

nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectConfigPath $ "unused" :| []

instance Binary ProjectConfigPath
instance Structured ProjectConfigPath
