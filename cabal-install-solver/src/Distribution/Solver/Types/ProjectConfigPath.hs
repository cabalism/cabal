{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ParallelListComp #-}
module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , showProjectConfigPath
    , nullProjectConfigPath
    , lengthConfigPath
    , nubConfigPath
    , fullConfigPathRoot
    , relativeConfigPath
    , normaliseConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList)
import Prelude (sequence)
import Data.List.NonEmpty (toList)
import qualified Data.List.NonEmpty as NE
import System.Directory
import System.FilePath

-- | Path to a configuration file, being either "the project" root or an import,
-- built up from the root to the leaf. The root is the last element and the leaf
-- is the first element.
newtype ProjectConfigPath = ProjectConfigPath (NonEmpty FilePath)
    deriving (Eq, Show, Generic)

instance Binary ProjectConfigPath
instance Structured ProjectConfigPath

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

lengthConfigPath :: ProjectConfigPath -> Int
lengthConfigPath (ProjectConfigPath p) = NE.length p

nubConfigPath :: ProjectConfigPath -> ProjectConfigPath
nubConfigPath (ProjectConfigPath p) = ProjectConfigPath $ NE.nub p

-- | If the project was a full path, we need to show the full path in messages
-- and do this by reconstructing the full path of the root (the project) from
-- its directory and file name.
fullConfigPathRoot :: FilePath -> ProjectConfigPath -> ProjectConfigPath
fullConfigPathRoot dir (ProjectConfigPath p) =
    ProjectConfigPath . NE.fromList $ NE.init p ++ [dir </> NE.last p]

-- Make paths relative to the root of the project, not relative to the file
-- they were imported from.
relativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
relativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath $ makeRelative dir <$> p

-- Make paths relative to the root of the project, not relative to the file
-- they were imported from.
normaliseConfigPath :: ProjectConfigPath -> ProjectConfigPath
normaliseConfigPath (ProjectConfigPath p) =
    ProjectConfigPath . NE.fromList . NE.init $
    NE.scanr (\a b -> takeDirectory b </> a) "." p

canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath dir (ProjectConfigPath p) = do
   xs <- sequence $ NE.scanr (\a b -> b >>= \b' ->
        canonicalizePath $ dir </> takeDirectory b' </> a) (pure ".") p
   return . relativeConfigPath dir . ProjectConfigPath . NE.fromList $ NE.init xs
