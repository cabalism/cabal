{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , showProjectConfigPath
    , nullProjectConfigPath
    , lengthConfigPath
    , nubConfigPath
    , fullConfigPathRoot
    , normaliseConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList)
import Prelude (sequence)

import Data.List.NonEmpty (toList)
import Network.URI (parseURI)
import System.Directory
import System.FilePath
import qualified Data.List.NonEmpty as NE

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
    ProjectConfigPath . NE.fromList
    $ NE.init p ++ [let root = NE.last p in dir </> root]

-- | Make paths relative to the root of the project, not relative to the file
-- they were imported from.
relativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
relativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath
    $ (\segment -> (if isURI segment then segment else makeRelative dir segment))
    <$> p

-- | Make paths relative to the root of the project, not relative to the file
-- they were imported from.
normaliseConfigPath :: ProjectConfigPath -> ProjectConfigPath
normaliseConfigPath (ProjectConfigPath p) =
    ProjectConfigPath . NE.fromList . NE.init $
    NE.scanr
        (\importee importer ->
            if isURI importee
                then importee
                -- The importer is already anchored to the root of the project
                -- by now so we can use its directory to anchor the importee.
                else takeDirectory importer </> importee)
        "."
        p

-- IMPORT-LOC-NORMALIZE-PATH:
-- +-- ./cyclical-same-filename-out-out-back.project
--  +-- ./cyclical-same-filename-out-out-back.config
--   +-- ./same-filename/cyclical-same-filename-out-out-back.config
--    +-- ./same-filename/../cyclical-same-filename-out-out-back.config
--     +-- ./same-filename/../same-filename/cyclical-same-filename-out-out-back.config
--      +-- ./same-filename/../same-filename/../cyclical-same-filename-out-out-back.config

-- +-- ./hops-0.project
--  +-- ./hops/hops-1.config
--   +-- ./hops/../hops-2.config
--    +-- ./hops/../hops/hops-3.config
--     +-- ./hops/../hops/../hops-4.config
--      +-- ./hops/../hops/../hops/hops-5.config
--       +-- ./hops/../hops/../hops/../hops-6.config
--        +-- ./hops/../hops/../hops/../hops/hops-7.config
--         +-- ./hops/../hops/../hops/../hops/../hops-8.config
--          +-- ./hops/../hops/../hops/../hops/../hops/hops-9.config

-- IMPORT-LOC-NORMALIZE-PATH:
-- +-- hops-0.project
--  +-- hops/hops-1.config
--   +-- hops-2.config
--    +-- hops/hops-3.config
--     +-- hops-4.config
--      +-- hops/hops-5.config
--       +-- hops-6.config
--        +-- hops/hops-7.config
--         +-- hops-8.config
--          +-- hops/hops-9.config

canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath dir (ProjectConfigPath p) = do
   xs <- sequence $ NE.scanr (\segment b -> b >>= \b' ->
        -- trace ("CANONICALIZE-SEGMENT=" ++ segment) 
        (if isURI segment then pure segment else
        canonicalizePath $ dir </> takeDirectory b' </> segment)) (pure ".") p
   let ys =
            -- trace ("CANONICALIZE-XS: " ++ show xs) 
            ProjectConfigPath . NE.fromList $ NE.init xs
   let zs =
            -- trace ("CANONICALIZE-YS\n" ++ showProjectConfigPath ys) 
            relativeConfigPath dir ys
   return $
    -- trace ("CANONICALIZE-ZS\n" ++ showProjectConfigPath zs)
    zs

isURI :: FilePath -> Bool
isURI = isJust  .parseURI
