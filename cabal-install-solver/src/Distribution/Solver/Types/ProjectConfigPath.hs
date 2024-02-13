{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , showProjectConfigPath
    , showProjectConfigPathFailReason
    , nullProjectConfigPath
    , hasDuplicatesConfigPath
    , fullConfigPathRoot
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList, foldr1)
import Prelude (foldr1, sequence)

import Data.List.NonEmpty (toList)
import Network.URI (parseURI)
import System.Directory
import System.FilePath
import qualified Data.List.NonEmpty as NE
import Distribution.Solver.Modular.Version (VR)
import Distribution.Pretty (prettyShow)

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

showProjectConfigPathFailReason :: VR -> ProjectConfigPath -> String
showProjectConfigPathFailReason vr projectConfig =
    -- SEE: https://stackoverflow.com/questions/4342013/the-composition-of-functions-in-a-list-of-functions
    ( foldr1 (.)
        [(showString "\n      " . showString l)
        | l <- lines $ showProjectConfigPath projectConfig
        ]
    . showString " requires "
    . showString (prettyShow vr)
    ) ""

projectConfigPathRoot :: ProjectConfigPath -> FilePath
projectConfigPathRoot (ProjectConfigPath xs) = last xs

nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectConfigPath $ "unused" :| []

-- | Check if the path has duplicates. A cycle of imports is not allowed. This
-- check should only be done after the path has been canonicalized with
-- @canonicalizeConfigPath@. This is because the import path may contain paths
-- that are the same in relation to their importers but different in relation to
-- the project root.
hasDuplicatesConfigPath :: ProjectConfigPath -> Bool
hasDuplicatesConfigPath (ProjectConfigPath p) = length p /= length (NE.nub p)

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

-- | Normalizes and canonicalizes paths so that '..' segments can be removed.
--
-- It converts paths like this:
-- @
-- +-- hops-0.project
--  +-- hops/hops-1.config
--   +-- ../hops-2.config
--    +-- hops/hops-3.config
--     +-- ../hops-4.config
--      +-- hops/hops-5.config
--       +-- ../hops-6.config
--        +-- hops/hops-7.config
--         +-- ../hops-8.config
--          +-- hops/hops-9.config
-- @
-- 
-- Into paths like this:
-- @
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
-- @
--
-- That way we have @hops-8.config" instead of
-- @./hops/../hops/../hops/../hops/../hops-8.config@.
--
-- >>> :{
-- (\s -> (".." `isInfixOf` s, "hops-8.config" `isSuffixOf` s))
-- <$> canonicalizePath
-- "../cabal-testsuite/PackageTests/ConditionalAndImport/hops/../hops/../hops/../hops/../hops-8.config"
-- :}
-- (False,True)
canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath dir (ProjectConfigPath p) = do
   xs <- sequence $ NE.scanr (\importee -> (>>= \importer ->
        if isURI importee
            then pure importee
            else canonicalizePath $ dir </> takeDirectory importer </> importee))
        (pure ".") p
   return . relativeConfigPath dir . ProjectConfigPath . NE.fromList $ NE.init xs

isURI :: FilePath -> Bool
isURI = isJust  .parseURI

-- $setup
-- >>> import Data.List