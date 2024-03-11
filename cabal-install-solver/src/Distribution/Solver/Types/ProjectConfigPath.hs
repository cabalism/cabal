{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , docProjectConfigPath
    , cyclicalImportMsg
    , docProjectConfigPathFailReason
    , nullProjectConfigPath
    , consProjectConfigPath
    , hasDuplicatesConfigPath
    , canonicalizeConfigPath
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList, (<>))
import Prelude (sequence)

import Data.Coerce (coerce)
import Data.List.NonEmpty ((<|))
import Network.URI (parseURI)
import System.Directory
import System.FilePath
import qualified Data.List.NonEmpty as NE
import Distribution.Solver.Modular.Version (VR)
import Distribution.Pretty (prettyShow)
import Text.PrettyPrint

-- | Path to a configuration file, either a singleton project root, or a longer
-- list representing a path to an import.  The path is a non-empty list that we
-- build up by prepending relative imports with @consProjectConfigPath@.
--
-- An import can be a URI, such as [a stackage
-- cabal.config](https://www.stackage.org/nightly/cabal.config), but we do not
-- support URIs in the middle of the path, URIs that import other URIs, or URIs
-- that import local files.
--
-- List elements are relative to each other but once canonicalized, elements are
-- relative to the directory of the project root.
newtype ProjectConfigPath = ProjectConfigPath (NonEmpty FilePath)
    deriving (Eq, Show, Generic)

instance Binary ProjectConfigPath
instance Structured ProjectConfigPath

-- | Renders the path like this;
-- @
-- D.config
--   imported by: C.config
--   imported by: B.config
--   imported by: A.project
-- @
-- >>> render . docProjectConfigPath $ ProjectConfigPath $ "D.config" :| ["C.config", "B.config", "A.project" ]
-- "D.config\n  imported by: C.config\n  imported by: B.config\n  imported by: A.project"
docProjectConfigPath :: ProjectConfigPath -> Doc
docProjectConfigPath (ProjectConfigPath (p :| [])) = text p
docProjectConfigPath (ProjectConfigPath (p :| ps)) = vcat $
    text p : [ text " " <+> text "imported by:" <+> text l | l <- ps ]

-- | A message for a cyclical import, assuming the head of the path is the
-- duplicate.
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg path@(ProjectConfigPath (duplicate :| _)) =
    vcat
    [ text "cyclical import of" <+> text duplicate <> semi
    , nest 2 (docProjectConfigPath path)
    ]

docProjectConfigPathFailReason :: VR -> ProjectConfigPath -> Doc
docProjectConfigPathFailReason vr pcp
    | ProjectConfigPath (p :| []) <- pcp =
        constraint p
    | ProjectConfigPath (p :| ps) <- pcp = vcat
        [ constraint p
        , cat [nest 2 $ text "imported by:" <+> text l | l <- ps ]
        ]
    where
        pathRequiresVersion p = text p <+> text "requires" <+> text (prettyShow vr)
        constraint p = parens $ text "constraint from" <+> pathRequiresVersion p

-- | The root of the path, the project itself.
projectConfigPathRoot :: ProjectConfigPath -> FilePath
projectConfigPathRoot (ProjectConfigPath xs) = last xs

-- | Used by some tests as a dummy "unused" project root.
nullProjectConfigPath :: ProjectConfigPath
nullProjectConfigPath = ProjectConfigPath $ "unused" :| []

-- | Check if the path has duplicates. A cycle of imports is not allowed. This
-- check should only be done after the path has been canonicalized with
-- @canonicalizeConfigPath@. This is because the import path may contain paths
-- that are the same in relation to their importers but different in relation to
-- the project root directory.
hasDuplicatesConfigPath :: ProjectConfigPath -> Bool
hasDuplicatesConfigPath (ProjectConfigPath p) = length p /= length (NE.nub p)

-- | Prepends the path of the importee to the importer path.
consProjectConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
consProjectConfigPath p ps = ProjectConfigPath (p <| coerce ps)

-- | Make paths relative to the directory of the root of the project, not
-- relative to the file they were imported from.
makeRelativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
makeRelativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath
    $ (\segment -> (if isURI segment then segment else makeRelative dir segment))
    <$> p

-- | Normalizes and canonicalizes paths so that '..' segments can be removed.
-- This function is idempotent.
--
-- It converts paths like this:
-- @
-- └─ hops-0.project
--    └─ hops/hops-1.config
--       └─ ../hops-2.config
--          └─ hops/hops-3.config
--             └─ ../hops-4.config
--                └─ hops/hops-5.config
--                   └─ ../hops-6.config
--                      └─ hops/hops-7.config
--                         └─ ../hops-8.config
--                            └─ hops/hops-9.config
-- @
-- 
-- Into paths like this:
-- @
-- └─ hops-0.project
--    └─ hops/hops-1.config
--       └─ hops-2.config
--          └─ hops/hops-3.config
--             └─ hops-4.config
--                └─ hops/hops-5.config
--                   └─ hops-6.config
--                      └─ hops/hops-7.config
--                         └─ hops-8.config
--                            └─ hops/hops-9.config
-- @
--
-- That way we have @hops-8.config@ instead of
-- @./hops/../hops/../hops/../hops/../hops-8.config@.
--
-- Let's see how @canonicalizePath@ works that is used in the implementation
-- then we'll see how @canonicalizeConfigPath@ works. Each time we'll use these
-- paths;
-- * @hops/hops-1.config@
-- * @hops/../hops-2.config@
-- * @hops/../hops/hops-3.config@
-- * @hops/../hops/../hops/../hops/../hops-8.config@
-- * @hops/../hops/../hops/../hops/../hops/hops-9.config@
--
-- >>> let d = testDir
-- >>> p <- canonicalizePath (d </> "hops/hops-1.config")
-- >>> makeRelative d <$> canonicalizePath p
-- "hops/hops-1.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizePath (d </> "hops/../hops-2.config")
-- >>> makeRelative d <$> canonicalizePath p
-- "hops-2.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizePath (d </> "hops/../hops/hops-3.config")
-- >>> makeRelative d <$> canonicalizePath p
-- "hops/hops-3.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizePath (d </> "hops/../hops/../hops/../hops/../hops-8.config")
-- >>> makeRelative d <$> canonicalizePath p
-- "hops-8.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizePath (d </> "hops/../hops/../hops/../hops/../hops/hops-9.config")
-- >>> makeRelative d <$> canonicalizePath p
-- "hops/hops-9.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/hops-1.config") :| [])
-- >>> render . docProjectConfigPath <$> canonicalizeConfigPath d p
-- "hops/hops-1.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/../hops-2.config") :| [])
-- >>> render . docProjectConfigPath <$> canonicalizeConfigPath d p
-- "hops-2.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/../hops/hops-3.config") :| [])
-- >>> render . docProjectConfigPath <$> canonicalizeConfigPath d p
-- "hops/hops-3.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/../hops/../hops/../hops/../hops-8.config") :| [])
-- >>> render . docProjectConfigPath <$> canonicalizeConfigPath d p
-- "hops-8.config"
--
-- >>> let d = testDir
-- >>> p <- canonicalizeConfigPath d (ProjectConfigPath $ (d </> "hops/../hops/../hops/../hops/../hops/hops-9.config") :| [])
-- >>> render . docProjectConfigPath <$> canonicalizeConfigPath d p
-- "hops/hops-9.config"
canonicalizeConfigPath :: FilePath -> ProjectConfigPath -> IO ProjectConfigPath
canonicalizeConfigPath dir (ProjectConfigPath p) = do
   d <- makeAbsolute dir
   xs <- sequence $ NE.scanr (\importee -> (>>= \importer ->
        if isURI importee
            then pure importee
            else canonicalizePath $ d </> takeDirectory importer </> importee))
        (pure ".") p
   return . makeRelativeConfigPath d . ProjectConfigPath . NE.fromList $ NE.init xs

isURI :: FilePath -> Bool
isURI = isJust  .parseURI

-- $setup
-- >>> import Data.List
-- >>> testDir <- canonicalizePath "../cabal-testsuite/PackageTests/ConditionalAndImport"
