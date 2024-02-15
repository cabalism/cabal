{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module Distribution.Solver.Types.ProjectConfigPath
    ( ProjectConfigPath(..)
    , projectConfigPathRoot
    , docProjectConfigPath
    , cyclicalImportMsg
    , duplicateImportMsg
    , docProjectConfigPathFailReason
    , nullProjectConfigPath
    , consProjectConfigPath
    , hasDuplicatesConfigPath
    , fullConfigPathRoot
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

showFR :: VR -> FilePath -> Doc
showFR vr p = text p <+> text "requires" <+> text (prettyShow vr)

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

duplicateImportMsg :: FilePath -> [(FilePath, ProjectConfigPath)] -> Doc
duplicateImportMsg uniqueImport dupImportsBy = vcat
    [ text "duplicate import of" <+> text uniqueImport <> semi
    , cat [nest 2 (docProjectConfigPath dib) | (_, dib) <- dupImportsBy]
    ]

cyclicalImportMsg :: FilePath -> ProjectConfigPath -> Doc
cyclicalImportMsg uniqueImport normLocPath = vcat
    [ text "cyclical import of" <+> text uniqueImport <> semi
    , nest 2 (docProjectConfigPath normLocPath)
    ]

docProjectConfigPathFailReason :: VR -> ProjectConfigPath -> Doc
docProjectConfigPathFailReason vr (ProjectConfigPath (p :| [])) =
    parens $ showFR vr p
docProjectConfigPathFailReason vr (ProjectConfigPath (p :| ps)) = vcat
    [ parens (showFR vr p)
    , cat [nest 2 $ text "imported by:" <+> text l | l <- ps ]
    ]

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

-- | If the project was a full path, we need to show the full path in messages
-- and do this by reconstructing the full path of the root (the project) from
-- its directory and file name.
fullConfigPathRoot :: FilePath -> ProjectConfigPath -> ProjectConfigPath
fullConfigPathRoot dir (ProjectConfigPath p) =
    ProjectConfigPath . NE.fromList
    $ NE.init p ++ [let root = NE.last p in dir </> root]

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
   return . makeRelativeConfigPath dir . ProjectConfigPath . NE.fromList $ NE.init xs

isURI :: FilePath -> Bool
isURI = isJust  .parseURI

-- $setup
-- >>> import Data.List
