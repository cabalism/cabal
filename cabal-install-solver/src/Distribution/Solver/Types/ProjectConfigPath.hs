{-# LANGUAGE ViewPatterns #-}

module Distribution.Solver.Types.ProjectConfigPath
    (
    -- * Project Config Path Manipulation
      ProjectConfigPath(..)
    , compareLexicographically
    , compareSegmentally
    , projectConfigPathRoot
    , nullProjectConfigPath
    , consProjectConfigPath
    , unconsProjectConfigPath
    , currentProjectConfigPath

    -- * Messages
    , docProjectConfigPath
    , docProjectImportedBy
    , docProjectConfigPathFailReason
    , quoteUntrimmed

    -- * Checks and Normalization
    , isCyclicConfigPath
    , isTopLevelConfigPath
    , isUntrimmedUriConfigPath
    , makeRelativeConfigPath
    , isURI
    ) where

import Distribution.Solver.Compat.Prelude hiding (toList, (<>))
import qualified Distribution.Solver.Compat.Prelude as P ((<>))
import Prelude ()

import Data.Coerce (coerce)
import Data.List.NonEmpty ((<|))
import Network.URI (parseURI, parseAbsoluteURI)
import System.FilePath hiding (splitPath)
import qualified System.FilePath as FP (splitPath)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import qualified Data.List.NonEmpty as NE
import Distribution.Solver.Modular.Version (VR)
import Distribution.Pretty (prettyShow, Pretty(..))
import Distribution.Utils.String (trim)
import Text.PrettyPrint
import Distribution.System (OS(Windows), buildOS)

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
    deriving (Eq, Generic)

instance Pretty ProjectConfigPath where pretty = docProjectConfigPath
instance Show ProjectConfigPath where show = prettyShow

-- | Sorts URIs after local file paths and longer file paths after shorter ones
-- as measured by the number of path segments. If still equal, then sorting is
-- lexical.
--
-- The project itself, a single element root path, compared to any of the
-- configuration paths it imports, should always sort first. Comparing one
-- project root path against another is done lexically.
--
-- For comparison purposes, path separators are normalized to the @buildOS@
-- platform's path separator.
--
-- >>> let abFwd = ProjectConfigPath $ "a/b.config" :| []
-- >>> let abBwd = ProjectConfigPath $ "a\\b.config" :| []
-- >>> compare abFwd abBwd
-- EQ
--
-- >>> let abc = ProjectConfigPath $ "a/b/c.config" :| []
-- >>> let yz = ProjectConfigPath $ "y/z.config" :| []
-- >>> (compare abc yz, let xs = [abc, yz] in xs == sort xs)
-- (GT,False)
--
-- >>> let abc = ProjectConfigPath $ "C.config" :| ["B.config", "A.project"]
-- >>> let bcd = ProjectConfigPath $ "D.config" :| ["C.config", "B.project"]
-- >>> (compare abc bcd, let xs = [abc, bcd] in xs == sort xs)
-- (LT,True)
--
-- >>> let abc = ProjectConfigPath $ "C.config" :| ["B.config", "A.project"]
-- >>> let yz = ProjectConfigPath $ "Z.config" :| ["Y.project"]
-- >>> (compare abc yz, let xs = [abc, yz] in xs == sort xs)
-- (GT,False)
instance Ord ProjectConfigPath where
    compare = compareSegmentally

-- | A comparison that puts projects first, URLs last and sorts the other paths
-- lexically.
compareLexicographically :: ProjectConfigPath -> ProjectConfigPath -> Ordering
compareLexicographically (ProjectConfigPath as) (ProjectConfigPath bs) =
        case (as, bs) of
            -- Single element paths are projects, they should always sort first.
            (a :| [], b :| []) -> compare (splitPath a) (splitPath b)
            (_ :| [], _) -> LT
            (_, _ :| []) -> GT

            (a :| aImporters, b :| bImporters) -> case (parseAbsoluteURI a, parseAbsoluteURI b) of
                (Just ua, Just ub) -> compare ua ub P.<> compare aImporters bImporters
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Nothing, Nothing) -> compare (splitPath a) (splitPath b) P.<> compare aImporters bImporters

-- | A comparison that puts projects first, URLs last and sorts the other paths
-- by putting longer paths after shorter ones as measured by the number of path
-- segments. If still equal, then sorting is lexical.
compareSegmentally:: ProjectConfigPath -> ProjectConfigPath -> Ordering
compareSegmentally pa@(ProjectConfigPath as) pb@(ProjectConfigPath bs) =
        case (as, bs) of
            -- There should only ever be one root project path, only one path
            -- with length 1. Comparing it to itself should be EQ. Don't assume
            -- this though, do a comparison anyway when both sides have length
            -- 1.  The root path, the project itself, should always be the first
            -- path in a sorted listing.
            (a :| [], b :| []) ->
                let aPaths = splitPath a
                    bPaths = splitPath b
                in
                    compare (length aPaths) (length bPaths)
                    P.<> compare aPaths bPaths

            (_ :| [], _) -> LT
            (_, _ :| []) -> GT

            (a :| _, b :| _) -> case (parseAbsoluteURI a, parseAbsoluteURI b) of
                (Just ua, Just ub) -> compare ua ub P.<> compare aImporters bImporters
                (Just _, Nothing) -> GT
                (Nothing, Just _) -> LT
                (Nothing, Nothing) ->
                    let aPaths = splitPath a
                        bPaths = splitPath b
                    in
                        compare (length as) (length bs)
                        P.<> compare asPaths bsPaths
                        P.<> compare (length aPaths) (length bPaths)
                        P.<> compare aPaths bPaths
                        P.<> compare aImporters bImporters
        where
            asPaths = splitPath <$> as
            bsPaths = splitPath <$> bs
            aImporters = snd $ unconsProjectConfigPath pa
            bImporters = snd $ unconsProjectConfigPath pb

splitPath :: FilePath -> [FilePath]
splitPath = FP.splitPath . normSep
    where
        normSep p =
            if buildOS == Windows then
                Windows.joinPath $ Windows.splitDirectories
                [if Posix.isPathSeparator c then Windows.pathSeparator else c| c <- p]
            else
                Posix.joinPath $ Posix.splitDirectories
                [if Windows.isPathSeparator c then Posix.pathSeparator else c| c <- p]

instance Binary ProjectConfigPath
instance NFData ProjectConfigPath
instance Structured ProjectConfigPath

-- | Renders the path like this;
--
-- >D.config
-- >  imported by: C.config
-- >  imported by: B.config
-- >  imported by: A.project
--
-- >>> render . docProjectConfigPath $ ProjectConfigPath $ "D.config" :| ["C.config", "B.config", "A.project"]
-- "D.config\n  imported by: C.config\n  imported by: B.config\n  imported by: A.project"
docProjectConfigPath :: ProjectConfigPath -> Doc
docProjectConfigPath (ProjectConfigPath (p :| [])) = quoteUntrimmed p
docProjectConfigPath (ProjectConfigPath (p :| ps)) = vcat $ quoteUntrimmed p :
    [ text " " <+> text "imported by:" <+> quoteUntrimmed l | l <- ps ]

-- | Render the paths which imports this config.
docProjectImportedBy :: ProjectConfigPath -> Doc
docProjectImportedBy (ProjectConfigPath (_ :| [])) = text ""
docProjectImportedBy (ProjectConfigPath (_ :| ps)) = vcat $
    [ text " " <+> text "imported by:" <+> quoteUntrimmed l | l <- ps ]

-- | If the path has leading or trailing spaces then show it quoted.
quoteUntrimmed :: FilePath -> Doc
quoteUntrimmed s = if trim s /= s then quotes (text s) else text s

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
isCyclicConfigPath :: ProjectConfigPath -> Bool
isCyclicConfigPath (ProjectConfigPath p) = length p /= length (NE.nub p)

-- | Check if the last segment of the path (root or importee) is a URI that has
-- leading or trailing spaces.
isUntrimmedUriConfigPath :: ProjectConfigPath -> Bool
isUntrimmedUriConfigPath (ProjectConfigPath (p :| _)) = let p' = trim p in p' /= p && isURI p'

-- | Check if the project config path is top-level, meaning it was not included by
-- some other project config.
isTopLevelConfigPath :: ProjectConfigPath -> Bool
isTopLevelConfigPath (ProjectConfigPath p) = NE.length p == 1

-- | Prepends the path of the importee to the importer path.
consProjectConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
consProjectConfigPath p ps = ProjectConfigPath (p <| coerce ps)

-- | Split the path into the importee and the importer path.
unconsProjectConfigPath :: ProjectConfigPath -> (FilePath, Maybe ProjectConfigPath)
unconsProjectConfigPath ps = fmap ProjectConfigPath <$> NE.uncons (coerce ps)

currentProjectConfigPath :: ProjectConfigPath -> FilePath
currentProjectConfigPath (ProjectConfigPath (p :| _)) = p

-- | Make paths relative to the directory of the root of the project, not
-- relative to the file they were imported from.
makeRelativeConfigPath :: FilePath -> ProjectConfigPath -> ProjectConfigPath
makeRelativeConfigPath dir (ProjectConfigPath p) =
    ProjectConfigPath
    $ (\segment@(trim -> trimSegment) -> (if isURI trimSegment then trimSegment else makeRelative dir segment))
    <$> p

isURI :: FilePath -> Bool
isURI = isJust . parseURI
