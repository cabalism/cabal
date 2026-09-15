{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Project configuration imports.
module Distribution.Client.ProjectConfig.Import
  ( -- * Parsing skeleton
    ProjectConfigSkeleton
  , projectSkeletonImports
  , fetchImport

    -- * Import modifiers
  , ImportSpec (..)
  , parseImportSpec
  , hideConstraints
  , hideImportConstraints

    -- * Messages
  , docProjectConfigFiles
  , cyclicalImportMsg
  , untrimmedUriImportMsg
  , hiddenConstraintsMsg
  , unusedHideConstraintsMsg

    -- * Checks
  , reportDuplicateImports
  ) where

import Control.Arrow (Kleisli (..), arr, second, (>>>))
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List ((\\))
import qualified Data.Map as Map
import Distribution.Client.Compat.Prelude hiding (empty, (<>))
import qualified Distribution.Client.Compat.Prelude as Prelude ((<>))
import Distribution.Client.HttpUtils
import qualified Distribution.Client.ProjectConfig.Lens as L
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.Targets (UserConstraint, userConstraintPackageName)
import Distribution.Compat.Lens (Lens', over, view)
import Distribution.PackageDescription (ConfVar (..))
import Distribution.Parsec (parsecCommaList)
import Distribution.Simple.Utils (debug, info, noticeDoc, ordNub)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import Distribution.Solver.Types.ProjectConfigPath
import Distribution.Types.CondTree (CondTree (..), mapTreeData, traverseCondTreeA)
import Distribution.Types.PackageName (PackageName)
import Distribution.Utils.String (trim)
import Network.URI (URI (..), parseURI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (isAbsolute, isPathSeparator, makeValid, (</>))
import Text.PrettyPrint (Doc, comma, empty, hsep, int, nest, punctuate, render, semi, text, vcat, (<>))

-- | ProjectConfigSkeleton is a tree of conditional blocks and imports wrapping
-- a config. It can be finalized by providing the conditional resolution info
-- and then resolving and downloading the imports
type ProjectConfigSkeleton = CondTree ConfVar ([(Maybe URI, ProjectConfigPath)], ProjectConfig)

projectSkeletonImports :: ProjectConfigSkeleton -> [(Maybe URI, ProjectConfigPath)]
projectSkeletonImports = fst . view traverseCondTreeA

-- | Fetch a local file import or remote URL import and parse it.
fetchImport
  :: (ProjectConfigToParse -> IO a)
  -> FilePath
  -> HttpTransport
  -> Verbosity
  -> FilePath
  -> ProjectConfigPath
  -> IO (Maybe URI, a)
fetchImport parser cacheDir httpTransport verbosity projectDir normLocPath =
  fetchImportConfig normLocPath >>= runKleisli (second (arr ProjectConfigToParse >>> Kleisli parser))
  where
    fetchImportConfig :: ProjectConfigPath -> IO (Maybe URI, BS.ByteString)
    fetchImportConfig (ProjectConfigPath (pci :| _)) = do
      debug verbosity $ "fetching import: " ++ pci
      let mbUri = parseURI (trim pci)
      (mbUri,) <$> case mbUri of
        Just uri -> do
          let fp = cacheDir </> map (\x -> if isPathSeparator x then '_' else x) (makeValid $ show uri)
          createDirectoryIfMissing True cacheDir
          _ <- downloadURI httpTransport verbosity uri fp
          BS.readFile fp
        Nothing ->
          BS.readFile $
            if isAbsolute pci then pci else coerce projectDir </> pci

-- | An import location with any modifiers indented beneath it, like this:
--
-- > import: https://www.stackage.org/lts-21.19/cabal.config
-- >   hide-constraints: hashable, text
data ImportSpec = ImportSpec
  { importSpecLoc :: FilePath
  -- ^ The location of the import, a file path or a URI.
  , importSpecHideConstraints :: [PackageName]
  -- ^ Packages whose constraints are hidden from the import and from anything
  -- that it imports in turn.
  }
  deriving (Eq, Show)

-- | Parses the lines of an import field. The first line is the import location
-- and any lines that follow are modifiers.
--
-- >>> importSpecLoc <$> parseImportSpec ["cabal.config"]
-- Right "cabal.config"
--
-- >>> fmap prettyShow . importSpecHideConstraints <$> parseImportSpec ["cabal.config", "hide-constraints: hashable, text", "hide-constraints: aeson"]
-- Right ["hashable","text","aeson"]
--
-- >>> importSpecLoc <$> parseImportSpec ["cabal.config", "hide-constraint: hashable"]
-- Left "unknown import modifier \"hide-constraint\", the only import modifier is hide-constraints"
--
-- >>> importSpecLoc <$> parseImportSpec ["cabal.config", "hide-constraints: hashable ==1.4.2.0"]
-- Left "expected a comma-separated list of package names for hide-constraints, got \"hashable ==1.4.2.0\""
parseImportSpec :: [String] -> Either String ImportSpec
parseImportSpec [] = Left "missing import location"
parseImportSpec (loc : modifiers)
  | null (trim loc) = Left "missing import location"
  | otherwise = ImportSpec loc . concat <$> traverse parseModifier modifiers
  where
    parseModifier :: String -> Either String [PackageName]
    parseModifier line = case break (== ':') line of
      (trim -> "hide-constraints", ':' : (trim -> pkgs)) ->
        either
          (const . Left $ "expected a comma-separated list of package names for hide-constraints, got " ++ show pkgs)
          Right
          (explicitEitherParsec (parsecCommaList parsec) pkgs)
      (trim -> name, ':' : _) ->
        Left $ "unknown import modifier " ++ show name ++ ", the only import modifier is hide-constraints"
      _ ->
        Left $ "expected an import modifier, like hide-constraints: <packages>, got " ++ show (trim line)

-- | Hides the constraints on the given packages from a parsed import, including
-- from its conditionals and from anything that it imports in turn. Returns the
-- skeleton without these constraints and the constraints that were hidden.
hideConstraints :: [PackageName] -> ProjectConfigSkeleton -> (ProjectConfigSkeleton, [(UserConstraint, ConstraintSource)])
hideConstraints pkgs skeleton =
  ( mapTreeData (second $ over constraints (filter (not . isHidden))) skeleton
  , foldMap (filter isHidden . view constraints . snd) skeleton
  )
  where
    constraints :: Lens' ProjectConfig [(UserConstraint, ConstraintSource)]
    constraints = L.projectConfigShared . L.projectConfigConstraints

    isHidden = (`elem` pkgs) . userConstraintPackageName . fst

-- | Hides constraints from a parsed import, logging the constraints hidden and
-- warning about any package that had no constraints to hide.
hideImportConstraints :: Verbosity -> ProjectConfigPath -> [PackageName] -> ProjectConfigSkeleton -> IO ProjectConfigSkeleton
hideImportConstraints _ _ [] skeleton = pure skeleton
hideImportConstraints verbosity importPath pkgs skeleton = do
  let (skeleton', hidden) = hideConstraints pkgs skeleton
      unused = ordNub pkgs \\ (userConstraintPackageName . fst <$> hidden)
  unless (null hidden) . info verbosity . render $ hiddenConstraintsMsg importPath hidden
  unless (null unused) . noticeDoc verbosity $ unusedHideConstraintsMsg (text "Warning:") importPath unused
  pure skeleton'

-- | Not just any file path. The project itself.
newtype ProjectFilePath = ProjectFilePath FilePath
  deriving (Eq, Generic)

-- | Isomorphic with 'ProjectConfigPath' but with separate constructors for the
-- root, imported file and imported URI.
data ProjectNode a where
  ProjectRoot :: FilePath -> ProjectNode ProjectFilePath
  ProjectFileImport :: FilePath -> ProjectConfigPath -> ProjectNode FilePath
  ProjectUriImport :: URI -> ProjectConfigPath -> ProjectNode URI

instance Eq (ProjectNode a) where
  (==) a b
    | ProjectRoot root <- a
    , ProjectRoot root' <- b =
        root == root'
    | ProjectFileImport importOf importBy <- a
    , ProjectFileImport importOf' importBy' <- b =
        (==)
          (consProjectConfigPath importOf importBy)
          (consProjectConfigPath importOf' importBy')
    | ProjectUriImport importOf importBy <- a
    , ProjectUriImport importOf' importBy' <- b =
        (==)
          (consProjectConfigPath (show importOf) importBy)
          (consProjectConfigPath (show importOf') importBy')

instance Pretty (ProjectNode a) where
  pretty = \case
    ProjectRoot root -> text root
    ProjectFileImport importOf importBy -> pretty $ consProjectConfigPath importOf importBy
    ProjectUriImport importOf importBy -> pretty $ consProjectConfigPath (show importOf) importBy

instance Show (ProjectNode a) where show = prettyShow

-- | Sorts the same as 'ProjectConfigPath' does.
instance Ord (ProjectNode a) where
  compare =
    (compare :: ProjectConfigPath -> ProjectConfigPath -> Ordering)
      `on` ( \case
              ProjectRoot root -> ProjectConfigPath $ root :| []
              ProjectFileImport importOf importBy -> consProjectConfigPath importOf importBy
              ProjectUriImport importOf importBy -> consProjectConfigPath (show importOf) importBy
           )

-- | Renders the paths as a list without showing which path imports another,
-- like this;
--
-- >- cabal.project
-- >- project-cabal/constraints.config
-- >- project-cabal/ghc-latest.config
-- >- project-cabal/ghc-options.config
-- >- project-cabal/pkgs.config
-- >- project-cabal/pkgs/benchmarks.config
-- >- project-cabal/pkgs/buildinfo.config
-- >- project-cabal/pkgs/cabal.config
-- >- project-cabal/pkgs/install.config
-- >- project-cabal/pkgs/integration-tests.config
-- >- project-cabal/pkgs/tests.config
--
--
-- >>> :{
--   do
--     let ps =
--              [ ProjectConfigPath ("cabal.project" :| [])
--              , ProjectConfigPath ("project-cabal/constraints.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/ghc-latest.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/ghc-options.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs.config" :| ["cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/benchmarks.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/buildinfo.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/cabal.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/install.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/integration-tests.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              , ProjectConfigPath ("project-cabal/pkgs/tests.config" :| ["project-cabal/pkgs.config","cabal.project"])
--              ]
--     return . render $ docProjectConfigFiles ps
-- :}
-- "- cabal.project\n- project-cabal/constraints.config\n- project-cabal/ghc-latest.config\n- project-cabal/ghc-options.config\n- project-cabal/pkgs.config\n- project-cabal/pkgs/benchmarks.config\n- project-cabal/pkgs/buildinfo.config\n- project-cabal/pkgs/cabal.config\n- project-cabal/pkgs/install.config\n- project-cabal/pkgs/integration-tests.config\n- project-cabal/pkgs/tests.config"
--
-- The listing puts projects first, URLs last and sorts the other paths
-- lexically, dropping any duplicates, like this:
--
-- >- cabal.project
-- >- 0.config
-- >- 2.config
-- >- cfg/1.config
-- >- cfg/3.config
-- >- with-ghc.config
-- >- https://www.stackage.org/lts-21.25/cabal.config
--
-- >>> let p = ProjectConfigPath $ "cabal.project" :| []
-- >>> let a = ProjectConfigPath $ "0.config" :| ["cabal.project"]
-- >>> let b = ProjectConfigPath $ "cfg/1.config" :| ["0.config", "cabal.project"]
-- >>> let c = ProjectConfigPath $ "with.config" :| ["0.config", "cabal.project"]
-- >>> let d = ProjectConfigPath $ "2.config" :| ["cfg/1.config", "0.config", "cabal.project"]
-- >>> let e = ProjectConfigPath $ "cfg/3.config" :| ["2.config", "cfg/1.config", "0.config", "cabal.project"]
-- >>> let f = ProjectConfigPath $ "https://www.stackage.org/lts-21.25/cabal.config" :| ["2.config", "cfg/1.config", "0.config", "cabal.project"]
-- >>> let g = ProjectConfigPath $ "https://www.stackage.org/lts-21.25/cabal.config" :| ["cfg/3.config", "2.config", "cfg/1.config", "0.config", "cabal.project"]
-- >>> let ps = [p, a, b, c, d, e, f, g]
-- >>> render $ docProjectConfigFiles ps
-- "- cabal.project\n- 0.config\n- 2.config\n- cfg/1.config\n- cfg/3.config\n- with.config\n- https://www.stackage.org/lts-21.25/cabal.config"
docProjectConfigFiles :: [ProjectConfigPath] -> Doc
docProjectConfigFiles (sortBy compareLexicographically -> ps) =
  vcat
    [ text "-" <+> text p
    | p <- ordNub [p | ProjectConfigPath (p :| _) <- ps]
    ]

-- | A message for a cyclical import, a "cyclical import of".
cyclicalImportMsg :: ProjectConfigPath -> Doc
cyclicalImportMsg path@(ProjectConfigPath (duplicate :| _)) =
  seenImportMsg
    (text "cyclical import of" <+> text duplicate <> semi)
    (ProjectFileImport duplicate path)
    []

-- | A message for a duplicate import, a "duplicate import of". If a check for
-- cyclical imports has already been made then this would report a duplicate
-- import by two different paths.
duplicateImportMsg :: Doc -> ProjectNode a -> [ProjectNode a] -> Doc
duplicateImportMsg intro = seenImportMsg intro

seenImportMsg :: Doc -> ProjectNode a -> [ProjectNode a] -> Doc
seenImportMsg intro projectNode seenImports =
  vcat
    [ intro
    , maybe empty (nest 2 . docProjectConfigPath) path
    , nest 2 $
        vcat
          [ docProjectConfigPath i
          | Just i <- importBy <$> filter ((duplicate ==) . importOf) seenImports
          ]
    ]
  where
    duplicate = importOf projectNode
    path = importBy projectNode

    importOf :: ProjectNode a -> FilePath
    importOf = \case
      ProjectRoot dup -> dup
      ProjectFileImport dup _ -> dup
      ProjectUriImport dup _ -> show dup

    importBy :: ProjectNode a -> Maybe ProjectConfigPath
    importBy = \case
      ProjectRoot _ -> Nothing
      ProjectFileImport _ by -> Just by
      ProjectUriImport _ by -> Just by

-- | A message for an import that has leading or trailing spaces.
untrimmedUriImportMsg :: Doc -> ProjectConfigPath -> Doc
untrimmedUriImportMsg intro path =
  vcat
    [ intro <+> text "import has leading or trailing whitespace" <> semi
    , nest 2 (docProjectConfigPath path)
    ]

-- | A message listing the constraints hidden from an import by
-- hide-constraints, with the file that each constraint came from.
hiddenConstraintsMsg :: ProjectConfigPath -> [(UserConstraint, ConstraintSource)] -> Doc
hiddenConstraintsMsg path hidden =
  vcat
    [ text "hide-constraints hid these constraints of import" <> semi
    , nest 2 (docProjectConfigPath path)
    , nest 2 $ vcat [pretty c <+> text "from" <+> constraintFrom src | (c, src) <- hidden]
    ]
  where
    constraintFrom = \case
      ConstraintSourceProjectConfig (ProjectConfigPath (p :| _)) -> text p
      src -> pretty src

-- | A message for packages named by hide-constraints that had no constraints to
-- hide.
unusedHideConstraintsMsg :: Doc -> ProjectConfigPath -> [PackageName] -> Doc
unusedHideConstraintsMsg intro path pkgs =
  vcat
    [ intro <+> text "hide-constraints found no constraints to hide for" <+> hsep (punctuate comma (pretty <$> pkgs)) <> semi
    , nest 2 (docProjectConfigPath path)
    ]

-- | Detect and report any duplicate imports, including those missed when parsing.
--
-- Parsing catches cyclical imports and some but not all duplicate imports. In
-- particular, it doesn't catch when the same project configuration is imported
-- via different import paths.
reportDuplicateImports :: Verbosity -> ProjectConfigSkeleton -> IO ()
reportDuplicateImports verbosity skeleton = do
  let (dupeRoots, dupeFiles, dupeUris) = detectDupes $ projectSkeletonImports skeleton
  unless (Map.null dupeRoots) (noticeDoc verbosity $ vcat (dupesMsg <$> Map.toList dupeRoots))
  unless (Map.null dupeFiles) (noticeDoc verbosity $ vcat (dupesMsg <$> Map.toList dupeFiles))
  unless (Map.null dupeUris) (noticeDoc verbosity $ vcat (dupesMsg <$> Map.toList dupeUris))

toDupes :: Ord k => [(k, [ProjectNode a])] -> Map k [Dupes a]
toDupes xs =
  xs
    & Map.fromListWith (Prelude.<>)
    & Map.filter ((> 1) . length)
    <&> \ys -> [Dupes v ys | v <- ys]

detectDupes :: [(Maybe URI, ProjectConfigPath)] -> (DupesMap ProjectFilePath, DupesMap FilePath, DupesMap URI)
detectDupes xs = (toDupes roots, toDupes files, toDupes uris)
  where
    (<$$>) = fmap . fmap
    roots =
      [ (h, [ProjectRoot h])
      | (Nothing, (h, Nothing)) <- unconsProjectConfigPath <$$> xs
      ]
    files =
      [ (h, [ProjectFileImport h (consProjectConfigPath h t)])
      | (Nothing, (h, Just t)) <- unconsProjectConfigPath <$$> xs
      ]
    uris =
      [ (f, [ProjectUriImport u (consProjectConfigPath f t)])
      | (Just u, (f, Just t)) <- unconsProjectConfigPath <$$> xs
      , show u == f
      ]

data Dupes a = Dupes
  { dupesImport :: ProjectNode a
  -- ^ The import that we're checking for duplicates.
  , dupesImports :: [ProjectNode a]
  -- ^ All the imports of this file.
  }
  deriving (Eq)

instance Ord (Dupes a) where
  compare x y =
    (compare `on` length . dupesImports) x y
      `thenCmp` (compare `on` sort . dupesImports) x y
      `thenCmp` (compare `on` dupesImport) x y
    where
      thenCmp :: Ordering -> Ordering -> Ordering
      thenCmp EQ o2 = o2
      thenCmp o1 _ = o1

type DupesMap a = Map FilePath [Dupes a]

dupesMsg :: (FilePath, [Dupes a]) -> Doc
dupesMsg (duplicate, ds@(take 1 . sort -> dupes)) =
  vcat $
    ((text "Warning:" <+> int (length ds) <+> text "imports of" <+> text duplicate) <> semi)
      : ((\Dupes{..} -> duplicateImportMsg empty dupesImport (sort $ dupesImports \\ [dupesImport])) <$> dupes)

-- $setup
-- >>> import Text.PrettyPrint (render)
