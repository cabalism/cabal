{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Project configuration imports.
module Distribution.Client.ProjectConfig.Import (reportDuplicateImports) where

import Data.Function ((&))
import Data.List ((\\))
import Text.PrettyPrint (Doc, int, semi, text, vcat)
import qualified Text.PrettyPrint as Disp (empty)
import Distribution.Client.ProjectConfig.Legacy
import Distribution.Solver.Types.ProjectConfigPath
import Network.URI (URI (..))
import qualified Data.Map as Map
import Distribution.Client.Compat.Prelude
import Data.Functor ((<&>))
import Distribution.Simple.Utils (noticeDoc)

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
toDupes xs = xs
    & Map.fromListWith (<>)
    & Map.filter ((> 1) . length)
    <&> \ys -> [Dupes v ys | v <- ys]

-- TODO: Sorting
detectDupes :: [(Maybe URI, ProjectConfigPath)] -> (DupesMap ProjectFilePath, DupesMap FilePath, DupesMap URI)
detectDupes xs = (toDupes roots', toDupes files', toDupes uris') where
    (<$$>) = fmap . fmap
    roots' =
      [ (h, [ProjectRoot h])
      | (Nothing, (h, Nothing)) <- unconsProjectConfigPath <$$> xs
      ]
    files' =
      [ (h, [ProjectFileImport h (consProjectConfigPath h t)])
      | (Nothing, (h, Just t)) <- unconsProjectConfigPath <$$> xs
      ]
    uris' =
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
      : ((\Dupes{..} -> duplicateImportMsg Disp.empty dupesImport (sort $ dupesImports \\ [dupesImport])) <$> dupes)
