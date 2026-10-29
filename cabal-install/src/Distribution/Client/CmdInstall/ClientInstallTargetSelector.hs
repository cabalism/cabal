module Distribution.Client.CmdInstall.ClientInstallTargetSelector
  ( WithoutProjectTargetSelector (..)
  , parseWithoutProjectTargetSelector
  , woPackageNames
  , woPackageTargets
  , woPackageSpecifiers
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI, parseURI, uriScheme)

import Distribution.Client.Errors
import Distribution.Client.TargetSelector
import Distribution.Client.Types
import Distribution.Compat.CharParsing (char, optional, string, try)
import Distribution.Package
import Distribution.Simple.LocalBuildInfo (ComponentName (CExeName))
import Distribution.Simple.Utils (dieWithException)

data WithoutProjectTargetSelector
  = WoPackageId PackageId
  | WoPackageComponent PackageId ComponentName
  | WoURI URI
  deriving (Show)

-- | Parse a target given to @cabal install@ outside of a project. Accepted
-- forms are a package name with an optional version (@pkg@, @pkg-1.2@), an
-- executable of such a package (@pkg:exename@ or @pkg:exe:exename@) and the
-- URI of a source tarball (@http:@, @https:@ or @file:@).
--
-- A colon-separated target that is not one of these, such as @pkg:exe:*@,
-- also parses as a URI with the package name as its scheme. Only the schemes
-- a tarball can be fetched from are taken as URIs, so that such a target is
-- reported as unknown rather than passed to the download tool. See #9817.
parseWithoutProjectTargetSelector :: Verbosity -> String -> IO WithoutProjectTargetSelector
parseWithoutProjectTargetSelector verbosity input =
  case explicitEitherParsec parser input of
    Right ts -> return ts
    Left _ -> case parseURI input of
      Just uri
        | uriScheme uri `elem` ["http:", "https:", "file:"] ->
            return (WoURI uri)
      _ -> dieWithException verbosity $ InstallTargetWithoutProject input
  where
    parser :: CabalParsing m => m WithoutProjectTargetSelector
    parser = do
      pid <- parsec
      cn <- optional (char ':' *> optional (try (string "exe:")) *> parsec)
      return $ case cn of
        Nothing -> WoPackageId pid
        Just cn' -> WoPackageComponent pid (CExeName cn')

woPackageNames :: WithoutProjectTargetSelector -> [PackageName]
woPackageNames (WoPackageId pid) = [pkgName pid]
woPackageNames (WoPackageComponent pid _) = [pkgName pid]
woPackageNames (WoURI _) = []

woPackageTargets :: WithoutProjectTargetSelector -> TargetSelector
woPackageTargets (WoPackageId pid) =
  TargetPackageNamed (pkgName pid) Nothing
woPackageTargets (WoPackageComponent pid cn) =
  TargetComponentUnknown (pkgName pid) (Right cn) WholeComponent
woPackageTargets (WoURI _) =
  TargetAllPackages (Just ExeKind)

woPackageSpecifiers :: WithoutProjectTargetSelector -> Either URI (PackageSpecifier pkg)
woPackageSpecifiers (WoPackageId pid) = Right (mkNamedPackage pid)
woPackageSpecifiers (WoPackageComponent pid _) = Right (mkNamedPackage pid)
woPackageSpecifiers (WoURI uri) = Left uri
