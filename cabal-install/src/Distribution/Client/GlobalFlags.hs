{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.GlobalFlags
  ( GlobalFlags (..)
  , globalFlagsOptions
  , defaultGlobalFlags
  , RepoContext (..)
  , withRepoContext
  , withRepoContext'
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HttpUtils
  ( HttpTransport
  , configureTransport
  )
import Distribution.Client.Types
  ( LocalRepo (..)
  , RemoteRepo (..)
  , Repo (..)
  , localRepoCacheKey
  , unRepoName
  )
import Distribution.Simple.Command
  ( ArgPlaceHolder
  , CommandUI (..)
  , MkOptDescr
  , OptionField
  , ShowOrParseArgs (..)
  , commandShowOptions
  , option
  , reqArg
  , reqArg'
  )
import Distribution.Simple.Flag
  ( Flag
  , flagToList
  , flagToMaybe
  , fromFlag
  , toFlag
  , pattern Flag
  )
import Distribution.Simple.Setup
  ( trueArg
  )
import Distribution.Simple.Utils
  ( info
  , warn
  )
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  , toNubList
  )

import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )

import Control.Concurrent
  ( MVar
  , modifyMVar
  , newMVar
  )
import qualified Data.Map as Map
import Distribution.ReadE
  ( parsecToReadE
  , succeedReadE
  )
import Network.URI
  ( URI
  , uriPath
  , uriScheme
  )
import System.FilePath
  ( isAbsolute
  , (</>)
  )

import qualified Distribution.Client.Security.DNS as Sec.DNS
import qualified Distribution.Client.Security.HTTP as Sec.HTTP
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Client.Repository.Cache as Sec
import qualified Hackage.Security.Client.Repository.Local as Sec.Local
import qualified Hackage.Security.Client.Repository.Remote as Sec.Remote
import qualified Hackage.Security.Util.Path as Sec
import qualified Hackage.Security.Util.Pretty as Sec
import qualified Text.PrettyPrint as PP

-- ------------------------------------------------------------

-- * Global flags

-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags
  { globalVersion :: Flag Bool
  , globalFullVersion :: Flag Bool
  , globalNumericVersion :: Flag Bool
  , globalConfigFile :: Flag FilePath
  , globalConstraintsFile :: Flag FilePath
  , globalRemoteRepos :: NubList RemoteRepo
  -- ^ Available Hackage servers.
  , globalCacheDir :: Flag FilePath
  , globalLocalNoIndexRepos :: NubList LocalRepo
  , globalActiveRepos :: Flag ActiveRepos
  , globalLogsDir :: Flag FilePath
  , globalIgnoreExpiry :: Flag Bool
  -- ^ Ignore security expiry dates
  , globalHttpTransport :: Flag String
  , globalStoreDir :: Flag FilePath
  , globalProgPathExtra :: NubList FilePath
  -- ^ Extra program path used for packagedb lookups in a global context (i.e. for http transports)
  }
  deriving (Show, Generic)
  deriving (Semigroup, Monoid) via Generically GlobalFlags

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
  GlobalFlags
    { globalVersion = Flag False
    , globalFullVersion = Flag False
    , globalNumericVersion = Flag False
    , globalConfigFile = mempty
    , globalConstraintsFile = mempty
    , globalRemoteRepos = mempty
    , globalCacheDir = mempty
    , globalLocalNoIndexRepos = mempty
    , globalActiveRepos = mempty
    , globalLogsDir = mempty
    , globalIgnoreExpiry = Flag False
    , globalHttpTransport = mempty
    , globalStoreDir = mempty
    , globalProgPathExtra = mempty
    }

instance Pretty GlobalFlags where
  pretty flags =
    PP.text . unwords $
      commandShowOptions
        ( CommandUI
            { commandName = ""
            , commandSynopsis = ""
            , commandUsage = const ""
            , commandDescription = Nothing
            , commandNotes = Nothing
            , commandDefaultFlags = defaultGlobalFlags
            , commandOptions = globalFlagsOptions
            }
        )
        flags

globalFlagsOptions :: ShowOrParseArgs -> [OptionField GlobalFlags]
globalFlagsOptions showOrParseArgs =
  case showOrParseArgs of
    ShowArgs -> argsShown
    ParseArgs -> argsShown ++ argsNotShown
  where
    -- arguments we want to show in the help
    argsShown =
      [ option
          ['V']
          ["version"]
          "Print version information"
          globalVersion
          (\v flags' -> flags'{globalVersion = v})
          trueArg
      , option
          []
          ["full-version"]
          "Print full version information with git revision (if available) and compiler"
          globalFullVersion
          (\v flags' -> flags'{globalFullVersion = v})
          trueArg
      , option
          []
          ["numeric-version"]
          "Print just the version number"
          globalNumericVersion
          (\v flags' -> flags'{globalNumericVersion = v})
          trueArg
      , option
          []
          ["config-file"]
          "Set an alternate location for the config file"
          globalConfigFile
          (\v flags' -> flags'{globalConfigFile = v})
          (reqArgFlag "FILE")
      , option
          []
          ["ignore-expiry"]
          "Ignore expiry dates on signed metadata (use only in exceptional circumstances)"
          globalIgnoreExpiry
          (\v flags' -> flags'{globalIgnoreExpiry = v})
          trueArg
      , option
          []
          ["http-transport"]
          "Set a transport for http(s) requests. Accepts 'curl', 'wget', 'powershell', and 'plain-http'. (default: 'curl')"
          globalHttpTransport
          (\v flags' -> flags'{globalHttpTransport = v})
          (reqArgFlag "HttpTransport")
      , option
          []
          ["store-dir", "storedir"]
          "The location of the build store"
          globalStoreDir
          (\v flags' -> flags'{globalStoreDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["active-repositories"]
          "The active package repositories (set to ':none' to disable all repositories)"
          globalActiveRepos
          (\v flags' -> flags'{globalActiveRepos = v})
          ( reqArg
              "REPOS"
              ( parsecToReadE
                  (\err -> "Error parsing active-repositories: " ++ err)
                  (toFlag `fmap` parsec)
              )
              (map prettyShow . flagToList)
          )
      ]

    -- arguments we don't want shown in the help
    -- the remote repo flags are not useful compared to the more general "active-repositories" flag.
    -- the global logs directory was only used in v1, while in v2 we have specific project config logs dirs
    -- default-user-config is support for a relatively obscure workflow for v1-freeze.
    argsNotShown =
      [ option
          []
          ["remote-repo"]
          "The name and url for a remote repository"
          globalRemoteRepos
          (\v flags' -> flags'{globalRemoteRepos = v})
          (reqArg' "NAME:URL" (toNubList . maybeToList . readRemoteRepo) (map showRemoteRepo . fromNubList))
      , option
          []
          ["local-no-index-repo"]
          "The name and a path for a local no-index repository"
          globalLocalNoIndexRepos
          (\v flags' -> flags'{globalLocalNoIndexRepos = v})
          (reqArg' "NAME:PATH" (toNubList . maybeToList . readLocalRepo) (map showLocalRepo . fromNubList))
      , option
          []
          ["remote-repo-cache"]
          "The location where downloads from all remote repos are cached"
          globalCacheDir
          (\v flags' -> flags'{globalCacheDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["logs-dir", "logsdir"]
          "The location to put log files"
          globalLogsDir
          (\v flags' -> flags'{globalLogsDir = v})
          (reqArgFlag "DIR")
      , option
          []
          ["default-user-config"]
          "Set a location for a cabal.config file for projects without their own cabal.config freeze file."
          globalConstraintsFile
          (\v flags' -> flags'{globalConstraintsFile = v})
          (reqArgFlag "FILE")
      ]

reqArgFlag
  :: ArgPlaceHolder
  -> MkOptDescr (b -> Flag String) (Flag String -> b -> b) b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

showRemoteRepo :: RemoteRepo -> String
showRemoteRepo = prettyShow

readRemoteRepo :: String -> Maybe RemoteRepo
readRemoteRepo = simpleParsec

showLocalRepo :: LocalRepo -> String
showLocalRepo = prettyShow

readLocalRepo :: String -> Maybe LocalRepo
readLocalRepo = simpleParsec

-- ------------------------------------------------------------

-- * Repo context

-- ------------------------------------------------------------

-- | Access to repositories
data RepoContext = RepoContext
  { repoContextRepos :: [Repo]
  -- ^ All user-specified repositories
  , repoContextGetTransport :: IO HttpTransport
  -- ^ Get the HTTP transport
  --
  -- The transport will be initialized on the first call to this function.
  --
  -- NOTE: It is important that we don't eagerly initialize the transport.
  -- Initializing the transport is not free, and especially in contexts where
  -- we don't know a priori whether or not we need the transport (for instance
  -- when using cabal in "nix mode") incurring the overhead of transport
  -- initialization on _every_ invocation (eg @cabal build@) is undesirable.
  , repoContextWithSecureRepo
      :: forall a
       . Repo
      -> (forall down. Sec.Repository down -> IO a)
      -> IO a
  -- ^ Get the (initialized) secure repo
  --
  -- (the 'Repo' type itself is stateless and must remain so, because it
  -- must be serializable)
  , repoContextIgnoreExpiry :: Bool
  -- ^ Should we ignore expiry times (when checking security)?
  }

-- | Wrapper around 'Repository', hiding the type argument
data SecureRepo = forall down. SecureRepo (Sec.Repository down)

withRepoContext :: Verbosity -> GlobalFlags -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity globalFlags =
  withRepoContext'
    verbosity
    (fromNubList (globalRemoteRepos globalFlags))
    (fromNubList (globalLocalNoIndexRepos globalFlags))
    (fromFlag (globalCacheDir globalFlags))
    (flagToMaybe (globalHttpTransport globalFlags))
    (flagToMaybe (globalIgnoreExpiry globalFlags))
    (fromNubList (globalProgPathExtra globalFlags))

withRepoContext'
  :: Verbosity
  -> [RemoteRepo]
  -> [LocalRepo]
  -> FilePath
  -> Maybe String
  -> Maybe Bool
  -> [FilePath]
  -> (RepoContext -> IO a)
  -> IO a
withRepoContext'
  verbosity
  remoteRepos
  localNoIndexRepos
  sharedCacheDir
  httpTransport
  ignoreExpiry
  extraPaths = \callback -> do
    for_ localNoIndexRepos $ \local ->
      unless (isAbsolute (localRepoPath local)) $
        warn verbosity $
          "file+noindex " ++ unRepoName (localRepoName local) ++ " repository path is not absolute; this is fragile, and not recommended"

    transportRef <- newMVar Nothing
    let httpLib =
          Sec.HTTP.transportAdapter
            verbosity
            (getTransport transportRef)
    initSecureRepos verbosity httpLib secureRemoteRepos $ \secureRepos' ->
      callback
        RepoContext
          { repoContextRepos =
              allRemoteRepos
                ++ allLocalNoIndexRepos
          , repoContextGetTransport = getTransport transportRef
          , repoContextWithSecureRepo = withSecureRepo secureRepos'
          , repoContextIgnoreExpiry = fromMaybe False ignoreExpiry
          }
    where
      secureRemoteRepos =
        [(remote, cacheDir) | RepoSecure remote cacheDir <- allRemoteRepos]

      allRemoteRepos :: [Repo]
      allRemoteRepos =
        [ (if isSecure then RepoSecure else RepoRemote) remote cacheDir
        | remote <- remoteRepos
        , let cacheDir = sharedCacheDir </> unRepoName (remoteRepoName remote)
              isSecure = remoteRepoSecure remote == Just True
        ]

      allLocalNoIndexRepos :: [Repo]
      allLocalNoIndexRepos =
        [ RepoLocalNoIndex local cacheDir
        | local <- localNoIndexRepos
        , let cacheDir
                | localRepoSharedCache local = sharedCacheDir </> localRepoCacheKey local
                | otherwise = localRepoPath local
        ]

      getTransport :: MVar (Maybe HttpTransport) -> IO HttpTransport
      getTransport transportRef =
        modifyMVar transportRef $ \mTransport -> do
          transport <- case mTransport of
            Just tr -> return tr
            Nothing -> configureTransport verbosity extraPaths httpTransport
          return (Just transport, transport)

      withSecureRepo
        :: Map Repo SecureRepo
        -> Repo
        -> (forall down. Sec.Repository down -> IO a)
        -> IO a
      withSecureRepo secureRepos repo callback =
        case Map.lookup repo secureRepos of
          Just (SecureRepo secureRepo) -> callback secureRepo
          Nothing -> throwIO $ userError "repoContextWithSecureRepo: unknown repo"

-- | Initialize the provided secure repositories
--
-- Assumed invariant: `remoteRepoSecure` should be set for all these repos.
initSecureRepos
  :: forall a
   . Verbosity
  -> Sec.HTTP.HttpLib
  -> [(RemoteRepo, FilePath)]
  -> (Map Repo SecureRepo -> IO a)
  -> IO a
initSecureRepos verbosity httpLib repos callback = go Map.empty repos
  where
    go :: Map Repo SecureRepo -> [(RemoteRepo, FilePath)] -> IO a
    go !acc [] = callback acc
    go !acc ((r, cacheDir) : rs) = do
      cachePath <- Sec.makeAbsolute $ Sec.fromFilePath cacheDir
      initSecureRepo verbosity httpLib r cachePath $ \r' ->
        go (Map.insert (RepoSecure r cacheDir) r' acc) rs

-- | Initialize the given secure repo
--
-- The security library has its own concept of a "local" repository, distinct
-- from @cabal-install@'s; these are secure repositories, but live in the local
-- file system. We use the convention that these repositories are identified by
-- URLs of the form @file:/path/to/local/repo@.
initSecureRepo
  :: Verbosity
  -> Sec.HTTP.HttpLib
  -> RemoteRepo
  -- ^ Secure repo ('remoteRepoSecure' assumed)
  -> Sec.Path Sec.Absolute
  -- ^ Cache dir
  -> (SecureRepo -> IO a)
  -- ^ Callback
  -> IO a
initSecureRepo verbosity httpLib RemoteRepo{..} cachePath = \callback -> do
  requiresBootstrap <- withRepo [] Sec.requiresBootstrap

  mirrors <-
    if requiresBootstrap
      then do
        info verbosity $
          "Trying to locate mirrors via DNS for "
            ++ "initial bootstrap of secure "
            ++ "repository '"
            ++ show remoteRepoURI
            ++ "' ..."

        Sec.DNS.queryBootstrapMirrors verbosity remoteRepoURI
      else pure []

  withRepo mirrors $ \r -> do
    when requiresBootstrap $
      Sec.uncheckClientErrors $
        Sec.bootstrap
          r
          (map Sec.KeyId remoteRepoRootKeys)
          (Sec.KeyThreshold (fromIntegral remoteRepoKeyThreshold))
    callback $ SecureRepo r
  where
    -- Initialize local or remote repo depending on the URI
    withRepo :: [URI] -> (forall down. Sec.Repository down -> IO a) -> IO a
    withRepo _ callback | uriScheme remoteRepoURI == "file:" = do
      dir <- Sec.makeAbsolute $ Sec.fromFilePath (uriPath remoteRepoURI)
      Sec.Local.withRepository
        dir
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback
    withRepo mirrors callback =
      Sec.Remote.withRepository
        httpLib
        (remoteRepoURI : mirrors)
        Sec.Remote.defaultRepoOpts
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback

    cache :: Sec.Cache
    cache =
      Sec.Cache
        { cacheRoot = cachePath
        , cacheLayout =
            Sec.cabalCacheLayout
              { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
              , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
              , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
              }
        }

    cacheFn :: FilePath -> Sec.CachePath
    cacheFn = Sec.rootPath . Sec.fragment

    -- We display any TUF progress only in verbose mode, including any transient
    -- verification errors. If verification fails, then the final exception that
    -- is thrown will of course be shown.
    logTUF :: Sec.LogMessage -> IO ()
    logTUF = info verbosity . Sec.pretty
