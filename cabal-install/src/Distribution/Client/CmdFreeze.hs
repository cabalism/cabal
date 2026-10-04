{-# LANGUAGE PatternSynonyms #-}

-- | cabal-install CLI command: freeze
module Distribution.Client.CmdFreeze
  ( freezeCommand
  , freezeAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.DistDirLayout
  ( DistDirLayout (distProjectFile)
  , ProjectFileKey (ProjectFileKeyFreeze)
  )
import Distribution.Client.IndexUtils (ActiveRepos, TotalIndexState, filterSkippedActiveRepos)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig
  ( ProjectConfig (..)
  , ProjectConfigShared (..)
  , writeProjectLocalFreezeConfig
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types (elabLibDependencies, elabSetupDependencies)
import Distribution.Client.Targets
  ( UserConstraint (..)
  , UserConstraintScope (..)
  , UserQualifier (..)
  )
import Distribution.Solver.Types.ConstraintSource
  ( ConstraintSource (..)
  )
import Distribution.Solver.Types.PackageConstraint
  ( PackageProperty (..)
  )

import Distribution.Client.Setup
  ( GlobalFlags
  )
import Distribution.Client.Types.ConfiguredId (ConfiguredId (confInstId))
import Distribution.Compat.Graph (nodeKey, nodeNeighbors)
import Distribution.Package
  ( PackageName
  , UnitId
  , newSimpleUnitId
  , packageName
  , packageVersion
  )
import Distribution.PackageDescription
  ( FlagAssignment
  , diffFlagAssignment
  , mkFlagAssignment
  , nullFlagAssignment
  , unFlagAssignment
  )
import Distribution.Simple.Flag (pattern Flag)
import Distribution.Simple.Utils
  ( dieWithException
  , notice
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )
import Distribution.Version
  ( Version
  , VersionRange
  , noVersion
  , simplifyVersionRange
  , thisVersion
  , unionVersionRanges
  )

import qualified Data.Map as Map
import qualified Data.Set as Set

import Distribution.Client.Errors
import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )

freezeCommand :: CommandUI (NixStyleFlags ())
freezeCommand =
  CommandUI
    { commandName = "v2-freeze"
    , commandSynopsis = "Freeze dependencies."
    , commandUsage = usageAlternatives "v2-freeze" ["[FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "The project configuration is frozen so that it will be reproducible "
            ++ "in future.\n\n"
            ++ "The precise dependency configuration for the project is written to "
            ++ "the 'cabal.project.freeze' file (or '$project_file.freeze' if "
            ++ "'--project-file' is specified). This file extends the configuration "
            ++ "from the 'cabal.project' file and thus is used as the project "
            ++ "configuration for all other commands (such as 'v2-build', "
            ++ "'v2-repl' etc).\n\n"
            ++ "The freeze file can be kept in source control. To make small "
            ++ "adjustments it may be edited manually, or to make bigger changes "
            ++ "you may wish to delete the file and re-freeze. For more control, "
            ++ "one approach is to try variations using 'v2-build --dry-run' with "
            ++ "solver flags such as '--constraint=\"pkg < 1.2\"' and once you have "
            ++ "a satisfactory solution to freeze it using the 'v2-freeze' command "
            ++ "with the same set of flags."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-freeze\n"
          ++ "    Freeze the configuration of the current project\n\n"
          ++ "  "
          ++ pname
          ++ " v2-build --dry-run --constraint=\"aeson < 1\"\n"
          ++ "    Check what a solution with the given constraints would look like\n"
          ++ "  "
          ++ pname
          ++ " v2-freeze --constraint=\"aeson < 1\"\n"
          ++ "    Freeze a solution using the given constraints\n"
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions = nixStyleOptions (const [])
    }

-- | To a first approximation, the @freeze@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- based on the install plan we write out a @cabal.project.freeze@ config file.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
freezeAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
freezeAction flags extraArgs globalFlags = do
  unless (null extraArgs) $
    dieWithException verbosity $
      FreezeAction extraArgs

  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    , buildSettings
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, totalIndexState, activeRepos) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages
      Nothing

  let freezeConfig = projectFreezeConfig elaboratedPlan totalIndexState activeRepos
      dryRun =
        buildSettingDryRun buildSettings
          || buildSettingOnlyDownload buildSettings

  if dryRun
    then notice verbosity "Freeze file not written due to flag(s)"
    else do
      writeProjectLocalFreezeConfig distDirLayout freezeConfig
      notice verbosity $
        "Wrote freeze file: " ++ distProjectFile distDirLayout ProjectFileKeyFreeze
  where
    verbosity = cfgVerbosity normal flags
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

-- | Given the install plan, produce a config value with constraints that
-- freezes the versions of packages used in the plan.
projectFreezeConfig
  :: ElaboratedInstallPlan
  -> TotalIndexState
  -> ActiveRepos
  -> ProjectConfig
projectFreezeConfig elaboratedPlan totalIndexState activeRepos0 =
  mempty
    { projectConfigShared =
        mempty
          { projectConfigConstraints =
              concat (Map.elems (projectFreezeConstraints elaboratedPlan))
          , projectConfigIndexState = Flag totalIndexState
          , projectConfigActiveRepos = Flag activeRepos
          }
    }
  where
    activeRepos :: ActiveRepos
    activeRepos = filterSkippedActiveRepos activeRepos0

-- | Given the install plan, produce solver constraints that will ensure the
-- solver picks the same solution again in future in different environments.
projectFreezeConstraints
  :: ElaboratedInstallPlan
  -> Map PackageName [(UserConstraint, ConstraintSource)]
projectFreezeConstraints plan =
  --
  -- TODO: [required eventually] this is currently an underapproximation
  -- since the constraints language is not expressive enough to specify the
  -- precise solution. See https://github.com/haskell/cabal/issues/3502.
  --
  -- A solution can have several instances of a package, in different scopes:
  -- at the top level, among the setup dependencies of a package, or among
  -- the dependencies of a build tool. We always write a constraint that
  -- allows every version of a package in the solution, in any scope. Where
  -- the top level or the setup scopes have fewer versions than that, we add
  -- a constraint for the scope. There is no syntax for the scope of a build
  -- tool, so two of those that differ are only constrained by the first
  -- constraint. See https://github.com/haskell/cabal/issues/9799.
  --
  -- We do not include any /version/ constraints for packages that are local
  -- to the project (e.g. if the solution has two instances of Cabal, one
  -- from the local project and one pulled in as a setup deps then we exclude
  -- all constraints on Cabal). We do however keep flag constraints of local
  -- packages.
  --
  -- A flag constraint applies to one scope. We constrain the flags of the
  -- top-level instance of a package, and the flags of an instance in a setup
  -- scope when it is not the top-level instance. If no instance is at the
  -- top level, or more than one is, we constrain the flags that they all
  -- give the same value. See https://github.com/haskell/cabal/issues/5134.
  --
  deleteLocalPackagesVersionConstraints
    (Map.unionWith (++) versionConstraints flagConstraints)
  where
    constraint scope property = (UserConstraint scope property, ConstraintSourceFreeze)

    -- A constraint for each scope, starting with the widest, leaving out
    -- those that say no more than a wider one.
    scoped
      :: Eq a
      => PackageName
      -> a
      -- in any scope
      -> Maybe a
      -- at the top level
      -> Maybe a
      -- in any setup scope
      -> [(PackageName, a)]
      -- in the setup scope of each package
      -> [(UserConstraintScope, a)]
    scoped p inAny atTopLevel inSetup inSetupOf =
      [(UserQualified UserQualToplevel p, x) | Just x <- [atTopLevel], x /= inAny]
        ++ [(UserAnySetupQualifier p, x) | Just x <- [inSetup], x /= inAny]
        ++ [ (UserQualified (UserQualSetup q) p, x)
           | (q, x) <- inSetupOf
           , Just x /= inSetup
           ]

    versionConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    versionConstraints =
      Map.mapWithKey
        ( \p versions ->
            [ constraint scope (PackagePropertyVersion (versionRange vs))
            | (scope, vs) <-
                (UserAnyQualifier p, versions)
                  : scoped
                    p
                    versions
                    (Map.lookup p topLevelVersions)
                    (Map.lookup p setupVersions)
                    [ (q, vs)
                    | (q, scopeVersions) <- setupScopeVersions
                    , Just vs <- [Map.lookup p scopeVersions]
                    ]
            ]
        )
        (versionsIn (InstallPlan.keysSet plan))
      where
        topLevelVersions = versionsIn topLevel
        setupVersions = versionsIn setupUnits
        setupScopeVersions = [(q, versionsIn units) | (q, units) <- Map.toList setupScopes]

    versionRange :: Set Version -> VersionRange
    versionRange =
      simplifyVersionRange . foldr (unionVersionRanges . thisVersion) noVersion

    -- The versions of each package among some units of the plan.
    versionsIn :: Set UnitId -> Map PackageName (Set Version)
    versionsIn units =
      Map.fromListWith
        Set.union
        [ (name, Set.singleton version)
        | pkg <- InstallPlan.toList plan
        , nodeKey pkg `Set.member` units
        , let (name, version) = case pkg of
                InstallPlan.PreExisting ipkg -> (packageName ipkg, packageVersion ipkg)
                InstallPlan.Configured elab -> (packageName elab, packageVersion elab)
                InstallPlan.Installed elab -> (packageName elab, packageVersion elab)
        ]

    flagConstraints :: Map PackageName [(UserConstraint, ConstraintSource)]
    flagConstraints =
      Map.map
        (map (\(scope, flags) -> constraint scope (PackagePropertyFlags flags)))
        (Map.unionWith (++) topLevelFlags setupFlags)

    topLevelFlags :: Map PackageName [(UserConstraintScope, FlagAssignment)]
    topLevelFlags =
      Map.mapWithKey (\p flags -> [(UserQualified UserQualToplevel p, flags)]) $
        Map.filter (not . nullFlagAssignment) $
          Map.map (agreedFlags . preferTopLevel) $
            Map.fromListWith
              (++)
              [ (packageName elab, [(elabUnitId elab `Set.member` topLevel, elabFlagAssignment elab)])
              | InstallPlan.Configured elab <- InstallPlan.toList plan
              ]

    -- The flags of the instances that are at the top level, or of all the
    -- instances if none is.
    preferTopLevel :: [(Bool, FlagAssignment)] -> [FlagAssignment]
    preferTopLevel instances =
      case [flags | (True, flags) <- instances] of
        [] -> map snd instances
        flags -> flags

    -- The flags of the instances in setup scopes. An instance that is also
    -- at the top level has the flags of the top-level constraint already,
    -- so a package whose instances in setup scopes are all at the top level
    -- needs no constraint here.
    setupFlags :: Map PackageName [(UserConstraintScope, FlagAssignment)]
    setupFlags =
      Map.filter (not . null) $
        Map.mapWithKey
          ( \p inSetup ->
              filter (not . nullFlagAssignment . snd) $
                (UserAnySetupQualifier p, inSetup)
                  : [ (UserQualified (UserQualSetup q) p, flags `diffFlagAssignment` inSetup)
                    | (q, flagsInScope, beyondTopLevel) <- setupScopeFlags
                    , p `Map.member` beyondTopLevel
                    , Just flags <- [Map.lookup p flagsInScope]
                    ]
          )
          (flagsIn setupUnits `Map.intersection` flagsIn (setupUnits `Set.difference` topLevel))
      where
        -- For each setup scope, the flags of its instances, and of those
        -- that are not at the top level.
        setupScopeFlags =
          [ (q, flagsIn units, flagsIn (units `Set.difference` topLevel))
          | (q, units) <- Map.toList setupScopes
          ]

    -- The flags that the source instances of each package among some units
    -- of the plan agree on.
    flagsIn :: Set UnitId -> Map PackageName FlagAssignment
    flagsIn units =
      Map.map agreedFlags $
        Map.fromListWith
          (++)
          [ (packageName elab, [elabFlagAssignment elab])
          | InstallPlan.Configured elab <- InstallPlan.toList plan
          , elabUnitId elab `Set.member` units
          ]

    -- The instances at the top level are those that the roots of the plan
    -- reach through library dependencies alone, leaving out those that are
    -- only reached through a setup or a build tool dependency. The roots are
    -- the local packages and anything else that nothing depends on.
    topLevel :: Set UnitId
    topLevel =
      closure
        [ nodeKey pkg
        | pkg <- InstallPlan.toList plan
        , isLocal pkg || null (InstallPlan.revDirectDeps plan (nodeKey pkg))
        ]
      where
        isLocal (InstallPlan.Configured elab) = elabLocalToProject elab
        isLocal _ = False

    -- The instances in the setup scope of each package that has setup
    -- dependencies are those that its setup dependencies reach through
    -- library dependencies.
    setupScopes :: Map PackageName (Set UnitId)
    setupScopes =
      Map.fromListWith
        Set.union
        [ (packageName elab, closure (map unitOf (elabSetupDependencies elab)))
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , not (null (elabSetupDependencies elab))
        ]

    setupUnits :: Set UnitId
    setupUnits = Set.unions (Map.elems setupScopes)

    unitOf :: (ConfiguredId, a) -> UnitId
    unitOf = newSimpleUnitId . confInstId . fst

    -- The units that some units reach through library dependencies.
    closure :: [UnitId] -> Set UnitId
    closure = go Set.empty
      where
        go seen [] = seen
        go seen (uid : uids)
          | uid `Set.member` seen = go seen uids
          | otherwise = go (Set.insert uid seen) (libraryDeps uid ++ uids)

        libraryDeps uid = case InstallPlan.lookup plan uid of
          Just (InstallPlan.PreExisting ipkg) -> nodeNeighbors ipkg
          Just (InstallPlan.Configured elab) -> map unitOf (elabLibDependencies elab)
          Just (InstallPlan.Installed elab) -> map unitOf (elabLibDependencies elab)
          Nothing -> []

    -- The flags that are given one value only.
    agreedFlags :: [FlagAssignment] -> FlagAssignment
    agreedFlags assignments =
      mkFlagAssignment
        [ (flag, value)
        | (flag, values) <- Map.toList flagValues
        , [value] <- [Set.toList values]
        ]
      where
        flagValues =
          Map.fromListWith
            Set.union
            [ (flag, Set.singleton value)
            | assignment <- assignments
            , (flag, value) <- unFlagAssignment assignment
            ]

    -- As described above, remove the version constraints on local packages,
    -- but leave any flag constraints.
    deleteLocalPackagesVersionConstraints
      :: Map PackageName [(UserConstraint, ConstraintSource)]
      -> Map PackageName [(UserConstraint, ConstraintSource)]
    deleteLocalPackagesVersionConstraints =
      Map.mergeWithKey
        ( \_pkgname () constraints ->
            case filter (not . isVersionConstraint . fst) constraints of
              [] -> Nothing
              constraints' -> Just constraints'
        )
        (const Map.empty)
        id
        localPackages

    isVersionConstraint (UserConstraint _ (PackagePropertyVersion _)) = True
    isVersionConstraint _ = False

    localPackages :: Map PackageName ()
    localPackages =
      Map.fromList
        [ (packageName elab, ())
        | InstallPlan.Configured elab <- InstallPlan.toList plan
        , elabLocalToProject elab
        ]
