module Distribution.Simple.GHC.BuildOptions
    ( mkLinkerOpts
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.PackageDescription as PD
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Utils.NubList

mkLinkerOpts :: LocalBuildInfo -> BuildInfo -> [FilePath] -> [FilePath] -> GhcOptions
mkLinkerOpts lbi bi objs libs =
    mempty
        { ghcOptLinkOptions =
            PD.ldOptions bi
            ++ [ "-static"
                | withFullyStaticExe lbi
                ]
            -- Pass extra `ld-options` given
            -- through to GHC's linker.
            ++ maybe
                []
                programOverrideArgs
                (lookupProgram ldProgram (withPrograms lbi))
        , ghcOptLinkLibs =
            if withFullyStaticExe lbi
            then extraLibsStatic bi
            else extraLibs bi
        , ghcOptLinkLibPath = toNubListR libs
        , ghcOptLinkFrameworks = toNubListR $ PD.frameworks bi
        , ghcOptLinkFrameworkDirs =
            toNubListR $
            PD.extraFrameworkDirs bi
        , ghcOptInputFiles = toNubListR objs
        }