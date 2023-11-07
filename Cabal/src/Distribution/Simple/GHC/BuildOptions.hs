module Distribution.Simple.GHC.BuildOptions
    ( mkProfOpts
    , mkLinkerOpts
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.PackageDescription as PD
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (Flag, toFlag)
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Utils.NubList

mkProfOpts :: BuildInfo -> (Hpc.Way -> Flag FilePath) -> Flag GhcProfAuto -> GhcOptions -> GhcOptions
mkProfOpts bi hpcdir profOptAuto vanillaOpts =
    vanillaOpts
        `mappend` mempty
        { ghcOptProfilingMode = toFlag True
        , ghcOptProfilingAuto = profOptAuto
        , ghcOptHiSuffix = toFlag "p_hi"
        , ghcOptObjSuffix = toFlag "p_o"
        , ghcOptExtra = hcProfOptions GHC bi
        , ghcOptHPCDir = hpcdir Hpc.Prof
        }

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