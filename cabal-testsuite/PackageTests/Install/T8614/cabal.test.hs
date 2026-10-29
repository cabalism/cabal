import Test.Cabal.Prelude
import System.Directory (removePathForcibly)

-- Installing one executable of a package that has several must install only
-- that one, both for a package of the project (#8614) and for a package from a
-- repository named as pkg:exename or pkg:exe:exename outside of a project
-- (#9817). A target that is none of the accepted forms is reported as unknown
-- rather than being passed to the download tool as a URI.
main = cabalTest $ do
  env <- getTestEnv
  let installdir = testPrefixDir env </> "bin"
      opts = ["--installdir=" ++ installdir, "--overwrite-policy=always", "--install-method=copy"]
      exe name = installdir </> name <.> exeExt
      -- Install the targets into an empty installdir, then check which
      -- executables arrived.
      installs targets present absent = do
        liftIO $ removePathForcibly installdir
        cabal "install" (targets ++ opts)
        mapM_ (shouldExist . exe) present
        mapM_ (shouldNotExist . exe) absent
  recordMode DoNotRecord $ do
    -- Project package.
    installs ["example1"] ["example1"] ["example2"]
    installs ["exe:example2"] ["example2"] ["example1"]
    installs ["t8614:exe:example1"] ["example1"] ["example2"]
    installs ["t8614"] ["example1", "example2"] []
    installs ["all:exes"] ["example1", "example2"] []
    -- A target selecting no component of the kind being installed is
    -- rejected. It used to install the package's executables regardless.
    fails (cabal' "install" ("t8614:libs" : opts))
      >>= assertOutputContains "does not select any executable"
    fails (cabal' "install" ("--lib" : "example1" : ("--package-env=" ++ testWorkDir env </> "env") : opts))
      >>= assertOutputContains "does not select any library"
    -- Except for the main library, which a bare package name selects too.
    installs ["lib:t8614"] ["example1", "example2"] []
    -- Repository package, outside of a project.
    withRepo "repo" $ do
      installs ["--ignore-project", "two-exes:first"] ["first"] ["second"]
      installs ["--ignore-project", "two-exes:exe:second"] ["second"] ["first"]
      installs ["--ignore-project", "two-exes-0.1:exe:first"] ["first"] ["second"]
      installs ["--ignore-project", "two-exes"] ["first", "second"] []
      fails (cabal' "install" ("--ignore-project" : "two-exes:exe:*" : opts))
        >>= assertOutputContains "Unknown target \"two-exes:exe:*\""
  where
    exeExt = if isWindows then "exe" else ""
