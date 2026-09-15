module UnitTests.Distribution.Client.Targets
  ( tests
  ) where

import Distribution.Client.HashValue (parseHashValue)
import Distribution.Client.Targets
  ( UserConstraint (..)
  , UserConstraintScope (..)
  , UserQualifier (..)
  , readUserConstraint
  )
import Distribution.Client.Types.PackageRevision (RevisionPin (..))
import Distribution.Package (mkPackageName)
import Distribution.PackageDescription (mkFlagAssignment, mkFlagName)
import Distribution.Version (anyVersion, mkVersion, thisVersion)

import Distribution.Parsec (explicitEitherParsec, parsec, parsecCommaList)

import Distribution.Solver.Types.OptionalStanza (OptionalStanza (..))
import Distribution.Solver.Types.PackageConstraint (PackageProperty (..))

import Test.Tasty
import Test.Tasty.HUnit

import Data.Either (isLeft)
import Data.List (intercalate)
import Data.Maybe (fromJust)

-- Helper function: makes a test group by mapping each element
-- of a list to a test case.
makeGroup :: String -> (a -> Assertion) -> [a] -> TestTree
makeGroup name f xs =
  testGroup name $
    zipWith testCase (map show [0 :: Integer ..]) (map f xs)

tests :: [TestTree]
tests =
  [ makeGroup
      "readUserConstraint"
      (uncurry readUserConstraintTest)
      exampleConstraints
  , makeGroup
      "parseUserConstraint"
      (uncurry parseUserConstraintTest)
      exampleConstraints
  , makeGroup
      "rejectUserConstraint"
      rejectUserConstraintTest
      [ "foo >=1.2@rev:2" -- not an exact version
      , "foo ==1.2.*@rev:2" -- not an exact version either
      , "setup.foo ==1.2.3@rev:2" -- a revision pin cannot be scoped
      , "bar:setup.foo ==1.2.3@rev:2"
      , "foo ==1.2.3@"
      , "foo ==1.2.3@rev:x"
      , "foo installed@rev:2"
      ]
  , makeGroup
      "readUserConstraints"
      (uncurry readUserConstraintsTest)
      [ -- First example only.

        ( case exampleStrs of (e : _) -> e; _ -> error "empty examples"
        , take 1 exampleUcs
        )
      , -- All examples separated by commas.
        (intercalate ", " exampleStrs, exampleUcs)
      ]
  ]
  where
    (exampleStrs, exampleUcs) = unzip exampleConstraints

exampleConstraints :: [(String, UserConstraint)]
exampleConstraints =
  [
    ( "template-haskell installed"
    , UserConstraint
        (UserQualified UserQualToplevel (pn "template-haskell"))
        PackagePropertyInstalled
    )
  ,
    ( "bytestring >= 0"
    , UserConstraint
        (UserQualified UserQualToplevel (pn "bytestring"))
        (PackagePropertyVersion anyVersion)
    )
  ,
    ( "any.directory test"
    , UserConstraint
        (UserAnyQualifier (pn "directory"))
        (PackagePropertyStanzas [TestStanzas])
    )
  ,
    ( "setup.Cabal installed"
    , UserConstraint
        (UserAnySetupQualifier (pn "Cabal"))
        PackagePropertyInstalled
    )
  ,
    ( "process:setup.bytestring ==5.2"
    , UserConstraint
        (UserQualified (UserQualSetup (pn "process")) (pn "bytestring"))
        (PackagePropertyVersion (thisVersion (mkVersion [5, 2])))
    )
  , -- flag MUST be prefixed with - or +

    ( "network:setup.containers +foo -bar +baz"
    , UserConstraint
        (UserQualified (UserQualSetup (pn "network")) (pn "containers"))
        ( PackagePropertyFlags
            ( mkFlagAssignment
                [ (fn "foo", True)
                , (fn "bar", False)
                , (fn "baz", True)
                ]
            )
        )
    )
  , -- an exact version with a revision pin

    ( "foo ==1.2.3@rev:2"
    , UserConstraintRevision
        (UserQualified UserQualToplevel (pn "foo"))
        (mkVersion [1, 2, 3])
        (RevisionNumber 2)
    )
  ,
    ( "any.foo ==1.2.3@sha256:69977f97a8db2c11e97bde92fff7e86e793c1fb23827b284bf89938ee463fbf0"
    , UserConstraintRevision
        (UserAnyQualifier (pn "foo"))
        (mkVersion [1, 2, 3])
        (RevisionHash (fromJust (parseHashValue "69977f97a8db2c11e97bde92fff7e86e793c1fb23827b284bf89938ee463fbf0")))
    )
    -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
    --
    -- , ("foo:happy:exe.template-haskell test",
    --    UserConstraint (UserQualified (UserQualExe (pn "foo") (pn "happy")) (pn "template-haskell"))
    --                   (PackagePropertyStanzas [TestStanzas]))
  ]
  where
    pn = mkPackageName
    fn = mkFlagName

readUserConstraintTest :: String -> UserConstraint -> Assertion
readUserConstraintTest str uc =
  assertEqual ("Couldn't read constraint: '" ++ str ++ "'") expected actual
  where
    expected = Right uc
    actual = readUserConstraint str

parseUserConstraintTest :: String -> UserConstraint -> Assertion
parseUserConstraintTest str uc =
  assertEqual ("Couldn't parse constraint: '" ++ str ++ "'") expected actual
  where
    expected = Right uc
    actual = explicitEitherParsec parsec str

readUserConstraintsTest :: String -> [UserConstraint] -> Assertion
readUserConstraintsTest str ucs =
  assertEqual ("Couldn't read constraints: '" ++ str ++ "'") expected actual
  where
    expected = Right ucs
    actual = explicitEitherParsec (parsecCommaList parsec) str

rejectUserConstraintTest :: String -> Assertion
rejectUserConstraintTest str =
  assertBool ("parsed: " ++ str) (isLeft (readUserConstraint str))
