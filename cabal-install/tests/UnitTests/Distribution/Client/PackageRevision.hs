module UnitTests.Distribution.Client.PackageRevision
  ( tests
  ) where

import Distribution.Client.HashValue (parseHashValue)
import Distribution.Client.Types.PackageRevision
  ( PackageRevision (..)
  , RevisionPin (..)
  , packageDescriptionRevision
  , packageRevisionsMap
  )
import Distribution.Package (PackageIdentifier (..), mkPackageName)
import Distribution.PackageDescription (customFieldsPD, emptyPackageDescription)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Version (mkVersion)

import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup
      "parse"
      [ testCase str $ eitherParsec str @?= Right rev
      | (str, rev) <- examples
      ]
  , testGroup
      "pretty"
      [ testCase str $ prettyShow rev @?= str
      | (str, rev) <- examples
      ]
  , testGroup
      "reject"
      [ testCase str $ assertBool "parsed" (isLeft (eitherParsec str :: Either String PackageRevision))
      | str <-
          [ "foo-1.2.3"
          , "foo-1.2.3@"
          , "foo-1.2.3@rev:"
          , "foo-1.2.3@rev:-1"
          , "foo-1.2.3@2"
          , "foo-1.2.3@sha256:"
          , "foo-1.2.3@sha256:abc" -- odd number of hex digits
          , "foo-1.2.3@sha256:zz"
          , "foo@rev:1"
          ]
      ]
  , testGroup
      "packageRevisionsMap"
      [ testCase "identical pins are merged" $
          packageRevisionsMap [fooRev2, fooRev2] @?= Right (Map.fromList [(foo, RevisionNumber 2)])
      , testCase "conflicting pins are rejected" $
          packageRevisionsMap [fooRev2, PackageRevision foo (RevisionNumber 3)]
            @?= Left (foo, RevisionNumber 2, RevisionNumber 3)
      ]
  , testGroup
      "packageDescriptionRevision"
      [ testCase "no x-revision" $ packageDescriptionRevision emptyPackageDescription @?= 0
      , testCase "x-revision" $
          packageDescriptionRevision emptyPackageDescription{customFieldsPD = [("x-revision", "3")]} @?= 3
      ]
  ]
  where
    foo = PackageIdentifier (mkPackageName "foo") (mkVersion [1, 2, 3])
    fooRev2 = PackageRevision foo (RevisionNumber 2)
    hash = fromJust (parseHashValue "69977f97a8db2c11e97bde92fff7e86e793c1fb23827b284bf89938ee463fbf0")

    examples :: [(String, PackageRevision)]
    examples =
      [ ("foo-1.2.3@rev:2", fooRev2)
      , ("foo-1.2.3@rev:0", PackageRevision foo (RevisionNumber 0))
      , ("foo-1.2.3@sha256:69977f97a8db2c11e97bde92fff7e86e793c1fb23827b284bf89938ee463fbf0", PackageRevision foo (RevisionHash hash))
      ]
