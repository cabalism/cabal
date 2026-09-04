-- | Tests for how a command tells target strings from the arguments they are
-- mixed with.
--
-- Everything here runs the split against a pure oracle recognising a fixed set
-- of names, so no project, filesystem or install plan is involved.
module UnitTests.Distribution.Client.TargetArgs (tests) where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust, isNothing)

import Distribution.Client.TargetArgs
  ( ArgKind (..)
  , ClassifiedArg (..)
  , TargetAndArgs (..)
  , classifyArgs
  , knownTargetOracle
  , separatorPosition
  , splitTargetAndArgs
  )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "classifyArgs" $
      zipWith
        (\n c -> testCase (show n) c)
        [0 :: Int ..]
        classifyCases
  , testGroup "splitTargetAndArgs" $
      map (uncurry testCase) splitCases
  , testGroup
      "properties"
      [ testProperty "classification is total and order preserving" prop_classifyTotal
      , testProperty "flags are never targets" prop_flagsNeverTargets
      , testProperty "classification ignores the separator" prop_classifyIgnoresSeparator
      , testProperty "nothing is lost or reordered" prop_noLoss
      , testProperty "an explicit separator reproduces the split" prop_idempotent
      , testProperty "with a separator every target resolved" prop_targetsResolve
      , testProperty "the separator bounds the targets" prop_boundaryRespected
      , testProperty "without a separator at least one target" prop_minimumOne
      , testProperty "appending arguments leaves targets alone" prop_argumentStability
      ]
  ]

-------------------------------------------------------------------------------
-- Running the split
-------------------------------------------------------------------------------

-- | The split, projected to just the two lists.
split
  :: [String]
  -- ^ Names the oracle recognises.
  -> [String]
  -- ^ Full command line.
  -> [String]
  -- ^ Targets mixed with arguments.
  -> ([String], [String])
split known fullArgs targetAndArgs =
  let r = splitFull known fullArgs targetAndArgs
   in (taTargets r, taArgs r)

splitFull :: [String] -> [String] -> [String] -> TargetAndArgs
splitFull known fullArgs targetAndArgs =
  runIdentity $ splitTargetAndArgs (knownTargetOracle known) fullArgs targetAndArgs

classifyWith :: [String] -> [String] -> [String] -> [ClassifiedArg]
classifyWith known fullArgs targetAndArgs =
  runIdentity $ classifyArgs (knownTargetOracle known) fullArgs targetAndArgs

-- | A compact stand-in for 'ArgKind' so that expected values in tables stay
-- readable; the payload of 'ArgTarget' is not what these tests are about.
data KindTag = F | T | P
  deriving (Eq, Show)

tag :: ClassifiedArg -> KindTag
tag ca = case caKind ca of
  ArgFlag -> F
  ArgTarget{} -> T
  ArgPlain -> P

tags :: [String] -> [String] -> [String] -> [KindTag]
tags known fullArgs = map tag . classifyWith known fullArgs

-------------------------------------------------------------------------------
-- Classification
-------------------------------------------------------------------------------

classifyCases :: [Assertion]
classifyCases =
  [ -- A flag is never probed, even when it collides with a name the oracle
    -- would recognise.
    tags ["--randomize"] ["run", "--randomize"] ["--randomize"] @?= [F]
  , tags ["foo"] ["run", "foo", "-v2", "bar"] ["foo", "-v2", "bar"] @?= [T, F, P]
  , -- '+RTS' is treated as a flag rather than probed.
    tags [] ["run", "--", "+RTS"] ["+RTS"] @?= [F]
  , -- Position relative to the separator is recorded, not used.
    map caBeforeSep (classifyWith ["foo"] ["run", "foo", "--", "bar"] ["foo", "bar"])
      @?= [True, False]
  , map caBeforeSep (classifyWith ["foo"] ["run", "--", "foo", "bar"] ["foo", "bar"])
      @?= [False, False]
  , -- With no separator everything counts as preceding it.
    map caBeforeSep (classifyWith ["foo"] ["run", "foo", "bar"] ["foo", "bar"])
      @?= [True, True]
  , separatorPosition ["run", "foo", "--", "bar"] ["foo", "bar"] @?= Just 1
  , separatorPosition ["run", "--", "foo", "bar"] ["foo", "bar"] @?= Just 0
  , separatorPosition ["run", "foo", "bar"] ["foo", "bar"] @?= Nothing
  ]

-------------------------------------------------------------------------------
-- The split
-------------------------------------------------------------------------------

splitCases :: [(String, Assertion)]
splitCases =
  [
    ( "target with no arguments"
    , split ["target"] ["exe", "cmd", "target"] ["target"] @?= (["target"], [])
    )
  ,
    ( "+RTS before the separator goes to cabal"
    , split ["target"] ["exe", "cmd", "target", "+RTS"] ["target"] @?= (["target"], [])
    )
  ,
    ( "separator after +RTS"
    , split ["target"] ["exe", "cmd", "target", "+RTS", "--"] ["target"] @?= (["target"], [])
    )
  ,
    ( "separator before +RTS"
    , split ["target"] ["exe", "cmd", "target", "--", "+RTS"] ["target", "+RTS"]
        @?= (["target"], ["+RTS"])
    )
  ,
    ( "nothing at all"
    , split [] ["exe", "cmd"] [] @?= ([], [])
    )
  ,
    ( "no target, +RTS to cabal"
    , split [] ["exe", "cmd", "+RTS"] [] @?= ([], [])
    )
  ,
    ( "no target, separator after +RTS"
    , split [] ["exe", "cmd", "+RTS", "--"] [] @?= ([], [])
    )
  ,
    ( "no target, +RTS to the executable"
    , split [] ["exe", "cmd", "--", "+RTS"] ["+RTS"] @?= ([], ["+RTS"])
    )
  ,
    ( "target repeated on the argument side"
    , split
        ["cabal-install:parser-tests"]
        ["-v2", "repl", "--dry-run", "cabal-install:parser-tests", "--", "--dry-run", "cabal-install:parser-tests", "--dry-run"]
        ["cabal-install:parser-tests", "--dry-run", "cabal-install:parser-tests", "--dry-run"]
        @?= (["cabal-install:parser-tests"], ["--dry-run", "cabal-install:parser-tests", "--dry-run"])
    )
  , -- https://github.com/haskell/cabal/issues/12231

    ( "target given only after the separator"
    , split
        ["saturn-test-suite"]
        ["run", "--", "saturn-test-suite", "--randomize", "--strict"]
        ["saturn-test-suite", "--randomize", "--strict"]
        @?= (["saturn-test-suite"], ["--randomize", "--strict"])
    )
  ,
    ( "leading flag after the separator is not a target"
    , split ["foo"] ["run", "--", "--randomize"] ["--randomize"] @?= ([], ["--randomize"])
    )
  ,
    ( "unrecognised leading word with no separator is kept as a target"
    , split ["foo"] ["run", "bar"] ["bar"] @?= (["bar"], [])
    )
  , -- A flag claims nothing, so it is exempt from that rule even though there
    -- is no separator to say arguments follow.

    ( "leading flag with no separator is not a target"
    , split ["foo"] ["run", "-"] ["-"] @?= ([], ["-"])
    )
  ,
    ( "argument named like a target stays an argument"
    , split ["foo", "bar"] ["run", "foo", "--", "bar"] ["foo", "bar"] @?= (["foo"], ["bar"])
    )
  ,
    ( "the target itself passed as an argument"
    , split ["foo"] ["run", "foo", "--", "foo"] ["foo", "foo"] @?= (["foo"], ["foo"])
    )
  ,
    ( "target repeated explicitly"
    , split ["foo"] ["run", "foo", "foo"] ["foo", "foo"] @?= (["foo", "foo"], [])
    )
  ,
    ( "a target after a flag is not collected"
    , split ["foo", "bar"] ["run", "foo", "--verbose", "bar"] ["foo", "--verbose", "bar"]
        @?= (["foo"], ["--verbose", "bar"])
    )
  ,
    ( "separator as the last element"
    , split ["foo"] ["run", "foo", "--"] ["foo"] @?= (["foo"], [])
    )
  ,
    ( "two targets before the separator"
    , split ["a", "b"] ["run", "a", "b", "--", "c"] ["a", "b", "c"] @?= (["a", "b"], ["c"])
    )
  , -- Only targets and flags belong before the separator, so an unrecognised
    -- word there stays a target and fails downstream as one.

    ( "unrecognised word before the separator stays a target"
    , split ["foo"] ["run", "foo", "bar", "--", "x"] ["foo", "bar", "x"]
        @?= (["foo", "bar"], ["x"])
    )
  ]

-------------------------------------------------------------------------------
-- Generating command lines
-------------------------------------------------------------------------------

-- | A command line, together with where its separator falls and which names
-- the oracle recognises.
data CmdLine = CmdLine
  { clKnown :: [String]
  , clCombined :: [String]
  , clSep :: Maybe Int
  }
  deriving (Show)

-- | The full command line implied by 'clCombined' and 'clSep'.
cmdFullArgs :: CmdLine -> [String]
cmdFullArgs CmdLine{clCombined, clSep} = case clSep of
  Nothing -> "run" : clCombined
  Just n -> "run" : take n clCombined ++ "--" : drop n clCombined

cmdSplit :: CmdLine -> ([String], [String])
cmdSplit cl = split (clKnown cl) (cmdFullArgs cl) (clCombined cl)

cmdClassify :: CmdLine -> [ClassifiedArg]
cmdClassify cl = classifyWith (clKnown cl) (cmdFullArgs cl) (clCombined cl)

-- | Target-shaped words, so that shrinks stay readable.
targetWords :: [String]
targetWords = ["a", "b", "a:exe:a-exe"]

flagWords :: [String]
flagWords = ["-v2", "--randomize", "+RTS"]

plainWords :: [String]
plainWords = ["x", "y"]

instance Arbitrary CmdLine where
  arbitrary = do
    combined <- listOf (elements (targetWords ++ flagWords ++ plainWords))
    known <- sublistOf targetWords
    sep <- oneof [pure Nothing, Just <$> choose (0, length combined)]
    return CmdLine{clKnown = known, clCombined = combined, clSep = sep}

  shrink cl =
    [ cl{clCombined = combined', clSep = clampSep combined'}
    | combined' <- shrink (clCombined cl)
    ]
      ++ [cl{clKnown = known'} | known' <- shrink (clKnown cl)]
    where
      clampSep combined' = min (length combined') <$> clSep cl

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | Every element is classified, once, in order.
prop_classifyTotal :: CmdLine -> Property
prop_classifyTotal cl =
  map caString (cmdClassify cl) === clCombined cl

-- | A flag is never taken as a target, even when the oracle would recognise
-- the string, and even as the leading word that would otherwise be kept as a
-- target claim.
--
-- Excludes the region before an explicit separator, where every string is a
-- target string by the user's own placement. Flags cannot occur there in
-- practice: the option parser consumes cabal's own flags before @--@.
prop_flagsNeverTargets :: CmdLine -> Property
prop_flagsNeverTargets cl =
  beforeSeparatorEmpty ==>
    counterexample (show classified) $
      all (\ca -> tag ca /= F) (take (length targets) classified)
  where
    classified = cmdClassify cl
    (targets, _) = cmdSplit cl
    beforeSeparatorEmpty = clSep cl `elem` [Nothing, Just 0]

-- | Moving the separator changes where things sit, never what they are.
prop_classifyIgnoresSeparator :: CmdLine -> Property
prop_classifyIgnoresSeparator cl =
  conjoin
    [ map tag (cmdClassify cl{clSep = sep}) === reference
    | sep <- Nothing : map Just [0 .. length (clCombined cl)]
    ]
  where
    reference = map tag (cmdClassify cl{clSep = Nothing})

-- | The split partitions the input: nothing added, dropped or reordered.
prop_noLoss :: CmdLine -> Property
prop_noLoss cl = targets ++ args === clCombined cl
  where
    (targets, args) = cmdSplit cl

-- | Putting an explicit separator at the boundary the split found reproduces
-- that split.
--
-- Restricted to splits where every target resolved: the fallback that keeps an
-- unrecognised leading word as a target applies only when there is no
-- separator, so writing one in would legitimately change the answer.
prop_idempotent :: CmdLine -> Property
prop_idempotent cl =
  allResolved ==> cmdSplit cl{clSep = Just (length targets)} === (targets, args)
  where
    (targets, args) = cmdSplit cl
    allResolved = all ((== T) . tag) (take (length targets) (cmdClassify cl))

-- | With a separator and nothing before it, only strings that resolve are
-- taken as targets. (With something before it the user has placed the targets
-- explicitly, so they are taken as given and left to fail as targets.)
prop_targetsResolve :: CmdLine -> Property
prop_targetsResolve cl0 =
  counterexample (show cl) $
    all ((== T) . tag) (take (length targets) (cmdClassify cl))
  where
    -- Built rather than filtered for: a generated separator lands on this one
    -- position too rarely to keep enough cases.
    cl = cl0{clSep = Just 0}
    (targets, _) = cmdSplit cl

-- | When something precedes the separator, the targets are among those; the
-- boundary is never crossed.
prop_boundaryRespected :: CmdLine -> Property
prop_boundaryRespected cl = case clSep cl of
  Just n | n > 0 -> property (length targets <= n)
  _ -> property True
  where
    (targets, _) = cmdSplit cl

-- | Without a separator a leading word is a target claim, so it is always
-- taken as a target even when it does not resolve. A leading flag claims
-- nothing and is exempt.
prop_minimumOne :: CmdLine -> Property
prop_minimumOne cl =
  (isNothing (clSep cl) && leadingWord) ==>
    length targets >= 1
  where
    (targets, _) = cmdSplit cl
    leadingWord = case cmdClassify cl of
      ca : _ -> tag ca /= F
      [] -> False

-- | Arguments appended after the separator cannot change which targets were
-- found.
--
-- Restricted to command lines that already have a separator: introducing one
-- legitimately changes the answer, because it withdraws the target claim that
-- a leading word makes when no separator is present.
prop_argumentStability :: CmdLine -> [String] -> Property
prop_argumentStability cl extra =
  isJust (clSep cl) ==> fst (cmdSplit cl') === fst (cmdSplit cl)
  where
    cl' = cl{clCombined = clCombined cl ++ extra}
