module DetailedTests (tests) where

import Distribution.TestSuite

tests :: IO [Test]
tests =
  return
    [ Test (check "reverse" (reverse "abc" == "cba"))
    , testGroup
        "arithmetic"
        [ Test (check "sum" (sum [1, 2, 3] == (6 :: Int)))
        , Test (check "product" (product [1, 2, 3] == (6 :: Int)))
        ]
    , Group
        { groupName = "one at a time"
        , concurrently = False
        , groupTests = [Test (triangular 100)]
        }
    ]

-- | A test that passes when the condition holds.
check :: String -> Bool -> TestInstance
check testName ok = test
  where
    test =
      TestInstance
        { run = return (Finished result)
        , name = testName
        , tags = []
        , options = []
        , setOption = \_ _ -> Right test
        }
    result
      | ok = Pass
      | otherwise = Fail (testName ++ " does not hold")

-- | A test with an option, that reports progress before it finishes. It
-- checks the formula for the sum of the numbers up to a limit.
triangular :: Int -> TestInstance
triangular limit =
  TestInstance
    { run = return (Progress "summing" (return (Finished result)))
    , name = "triangular"
    , tags = ["slow"]
    , options = [limitOption]
    , setOption = \option value ->
        if option /= optionName limitOption
          then Left ("unknown option: " ++ option)
          else case reads value of
            [(n, "")] -> Right (triangular n)
            _ -> Left ("not a number: " ++ value)
    }
  where
    result
      | sum [1 .. limit] == limit * (limit + 1) `div` 2 = Pass
      | otherwise = Fail ("the formula is wrong for " ++ show limit)

limitOption :: OptionDescr
limitOption =
  OptionDescr
    { optionName = "limit"
    , optionDescription = "The number to sum up to"
    , optionType =
        OptionNumber
          { optionNumberIsInt = True
          , optionNumberBounds = (Just "0", Nothing)
          }
    , optionDefault = Just "100"
    }
