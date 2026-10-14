{-# LANGUAGE OverloadedStrings #-}
import Distribution.Fields.Parser
main :: IO ()
main = do
  let ok = either (const "ERROR") (const "OK") :: Either a b -> String
  putStrLn $ "brace field, comment before {: " ++ ok (readFields "foo:\n-- c\n{\n bar\n}\n")
  putStrLn $ "braces section, comment before field's {: " ++ ok (readFields "x { foo:\n -- c\n { bar } }\n")
  putStrLn $ "braces section, comment after section name: " ++ ok (readFields "x { library -- c\n {\n  foo: bar\n }\n}\n")
