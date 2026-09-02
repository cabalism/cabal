import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("only-bench args: " ++ show args)
