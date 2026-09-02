import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("only-test args: " ++ show args)
