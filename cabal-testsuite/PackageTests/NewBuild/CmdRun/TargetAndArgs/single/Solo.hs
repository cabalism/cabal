import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("solo args: " ++ show args)
