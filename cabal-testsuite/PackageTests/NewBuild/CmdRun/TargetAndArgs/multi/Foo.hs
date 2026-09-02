import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("foo args: " ++ show args)
