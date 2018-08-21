import Control.Monad
import System.Directory

main' :: IO ()
main' = do
  putStr "Substring: "
  sbstr <- getLine
  cntnt <- getDirectoryContents "."
  mapM_ putStr cntnt
    
    --if name /= "" then (putStrLn $ "Hi, " ++ name ++ "!") else main'

