import Control.Monad
import System.Directory
import Data.List

main' :: IO ()
main' = do
  putStr "Substring: "
  sbstr <- getLine
  if sbstr /= "" then deleteF sbstr else putStr "Canceled" 

deleteF :: String -> IO ()
deleteF sbstr = do
  files <- getDirectoryContents "."
  ffiles <- filterM (return . isInfixOf sbstr) files
  mapM_ (\x -> putStrLn $ "Removing file: " ++ x) ffiles
  mapM_ removeFile ffiles
    --if name /= "" then (putStrLn $ "Hi, " ++ name ++ "!") else main'
  
