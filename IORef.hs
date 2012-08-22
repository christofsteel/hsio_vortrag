import Control.Concurrent
import Data.IORef
import Data.Char

printAllTheTime :: IORef String -> IO ()
printAllTheTime ref = do
  var <- readIORef ref
  putStrLn var
  getLine
  printAllTheTime ref

main = do
  ref <- newIORef "Foo"
  forkIO $ modifyIORef ref (map toUpper)
  printAllTheTime ref
  
