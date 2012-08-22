import Network
import System.IO
import System.Environment

main = do
    files <- getArgs
    str <- mapM readFile files
    putStrLn "eine Datei"
    handle <- connectTo "localhost" $ PortNumber 1337
    hPutStr handle $ concat str
    hClose handle
