import Network
import Control.Concurrent
import System.IO

readAndSend h = do
  line <- getLine
  hPutStrLn h line
  hFlush h

recvAndPrint h = do
  line <- hGetLine h
  putStrLn line

main = do
  handle <- connectTo "localhost" $ PortNumber 1337
  forkIO $ loop $ readAndSend handle
  loop $ recvAndPrint handle
  where loop f = f >> loop f
