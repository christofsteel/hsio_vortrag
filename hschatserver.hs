import Control.Concurrent
import System.IO
import Data.IORef
import Network

chatWith handle handlesref = do
  line <- hGetLine handle
  handles <- readIORef handlesref
  mapM (\h -> hPutStrLn h line >> hFlush h) handles
  chatWith handle handlesref

main = do
  socket <- listenOn $ PortNumber 1337
  handlesref <- newIORef []
  loop $ do
    (handle,_,_) <- accept socket
    handles <- readIORef handlesref
    writeIORef handlesref (handle:handles)
    forkIO $ chatWith handle handlesref
  where loop f = f >> loop f
