import Network
import System.IO

main = withSocketsDo $ do
  socket <- listenOn $ PortNumber 1337
  (handle,_,_) <- accept socket
  repeatIfNotEOF handle $ do
        line <- hGetLine handle
        putStrLn line
  where
    repeatIfNotEOF handle f = do
      isEOF <- hIsEOF handle
      if isEOF
        then return ()
        else f >> repeatIfNotEOF handle f
      
