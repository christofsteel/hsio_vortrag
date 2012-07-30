Haskell IO Vortrag
==================

# Gliederung:
* Einführung
  * ghc / cabal install
  * hackage
  * main
* stdin/stdout
  * stdout: putStrLn/putStr/putChar/print
  * stdin: getChar/getLine/readLn/getContents
  * Hello World/Hello <user>
* File Handling
  * import System.IO
  * fileOpen/withFile
  * read: hGetChar/hGetLine/hReadLn/hGetContents/readFile
  * write: hPutStrLn/hPutStr/hPutChar/hPrint/writeFile
  * en/decrypter
* Netzwerk
  * import Network
  * Server
  * Client
  * simple File transfer (netcat like)
* Threading
  * import Control.Concurrent
  * forkIO
  * Mutable Variables
    * IORef
    * MVar
    * STM (nicht behandeln)
  * Multithreaded Chat server
* Coole Sachen
  * getArgs
  * cmdArgs
  * ByteStrings
  * Foreign Function Interface
