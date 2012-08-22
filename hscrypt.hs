import System.IO
import Data.Bits(xor)

inputfile = "inputfile"
outputfile = "outputfile"
keyfile = "keyfile"

encrypt :: String -> String -> String -> String
encryot [] _ _ = [] 
encrypt input [] origkey = encrypt input origkey origkey
encrypt (s:input) (k:key) origkey = 
  toEnum (xor (fromEnum s) (fromEnum k)) : encrypt input key origkey

main = do
  key <- readFile keyfile
  input <- readFile inputfile
  filehandle <- openFile outputfile WriteMode
  hPutStr filehandle $ encrypt input key key
  hClose filehandle
