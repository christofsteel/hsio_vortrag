import Control.Concurrent

forkedThread :: Int -> IO ()
forkedThread i = (putStrLn . show) i >>= \_ -> forkedThread $ i+1

main = do
  forkIO $ forkedThread 0
  loop $ putStrLn "Ich bin der Master"
  where loop f = f >> loop f
