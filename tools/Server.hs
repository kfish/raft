
module Main where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Network
import System.IO
import Text.Printf

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    loop
  where
    loop = do
      line <- hGetLine h
      if line == "end"
        then hPutStrLn h ("Thank you for using the Haskell doubling service.")
        else do
          hPutStrLn h (show (2 * (read line :: Integer)))
          loop

main :: IO ()
main = do
  s <- listenOn (PortNumber 44444)
  forever $ do
    (h, host, _) <- accept s
    printf "new client: %s\n" host
    forkIO (talk h `finally` hClose h)


