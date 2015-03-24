{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import Network
import Network.Socket as S
import System.IO
import Text.Printf

import Network.Stream as Stream
import Network.Stream.Socket as Stream
import Network.Stream.Types as Stream
import ClientTypes

----------------------------------------------------------------------

serve :: S.Socket -> IO ()
serve h = do
    stream <- mkSocketStream h
    loop stream where
    loop stream = do
      cmd <- Stream.runGet stream getCmd
      case cmd of
          CmdSet k v -> liftIO . putStrLn $ "Set " ++ show k ++ " to " ++ show v
          CmdGet k -> liftIO . putStrLn $ "Get " ++ show k
          CmdSleep n -> do
              liftIO . putStrLn $ "Sleep " ++ show n
      loop stream

    getCmd :: Get (Cmd BS.ByteString Int)
    getCmd = get

main :: IO ()
main = do
  s <- listenOn (PortNumber 44444)
  forever $ do
    (h, addr) <- S.accept s
    forkIO (serve h `finally` S.sClose h)

