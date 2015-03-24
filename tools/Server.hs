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
    loop stream
  where
    loop stream = do
      cmd <- Stream.runGet stream getCmd
      case cmd of
          CmdSet k v -> do
              liftIO . putStrLn $ "Set " ++ show k ++ " to " ++ show v
              Stream.runPut stream $ put (RspSetOK k v)
          CmdGet k -> do
              liftIO . putStrLn $ "Get " ++ show k
              Stream.runPut stream $ put (mkRspGetOK k 7)
          CmdSleep n -> do
              liftIO . putStrLn $ "Sleep " ++ show n
      loop stream

    getCmd :: Get (Cmd BS.ByteString Int)
    getCmd = get

    mkRspGetOK :: BS.ByteString -> Int -> ClientResponse BS.ByteString Int
    mkRspGetOK k v = RspGetOK k v

main :: IO ()
main = do
  s <- listenOn (PortNumber 44444)
  forever $ do
    (h, addr) <- S.accept s
    forkIO (serve h `finally` S.sClose h)

