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

import Consensus.Types
import qualified TestStore as TS

----------------------------------------------------------------------

serve :: S.Socket -> TS.TestStore -> IO ()
serve h store0 = do
    stream <- mkSocketStream h
    loop stream store0
  where
    loop stream s = do
      cmd <- Stream.runGet stream getCmd
      loop stream =<< case cmd of
          CmdSet k v -> do
              liftIO . putStrLn $ "Set " ++ show k ++ " to " ++ show v
              let s' = TS.testStore 0 [v] (Term 0) s
              Stream.runPut stream $ put (RspSetOK k v)
              return s'
          CmdGet k -> do
              liftIO . putStrLn $ "Get " ++ show k
              Stream.runPut stream . put $
                  case TS.testQuery 0 s of
                      Just (v, _) -> mkRspGetOK k v
                      Nothing -> RspGetFail k
              return s
          CmdSleep n -> do
              liftIO . putStrLn $ "Sleep " ++ show n
              return s

    getCmd :: Get (Cmd BS.ByteString Int)
    getCmd = get

    mkRspGetOK :: BS.ByteString -> Int -> ClientResponse BS.ByteString Int
    mkRspGetOK k v = RspGetOK k v

main :: IO ()
main = do
  s <- listenOn (PortNumber 44444)
  let store = TS.empty
  forever $ do
    (h, addr) <- S.accept s
    forkIO (serve h store `finally` S.sClose h)

