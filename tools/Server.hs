{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

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

import Network.Protocol
import Network.Protocol.Server

import Consensus.Types
import qualified TestStore as TS

----------------------------------------------------------------------

data TestProtocol = TestProtocol
    { ts :: TS.TestStore
    }

instance Protocol TestProtocol where

    type Request TestProtocol = Cmd BS.ByteString Int
    type Response TestProtocol = ClientResponse BS.ByteString Int

    step tp cmd = case cmd of
          CmdSet k v ->
              let s' = TS.testStore 0 [v] (Term 0) (ts tp) in
              (tp{ts=s'}, Just $ RspSetOK k v)
          CmdGet k -> let rsp = case TS.testQuery 0 (ts tp) of
                                    Just (v, _) -> RspGetOK k v
                                    Nothing -> RspGetFail k
                      in (tp, Just $ rsp)
          CmdSleep n -> (tp, Nothing)

----------------------------------------------------------------------

main :: IO ()
main = do
  let store = TestProtocol TS.empty
  serveOn (PortNumber 44444) store

