{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Free
import Control.Monad.State as State
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import qualified Data.Map as Map
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

data TestProtocol s = TestProtocol
    { ts :: s
    }

instance (Store s) => Protocol (TestProtocol s) where

    -- Look, really you want a test protocol that just stores strings against indexes.
    -- First step: parse/send the key as an Index (ie. int) not a string key

    type Request (TestProtocol s) = ClientCommand Index (Value s)
    type Response (TestProtocol s) = ClientResponse Index (Value s)
    type Effects (TestProtocol s) = Free (LogStoreF [] (Value s)) ()

    step tp cmd = case cmd of
          CmdSet k v ->
              let s' = runLogStore (store' k (Term 0) [v] >> end') (ts tp)
              in (tp{ts=s'}, Free LogEnd, Just $ RspSetOK k v)

          CmdGet k ->
              let res = valueAt k (ts tp)
                  rsp = case res of
                            Just (v, _) -> RspGetOK k v
                            Nothing -> RspGetFail k
              in (tp, Free LogEnd, Just $ rsp)

          CmdSleep n -> (tp, Free LogEnd, Nothing)

----------------------------------------------------------------------

main :: IO ()
main = do
  let store = TestProtocol TS.empty
  serveOn (PortNumber 44444) store

