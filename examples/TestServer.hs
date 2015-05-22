{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Monad.Free
import Network

import ClientTypes

import Network.Protocol
import Network.Protocol.Server

import Consensus.Types
import qualified TestStore as TS

----------------------------------------------------------------------

data TestProtocol a = TestProtocol a

testProtocol :: TestProtocol a
testProtocol = TestProtocol undefined

instance Protocol (TestProtocol a) where

    -- Look, really you want a test protocol that just stores strings against indexes.
    -- First step: parse/send the key as an Index (ie. int) not a string key

    type Request (TestProtocol a) = ClientCommand Index a
    type Response (TestProtocol a) = ClientResponse Index a
    type Effects (TestProtocol a) = Free (LogStoreF [] a)

    step p cmd = case cmd of
          CmdSet k v -> do
              store' k (Term 0) [v]
              return (p, Just $ RspSetOK k v)

          CmdGet k -> do
              res <- query' k
              let rsp = case res of
                            Just (v, _) -> RspGetOK k v
                            Nothing -> RspGetFail k
              return (p, Just rsp)

          CmdSleep n -> return (p, Nothing)

----------------------------------------------------------------------

main :: IO ()
main = serveOn (PortNumber 44444) testProtocol TS.empty
