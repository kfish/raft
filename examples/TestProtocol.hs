{-# LANGUAGE TypeFamilies #-}

module TestProtocol where

import Control.Monad.Free

import ClientTypes

import Network.Protocol

import Raft.Types

----------------------------------------------------------------------

data TestProtocol a = TestProtocol a

testProtocol :: TestProtocol a
testProtocol = TestProtocol undefined

instance Protocol (TestProtocol a) where

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
