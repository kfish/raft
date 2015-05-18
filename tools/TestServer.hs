{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
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

data TestProtocol = TestProtocol
    { ts :: TS.TestStore
    }

instance Protocol TestProtocol where

    -- Look, really you want a test protocol that just stores strings against indexes.
    -- First step: parse/send the key as an Index (ie. int) not a string key

    type Request TestProtocol = Cmd Index Int
    type Response TestProtocol = ClientResponse Index Int

    step tp cmd = case cmd of
          CmdSet k v -> do
              let s' = flip State.execState (ts tp) $ do
                         TS.runTestStore (store' 0 (Term 0) [v] >> end')
              return (tp{ts=s'}, Just $ RspSetOK k v)

          CmdGet k -> do
              let res = flip evalState (ts tp) $ do
                          TS.TestStore s _c <- State.get
                          return $ Map.lookup k s
              let rsp = case res of
                            Just (v, _) -> RspGetOK k v
                            Nothing -> RspGetFail k
              return (tp, Just $ rsp)

          CmdSleep n -> return (tp, Nothing)

----------------------------------------------------------------------

main :: IO ()
main = do
  let store = TestProtocol TS.empty
  serveOn (PortNumber 44444) store

