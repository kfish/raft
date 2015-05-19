{-# LANGUAGE TypeFamilies #-}

module DummyProtocol where

import Network.Protocol

import qualified Consensus.Types as Consensus

data Dummy = Dummy Int

data DummyRequest = QueryDummy | SetDummy Int
data DummyResponse = DummyIs Int

instance Protocol Dummy where
    type Request Dummy = DummyRequest
    type Response Dummy = DummyResponse

    step (Dummy n) (QueryDummy) = return (Dummy n, Just $ DummyIs n)
    step (Dummy _) (SetDummy n) = return (Dummy n, Just $ DummyIs n)

