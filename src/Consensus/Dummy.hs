{-# LANGUAGE TypeFamilies #-}

module Consensus.Dummy where

import Network.Protocol

import qualified Consensus.Types as Consensus

data Dummy = Dummy Int

instance Protocol Dummy where
    data Request Dummy = QueryDummy | SetDummy Int
    data Response Dummy = DummyIs Int

    step (Dummy n) (QueryDummy) = (Dummy n, Just $ DummyIs n)
    step (Dummy _) (SetDummy n) = (Dummy n, Just $ DummyIs n)

