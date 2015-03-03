{-# LANGUAGE TypeFamilies #-}

module Consensus.Dummy where

import qualified Consensus.Types as Consensus

data Dummy = Dummy Int

instance Consensus.Protocol Dummy where
    data Request Dummy = QueryDummy | SetDummy Int
    data Response Dummy = DummyIs Int

    step (Dummy n) (QueryDummy) = (Dummy n, DummyIs n)
    step (Dummy _) (SetDummy n) = (Dummy n, DummyIs n)

