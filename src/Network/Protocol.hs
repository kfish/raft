{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where

class Protocol p where

    type Request p :: *

    type Response p :: *

    step :: p -> Request p -> (p, Maybe (Response p))
