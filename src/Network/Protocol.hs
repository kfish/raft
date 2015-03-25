{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where

class Protocol p where

    data Request p :: *

    data Response p :: *

    step :: p -> Request p -> (p, Maybe (Response p))
