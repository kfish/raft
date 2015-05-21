{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where

class Protocol p where

    type Request p :: *

    type Response p :: *

    type Effects p :: * -> *

    step :: p -> Request p -> Effects p (p, Maybe (Response p))
