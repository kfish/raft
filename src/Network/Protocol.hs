{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where

class Protocol p where

    type Request p :: *

    type Response p :: *

    step :: Monad m => p -> Request p -> m (p, Maybe (Response p))
