{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where

{-
class Protocol p where

    type Request p :: *

    type Response p :: *

    step :: Monad m => p -> Request p -> m (p, Maybe (Response p))
-}

class Monad m => Protocol p m where

    type Request p :: *

    type Response p :: *

    step :: p -> Request p -> m (p, Maybe (Response p))
