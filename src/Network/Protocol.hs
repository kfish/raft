{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol (
      Protocol(..)
) where


import Control.Monad.State

class Protocol p where

    type Request p :: *

    type Response p :: *

    type Effects p :: *

    -- step :: Monad m => p -> Request p -> m (p, Maybe (Response p))
    step :: p -> Request p -> (p, Effects p, Maybe (Response p))

{-
class (MonadIO m) => EvalIO s m where
    evalIO :: m a -> IO a

instance EvalIO s (StateT s IO) where
    evalIO s f = evalStateT (f s)
    -}
