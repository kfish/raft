{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Consensus.Types (
      Identifier
    , Index
    , Term(..)

    , Store(..)

    , LogStoreF(..)
    , query'
    , store'
    , commit'
    , truncate'
    , end'
) where

import Control.Applicative ((<$>))
import Control.Monad.Free
import Data.Foldable (Foldable)
import qualified Data.Foldable as Fold
import Data.Serialize

import Network.Protocol

----------------------------------------------------------------------

type Identifier = Int

type Index = Int

----------------------------------------------------------------------
-- Term

newtype Term = Term Int
    deriving (Show, Eq, Ord)

instance Serialize Term where
    put (Term x) = put x
    get = Term <$> get

class HasTerm a where
    termOf :: a -> Term

----------------------------------------------------------------------

{-
class Node a where 

    identify :: a -> Identifier

    receive :: Monad m => a -> m (a, Request b)

    send :: Monad m => Response b -> a -> m a
    -}

{-
class Store s where

    type Value s :: *

    -- | Query the value at a given index
    query :: Monad m => Index -> s -> m (Maybe (Value s, Term))

    -- | Store a value at a given index
    store :: (Foldable t, Monad m) =>
        Int -> t (Value s) -> Term -> s -> m s

    commit :: Monad m => Index -> s -> m s

    -- | Delete a given entry and all that follow it
    truncate :: Monad m => Index -> s -> m s
-}


data LogStoreF t entry next
    = LogQuery Index (Maybe (entry, Term) -> next)
    | LogStore Index Term (t entry) next
    | LogCommit Index next
    | LogTruncate Index next
    | LogEnd

instance Functor (LogStoreF t entry) where
    fmap f (LogQuery ix cont)              = LogQuery ix (f . cont)
    fmap f (LogStore ix term entries next) = LogStore ix term entries (f next)
    fmap f (LogCommit ix next)             = LogCommit ix (f next)
    fmap f (LogTruncate ix next)           = LogTruncate ix (f next)
    fmap f LogEnd                          = LogEnd

query' :: MonadFree (LogStoreF t entry) m => Index -> m (Maybe (entry, Term))
query' ix = liftF (LogQuery ix id)

store' :: MonadFree (LogStoreF t entry) m => Index -> Term -> t entry -> m ()
store' ix term entries = liftF (LogStore ix term entries ())

commit' :: MonadFree (LogStoreF t entry) m => Index -> m ()
commit' ix = liftF (LogCommit ix ())

truncate' :: MonadFree (LogStoreF t entry) m => Index -> m ()
truncate' ix = liftF (LogTruncate ix ())

end' :: MonadFree (LogStoreF t entry) m => m ()
end' = liftF LogEnd

class Store s where
    type Value s :: *

    -- runLogStore :: (Fold.Foldable t)
    --             => Free (LogStoreF t (Value s)) () -> s -> s
    runLogStore :: Free (LogStoreF [] (Value s)) () -> s -> s
    valueAt :: Index -> s -> Maybe (Value s, Term)
