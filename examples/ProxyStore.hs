{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ProxyStore (
    ProxyStore(..)
  , open
) where

import Control.Applicative ((<$>))

import Control.Monad.Free
import Control.Monad.State as State

import qualified Data.Foldable as Fold
import Data.Serialize as Serialize

import Network.Socket
import Network.Stream
import Network.Stream as Stream
import Network.Stream.Socket
import Network.Stream.Types

import qualified Raft.Protocol as CS
import qualified Raft.Types as CS

import ClientTypes

----------------------------------------------------------------------

data ProxyStore = ProxyStore {
      psOrigin :: Stream
    }

type ProxyStoreM = StateT ProxyStore IO

open :: Endpoint -> IO ProxyStore
open endpoint = do
  s <- connectSocket endpoint
  -- setSocketOption s NoDelay 1
  ProxyStore <$> mkSocketStream s

runProxyStore :: (Fold.Foldable t)
               => Free (CS.LogStoreF t Int) r -> ProxyStoreM r
runProxyStore (Pure r) = return r
runProxyStore (Free x) = case x of
    CS.LogQuery ix cont -> do
        ProxyStore origin <- State.get
        val <- liftIO $ do
          putStrLn ("Query value at index " ++ show ix)
          Stream.runPut origin $
            Serialize.put (CmdGet ix :: ClientCommand CS.Index Int)
          putStrLn ("Waiting on origin response ...")
          (rsp :: ClientResponse CS.Index (Int, CS.Term)) <- Stream.runGet origin Serialize.get
          -- (rsp :: ClientResponse CS.Index Int) <- Stream.runGet origin Serialize.get
          return $ case rsp of
            RspGetOK k v -> Just v
            RspSetOK k v -> error "Got RspSetOK, expected RspGetOK"
            _            -> Nothing
        liftIO $ putStrLn ("Got value " ++ show val)
        runProxyStore $ cont val
    CS.LogStore ix (CS.Term term) xs next -> do
        ProxyStore origin <- State.get
        liftIO $ putStrLn ("Store from " ++ show ix)
        liftIO $ mapM_ (\(ixx, x) ->
          Stream.runPut origin $
            Serialize.put (CmdSet ixx (x, CS.Term term) :: ClientCommand CS.Index (Int, CS.Term)))
          (zip [ix..] (Fold.toList xs))
        liftIO $ putStrLn ("Waiting on origin response ...")
        (_ :: ClientResponse CS.Index (Int, CS.Term)) <- liftIO $ Stream.runGet origin Serialize.get
        -- (_ :: Int) <- liftIO $ Stream.runGet origin Serialize.get
        liftIO $ putStrLn ("Got origin response")
        runProxyStore next
    CS.LogCommit ix next -> do
        -- ProxyStore conn <- get
        liftIO $ putStrLn ("Commit from " ++ show ix)
        runProxyStore next
    CS.LogTruncate ix next -> do
        -- ProxyStore conn <- get
        liftIO $ putStrLn ("Truncate from " ++ show ix)
        runProxyStore next

instance CS.Store ProxyStore where
    type Value ProxyStore = Int

instance CS.StoreIO ProxyStore where
    interpret cmds s = do
        (r, s') <- runStateT (runProxyStore cmds) s
        return (s', r)
