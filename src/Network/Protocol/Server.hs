{-# LANGUAGE FlexibleContexts #-}

module Network.Protocol.Server (
      serveOn
) where

-- import Control.Concurrent (forkIO)
import Control.Concurrent.MonadIO --(fork)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import Network
import Network.Socket as S
import System.IO
import Text.Printf

import Network.Protocol

import Network.Stream as Stream
import Network.Stream.Socket as Stream
import Network.Stream.Types as Stream

----------------------------------------------------------------------

serveOn :: (HasFork m, MonadIO m, Protocol p m, Serialize (Request p), Serialize (Response p))
        => PortID -> p -> m ()
serveOn port p0 = do
    s <- liftIO $ listenOn port
    forever $ do
        (h, addr) <- liftIO $ S.accept s
        stream <- liftIO $ mkSocketStream h
        fork (loop stream p0 `finally` (liftIO $ S.sClose h))
  where
    loop stream p = do
      cmd <- liftIO $ Stream.runGet stream get
      (p', m'rsp) <- step p cmd
      case m'rsp of
          Just rsp -> liftIO $ Stream.runPut stream $ put rsp
          Nothing -> return ()
      loop stream p'

