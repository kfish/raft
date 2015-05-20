{-# LANGUAGE FlexibleContexts #-}

module Network.Protocol.Server (
      serveOn
) where

import Control.Concurrent (forkIO)
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
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

serveOn :: (Protocol p, Serialize (Request p), Serialize (Response p))
        => PortID -> p -> IO ()
serveOn port p0 = do
    s <- listenOn port
    forever $ do
        (h, addr) <- S.accept s
        stream <- mkSocketStream h
        forkIO (loop stream p0 `finally` S.sClose h)
  where
    loop stream p = do
      cmd <- Stream.runGet stream get
      let (p', e, m'rsp) = step p cmd
      case m'rsp of
          Just rsp -> Stream.runPut stream $ put rsp
          Nothing -> return ()
      loop stream p'

