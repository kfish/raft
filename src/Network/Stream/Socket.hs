module Network.Stream.Socket
    ( S.Socket
    , bracketSocket
    , connectSocket
    , closeSocket
    ) where

import           Control.Monad.Catch (MonadMask, bracket, bracketOnError)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T
import           Network (PortID(PortNumber))
import qualified Network.Socket as S
import           Network.Stream.Types

------------------------------------------------------------------------

bracketSocket :: (MonadFail m, MonadMask m, MonadIO m) => Endpoint -> (S.Socket -> m a) -> m a
bracketSocket endpoint = bracket (connectSocket endpoint) closeSocket

closeSocket :: MonadIO m => S.Socket -> m ()
closeSocket = liftIO . S.sClose

------------------------------------------------------------------------

connectSocket :: (MonadFail m, MonadMask m, MonadIO m) => Endpoint -> m S.Socket
connectSocket endpoint = do
    (addr:_) <- liftIO $ S.getAddrInfo (Just hints) (Just host) (Just port)
    bracketOnError (newSocket addr) closeSocket $ \sock -> do
       liftIO $ S.connect sock (S.addrAddress addr)
       return sock
  where
    host  = T.unpack (epHost endpoint)
    port  = show (epPort endpoint)
    hints = S.defaultHints { S.addrFlags = [S.AI_ADDRCONFIG]
                           , S.addrSocketType = S.Stream }

newSocket :: MonadIO m => S.AddrInfo -> m S.Socket
newSocket addr = liftIO $ S.socket (S.addrFamily addr)
                                   (S.addrSocketType addr)
                                   (S.addrProtocol addr)
