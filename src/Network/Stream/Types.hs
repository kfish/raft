{-# LANGUAGE DeriveDataTypeable #-}

module Network.Stream.Types where

import           Control.Exception (Exception)
import           Data.Text (Text)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
{-
import           Data.ByteString (ByteString)
import qualified Data.Vector as V
import           Data.Word (Word16, Word64)
-}

------------------------------------------------------------------------

data Endpoint = Endpoint 
    { epHost :: !HostName
    , epPort :: !Port
    } deriving (Eq, Ord, Show) 
       
type HostName = Text
type Port     = Int

------------------------------------------------------------------------

data RemoteError = RemoteError !Text !Text
    deriving (Show, Eq, Data, Typeable)

instance Exception RemoteError

------------------------------------------------------------------------

data DecodeError = DecodeError !Text
    deriving (Show, Eq, Data, Typeable)

instance Exception DecodeError

------------------------------------------------------------------------

data ConnectionClosed = ConnectionClosed
    deriving (Show, Eq, Data, Typeable)

instance Exception ConnectionClosed

------------------------------------------------------------------------
