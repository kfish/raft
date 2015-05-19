module ClientTypes (
    ClientCommand(..)
  , ClientResponse(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Serialize
import qualified Data.ByteString.Char8 as S

data ClientCommand k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  | CmdHelp (Maybe S.ByteString)
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump
  deriving (Show, Eq)

data ClientResponse k v =
    RspSetOK k v
  | RspGetOK k v
  | RspGetFail k
  deriving (Show, Eq)

instance (Serialize k, Serialize v) => Serialize (ClientCommand k v) where
    put (CmdSet k v) = do
        put 'S'
        put k
        put v
    put (CmdGet k) = do
        put 'G'
        put k
    put (CmdSleep n) = do
        put 'D'
        put n
    put _ = return ()

    get = do
        x <- get :: Get Char
        case x of
            'S' -> CmdSet <$> get <*> get
            'G' -> CmdGet <$> get
            'D' -> CmdSleep <$> get
            _ -> mzero
                
instance (Serialize k, Serialize v) => Serialize (ClientResponse k v) where
    put (RspSetOK k v) = do
        put 'S'
        put k
        put v
    put (RspGetOK k v) = do
        put 'G'
        put k
        put v
    put (RspGetFail k) = do
        put 'F'
        put k

    get = do
        x <- get :: Get Char
        case x of
            'S' -> RspSetOK <$> get <*> get
            'G' -> RspGetOK <$> get <*> get
            'F' -> RspGetFail <$> get
