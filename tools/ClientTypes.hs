module ClientTypes (
    Cmd(..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Binary
import qualified Data.ByteString.Char8 as S

data Cmd k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  | CmdHelp (Maybe S.ByteString)
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump

instance (Binary k, Binary v) => Binary (Cmd k v) where
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
                
