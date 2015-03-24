module ClientTypes (
    Cmd(..)
) where

import qualified Data.ByteString.Char8 as S

data Cmd k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  | CmdHelp (Maybe S.ByteString)
  -- | CmdUse Host PortNumber
  -- | CmdPause
  -- | CmdDump
