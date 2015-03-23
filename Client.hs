
module Main where

import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO

data Client = Client
  { 
  }

data Command k v =
    CmdSet k v
  | CmdGet k
  | CmdSleep Int
  -- | CmdUse Host PortNumber
  | CmdPause
  | CmdDump

main :: IO ()
main = do
  args <- getArgs
  forever $ do
    line <- getLine
    putStrLn ("Got " ++ line)
