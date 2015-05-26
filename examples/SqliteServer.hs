module Main where

import Network

import Network.Protocol.Server

import TestProtocol
import qualified SqliteStore as SS

----------------------------------------------------------------------

main :: IO ()
main = SS.open >>= serveOn (PortNumber 44444) testProtocol
