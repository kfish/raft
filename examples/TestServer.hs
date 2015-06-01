module Main where

import Network

import Network.Protocol.Server

import TestProtocol
import Raft.Store.Volatile

----------------------------------------------------------------------

main :: IO ()
main = serveOn (PortNumber 44444) testProtocol empty
