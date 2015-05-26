module Main where

import Network

import Network.Protocol.Server

import TestProtocol
import qualified TestStore as TS

----------------------------------------------------------------------

main :: IO ()
main = serveOn (PortNumber 44444) testProtocol TS.empty
