module Main where

import Network

import Network.Protocol.Server

import Raft.Store.Sqlite

import TestProtocol

----------------------------------------------------------------------

main :: IO ()
main = open >>= serveOn (PortNumber 44444) testProtocol
