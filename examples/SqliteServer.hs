module Main where

import Network

import Network.Protocol.Server

import Raft.Store.Sqlite

import TestProtocol

----------------------------------------------------------------------

main :: IO ()
main = open "test.db" >>= serveOn (PortNumber 44444) testProtocol
