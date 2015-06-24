{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network

import Network.Protocol.Server
import Network.Stream.Types

import ProxyStore

import TestProtocol

----------------------------------------------------------------------

main :: IO ()
main = open (Endpoint "localhost" 44445) >>=
       serveOn (PortNumber 44444) testProtocol
