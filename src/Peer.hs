{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}

module Peer where

import Data.IFunctor (At, returnAt)
import qualified Data.IFunctor as I
import Protocol
import Type
import TypedSession.Core

clientPeer :: Peer PingPongRole PingPong Client IO (At () (Done Client)) S0
clientPeer = I.do
  yield Ping
  Pong <- await
  yield (Add 1)
  clientPeer

serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) S0
serverPeer = I.do
  Ping <- await
  yield Pong
  serverPeer

counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At Int (Done Counter)) S1
counterPeer val = I.do
  liftm $ putStrLn $ "Counter val is: " ++ show val
  Add i <- await
  counterPeer (val + i)
