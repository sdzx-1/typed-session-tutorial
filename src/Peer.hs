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
  returnAt ()

serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) S0
serverPeer = I.do
  Ping <- await
  yield Pong
  returnAt ()
