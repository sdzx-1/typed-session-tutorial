{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
  yield Ping1
  Pong1 <- await
  returnAt ()

serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) S0
serverPeer = I.do
  await I.>>= \case
    Ping -> I.do
      yield Pong
      await I.>>= \case
        Ping1 -> I.do
          yield Pong1
          returnAt ()
