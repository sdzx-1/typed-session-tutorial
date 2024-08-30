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

choice :: Int -> ChoiceNextActionFun IO
choice i = do
  if i == 100
    then liftConstructor BranchSt_Finish
    else liftConstructor BranchSt_Continue

clientPeer :: Int -> Peer PingPongRole PingPong Client IO (At () (Done Client)) S0
clientPeer i = I.do
  choice i I.>>= \case
    BranchSt_Continue -> I.do
      yield Ping
      Pong <- await
      yield (Add 1)
      clientPeer (i + 1)
    BranchSt_Finish -> I.do
      yield ServerStop
      yield CounterStop
      returnAt ()

serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) (S1 s)
serverPeer = I.do
  await I.>>= \case
    Ping -> I.do
      yield Pong
      serverPeer
    ServerStop -> returnAt ()

counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At Int (Done Server)) (S2 s)
counterPeer val = I.do
  liftm $ putStrLn $ "Counter val is: " ++ show val
  await I.>>= \case
    Add i -> counterPeer (val + i)
    CounterStop -> returnAt val