{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TypeApplications #-}

module Peer where

import Data.IFunctor (At (..), returnAt)
import qualified Data.IFunctor as I
import Data.IORef
import Protocol
import System.Random (randomRIO)
import Type
import TypedSession.Core

choice :: Int -> ChoiceNextActionFun IO
choice i = do
  if
    | i == 101 -> liftConstructor BranchSt_Finish
    | i == 100 -> liftConstructor BranchSt_Check
    | i `mod` 10 == 0 -> liftConstructor BranchSt_Check
    | otherwise -> liftConstructor BranchSt_Continue

clientPeer
  :: Int
  -> IORef Int
  -> Peer PingPongRole PingPong Client IO (At (Either String Int) (Done Client)) ClientStartSt
clientPeer i valRef = I.do
  choice i I.>>= \case
    BranchSt_Continue -> I.do
      yield Ping
      Pong <- await
      At randVal <- liftm $ do
        rval <- randomRIO @Int (0, 100)
        modifyIORef valRef (+ rval)
        pure rval
      yield (Add randVal)
      clientPeer (i + 1) valRef
    BranchSt_Finish -> I.do
      yield ServerStop
      yield CounterStop
      At val <- liftm $ readIORef valRef
      returnAt (Right val)
    BranchSt_Check -> I.do
      At val <- liftm $ readIORef valRef
      yield (CheckVal val)
      await I.>>= \case
        CheckSuccessed -> clientPeer (i + 1) valRef
        (CheckFailed st) -> I.do
          liftm $ putStrLn st
          yield (Fix val)
          FixFinish <- await
          clientPeer i valRef

serverPeer :: Peer PingPongRole PingPong Server IO (At (Either String ()) (Done Server)) (ServerStartSt s)
serverPeer = I.do
  await I.>>= \case
    Ping -> I.do
      yield Pong
      serverPeer
    ServerStop -> returnAt (Right ())

checkFun :: Int -> Int -> CheckResultFun IO
checkFun val ci =
  if val == ci
    then liftConstructor BranchSt_Successed
    else liftConstructor BranchSt_Failed

counterPeer :: Int -> Peer PingPongRole PingPong Counter IO (At (Either String Int) (Done Server)) (CounterStartSt s)
counterPeer val = I.do
  liftm $ putStrLn $ "Counter val is: " ++ show val
  await I.>>= \case
    Add i -> I.do
      At rval <- liftm $ randomRIO @Int (0, 1000)
      -- 0.995^100 ~= 0.6
      let nval = val + if rval < 995 then i else (i - 1)
      counterPeer nval
    CounterStop -> returnAt (Right val)
    CheckVal ci -> I.do
      checkFun val ci I.>>= \case
        BranchSt_Successed -> I.do
          yield CheckSuccessed
          counterPeer val
        BranchSt_Failed -> I.do
          let reason =
                "Check failed, now value is "
                  <> show val
                  <> ", check value is "
                  <> show ci
          yield (CheckFailed reason)
          (Fix newVal) <- await
          yield FixFinish
          counterPeer newVal