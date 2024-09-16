{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Protocol where

import Data.IFunctor (At, returnAt)
import qualified Data.IFunctor
import qualified Data.IFunctor as I
import GHC.Exts (DataToTag (..), Int (..))
import TypedSession.Core

-- [pingpongProtocol|
-- Msg Ping [] Client Server
-- Msg Ping1 [] Client Server
-- Terminal

-- | ]
data PingPongRole = Client | Server
  deriving (Show, Eq, Ord, Enum, Bounded)

data PingPongBranchSt = PingPongBranchSt
  deriving (Show, Eq, Ord, Enum, Bounded)

data SPingPongRole (a_a2Vf :: PingPongRole) where
  SClient :: SPingPongRole 'Client
  SServer :: SPingPongRole 'Server

type instance Data.IFunctor.Sing = SPingPongRole

instance Data.IFunctor.SingI 'Client where
  sing = SClient
instance Data.IFunctor.SingI 'Server where
  sing = SServer
instance SingToInt PingPongRole where
  singToInt x_a2Vg =
    I# (dataToTag# x_a2Vg)

data PingPong = End | S0 | S1

data SPingPong (a_a2Vi :: PingPong) where
  SEnd :: SPingPong 'End
  SS0 :: SPingPong 'S0
  SS1 :: SPingPong 'S1
type instance Data.IFunctor.Sing = SPingPong

type ClientStartSt = 'S0
type ServerStartSt = 'S0

instance Data.IFunctor.SingI 'End where
  sing = SEnd
instance Data.IFunctor.SingI 'S0 where
  sing = SS0
instance Data.IFunctor.SingI 'S1 where
  sing = SS1
instance SingToInt PingPong where
  singToInt x_a2Vl =
    I# (dataToTag# x_a2Vl)

instance Protocol PingPongRole PingPong where
  type Done 'Client = 'End
  type Done 'Server = 'End
  data
    Msg
      PingPongRole
      PingPong
      (from)
      (send)
      (sendNewSt)
    where
    Ping :: Msg PingPongRole PingPong 'S0 '(Client, S1) '(Server, S1)
    Ping1 :: Msg PingPongRole PingPong 'S1 '(Client, End) '(Server, End)

serverPeer :: Peer PingPongRole PingPong Server IO (At () (Done Server)) S0
serverPeer = I.do
  await I.>>= \case
    Recv Ping -> I.do
      await I.>>= \case
        Recv Ping1 -> returnAt ()