{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Protocol where

import Data.Kind (Type)

---------------- Mcbirde Indexed Monad--------------------
infixr 0 ~>

type f ~> g = forall x. f x -> g x

class IFunctor f where
  imap :: (a ~> b) -> f a ~> f b

class (IFunctor m) => IMonad m where
  ireturn :: a ~> m a
  ibind :: (a ~> m b) -> m a ~> m b

data At :: Type -> k -> k -> Type where
  At :: a -> At a k k

(>>=) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m a ix -> (a ~> m b) -> m b ix
m >>= f = ibind f m

returnAt :: (IMonad m) => a -> m (At a k) k
returnAt = ireturn . At

------------------------Protocol type class-----------------------
class Protocol role' ps where
  type Done (sr :: role') :: ps
  data Msg role' ps (fromSt :: ps) (sender :: role') (senderNewSt :: ps) (receiver :: role') (receiverNewSt :: ps)

--------------------------Action peer -----------------------------
data Peer role' ps (r :: role') (m :: Type -> Type) (ia :: ps -> Type) (st :: ps) where
  IReturn :: ia st -> Peer role' ps r m ia st
  LiftM :: m (Peer role' ps r m ia st') -> Peer role' ps r m ia st
  Yield
    :: Msg role' ps from send sps recv rps
    -> Peer role' ps send m ia sps
    -> Peer role' ps send m ia from
  Await
    :: (Msg role' ps from send sps recv ~> Peer role' ps recv m ia)
    -> Peer role' ps recv m ia from

------------------------IFunctor and IMonad instance for Peer--------------------------
instance (Functor m) => IFunctor (Peer role' ps r m) where
  imap f = \case
    IReturn ia -> IReturn (f ia)
    LiftM f' -> LiftM (fmap (imap f) f')
    Yield ms cont -> Yield ms (imap f cont)
    Await cont -> Await (imap f . cont)

instance (Functor m) => IMonad (Peer role' ps r m) where
  ireturn = IReturn
  ibind f = \case
    IReturn ia -> (f ia)
    LiftM f' -> LiftM (fmap (ibind f) f')
    Yield ms cont -> Yield ms (ibind f cont)
    Await cont -> Await (ibind f . cont)

-------------------- await function-------------------------
await
  :: (Functor m)
  => Peer role' ps recv m (Msg role' ps from send sps recv) from
await = Await ireturn

----------------ExampleRole, Example -------------------
data ExampleRole = Client | Server
  deriving (Show, Eq, Ord, Enum, Bounded)

data Example = End | S0 | S1

---------------- Protocol instance for ExampleRole and Example-------------------
instance Protocol ExampleRole Example where
  type Done 'Client = 'End
  type Done 'Server = 'End
  data
    Msg
      ExampleRole
      Example
      (from :: Example)
      (send :: ExampleRole)
      (sendNewSt :: Example)
      (recv :: ExampleRole)
      (receiverNewSt :: Example)
    where
    Ping :: Msg ExampleRole Example 'S0 'Client 'S1 'Server 'S1
    Ping1 :: Msg ExampleRole Example 'S1 'Client 'End 'Server 'End

----------------------- serverPeer -------------------------
{-
Msg Ping,  Client ->  Server
Msg Ping1, Client -> Server
-}
serverPeer :: Peer ExampleRole Example Server IO (At () (Done Server)) S0
serverPeer = Protocol.do
  await Protocol.>>= \case
    Ping -> Protocol.do
      await Protocol.>>= \case
        Ping1 -> returnAt ()