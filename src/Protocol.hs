{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Protocol where

import Data.Kind (Type)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

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

(>>) :: (IMonad (m :: (x -> Type) -> x -> Type)) => m (At a j) i -> m b j -> m b i
m >> f = ibind (\(At _) -> f) m

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
    :: (Msg role' ps from send1 sps1 recv ~> Peer role' ps recv m ia)
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

-------------------- yield function-------------------------

yield
  :: forall role' ps from send sps recv rps m
   . (Functor m)
  => Msg role' ps from send sps recv rps
  -> Peer role' ps send m (At () sps) from
yield msg =
  Yield
    msg
    (returnAt ())

-------------------- await function-------------------------
await
  :: forall role' ps from send sps recv m
   . (Functor m)
  => Peer role' ps recv m (Msg role' ps from send sps recv) from
await = Await ireturn

-------------------- liftm function-------------------------
liftm :: (Functor m) => m a -> Peer role' ps r m (At a ts) ts
liftm m = LiftM (returnAt <$> m)

----------------ExampleRole, Example -------------------
data ExampleRole = Client | Server
  deriving (Show, Eq, Ord, Enum, Bounded)

---------------------------------
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

instance Show (Msg ExampleRole Example from send sendNewSt recv receiverNewSt) where
  show = \case
    Ping -> "Ping"
    Ping1 -> "Ping1"

----------------------- clientPeer -------------------------
clientPeer :: Peer ExampleRole Example Client IO (At () (Done Client)) S0
clientPeer = Protocol.do
  yield Ping
  yield Ping1
  liftm $ putStrLn "Client finish!"

----------------------- serverPeer -------------------------
{-
Msg Ping,  Client -> Server
Msg Ping1, Client -> Server
-}

serverPeer :: Peer ExampleRole Example Server IO (At () (Done Server)) S0
serverPeer = Protocol.do
  await Protocol.>>= \case
    Ping ->
      await Protocol.>>= \case
        -- This will generate a warning:
        --   Pattern match has inaccessible right hand side
        --      In a \case alternative: Ping1 -> ...compile(-Woverlapping-patterns)
        --
        -- But semantically there shouldn't be any warnings here.
        Ping1 -> Protocol.do
          liftm $ putStrLn "Server finish!"
          returnAt ()

runClientAndServer
  :: Peer ExampleRole Example Client IO (At () (Done Client)) startSt
  -> Peer ExampleRole Example Server IO (At () (Done Server)) startSt1
  -> IO ()
runClientAndServer cp sp = case (cp, sp) of
  (Yield msg cCont, Await sCont) -> do
    let sNext = sCont (unsafeCoerce msg)
    putStrLn $ printf "Client send msg %s to Server" (show msg)
    runClientAndServer cCont sNext
  (Await cCont, Yield msg sCont) -> do
    let cNext = cCont (unsafeCoerce msg)
    putStrLn $ printf "Server send msg %s to Client" (show msg)
    runClientAndServer cNext sCont
  (LiftM clientM, sPeer) -> do
    cNext <- clientM
    runClientAndServer cNext sPeer
  (cPeer, LiftM serverM) -> do
    sNext <- serverM
    runClientAndServer cPeer sNext
  (IReturn _, IReturn _) -> pure ()
  _ -> error "It will never happen!"

main :: IO ()
main = runClientAndServer clientPeer serverPeer