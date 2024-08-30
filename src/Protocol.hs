{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Protocol where

import Data.Binary (Binary (get), put)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as L
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import Type
import TypedSession.Codec
import TypedSession.Core
import TypedSession.Driver

[pingpongProtocol|

Label 0
Branch Client ChoiceNextAction {
  BranchSt Continue []
    Msg Ping [] Client Server
    Msg Pong [] Server Client
    Msg Add [Int] Client Counter
    Goto 0
  BranchSt Finish []
    Msg ServerStop [] Client Server
    Msg CounterStop [] Client Counter
    Terminal
  BranchSt Check []
    Msg CheckVal [Int] Client Counter
    Branch Counter CheckResult {
      BranchSt Successed []
        Msg CheckSuccessed [] Counter Client
        Goto 0
      BranchSt Failed []
        Msg CheckFailed [String] Counter Client
        Msg Fix [Int] Client Counter
        Msg FixFinish [] Counter Client
        Goto 0
    }
}
|]
instance Show (AnyMsg PingPongRole PingPong) where
  show (AnyMsg msg) = case msg of
    Ping -> "Ping"
    Pong -> "Pong"
    Add i -> "Add " <> show i
    ServerStop -> "ServerStop"
    CounterStop -> "CounterStop"
    CheckVal i -> "CheckVal " <> show i
    CheckSuccessed -> "CheckSuccessed"
    CheckFailed st -> "CheckFailed " <> st
    Fix i -> "Fix val " <> show i
    FixFinish -> "FixFinish"

encodeMsg :: Encode PingPongRole PingPong L.ByteString
encodeMsg = Encode $ \x -> runPut $ case x of
  Ping -> putWord8 0
  Pong -> putWord8 1
  Add i -> putWord8 2 >> put i
  ServerStop -> putWord8 3
  CounterStop -> putWord8 4
  CheckVal i -> putWord8 5 >> put i
  CheckSuccessed -> putWord8 6
  CheckFailed st -> putWord8 7 >> put st
  Fix st -> putWord8 8 >> put st
  FixFinish -> putWord8 9

getAnyMsg :: Get (AnyMsg PingPongRole PingPong)
getAnyMsg = do
  tag <- getWord8
  case tag of
    0 -> return $ AnyMsg Ping
    1 -> return $ AnyMsg Pong
    2 -> do
      i <- get
      return $ AnyMsg $ Add i
    3 -> return $ AnyMsg ServerStop
    4 -> return $ AnyMsg CounterStop
    5 -> do
      i <- get
      return $ AnyMsg $ CheckVal i
    6 -> return $ AnyMsg CheckSuccessed
    7 -> do
      st <- get
      return $ AnyMsg (CheckFailed st)
    8 -> do
      st <- get
      return $ AnyMsg (Fix st)
    9 -> return $ AnyMsg FixFinish
    _ -> fail "Invalid message tag"

convertDecoderLBS
  :: Decoder a
  -> (DecodeStep L.ByteString CodecFailure a)
convertDecoderLBS = go
 where
  go :: Decoder a -> DecodeStep L.ByteString CodecFailure a
  go (Done tr _ a) = DecodeDone a (Just $ L.fromStrict tr)
  go (Fail _ _ e) = DecodeFail (CodecFailure e)
  go (Partial k) = DecodePartial $ \mbs -> case mbs of
    Nothing -> DecodeFail (CodecFailure "Peer disconnected!!")
    Just bs -> go (k $ Just $ L.toStrict bs)

decodeMsg
  :: DecodeStep
      L.ByteString
      CodecFailure
      (AnyMsg PingPongRole PingPong)
decodeMsg = convertDecoderLBS (runGetIncremental getAnyMsg)

socketAsChannel :: Socket.Socket -> Channel IO L.ByteString
socketAsChannel socket =
  Channel{send, recv}
 where
  send :: L.ByteString -> IO ()
  send chunks = do
    Socket.sendMany socket (L.toChunks chunks)

  recv :: IO (Maybe L.ByteString)
  recv = do
    chunk <- Socket.recv socket L.smallChunkSize
    if BS.null chunk
      then return Nothing
      else return (Just (L.fromStrict chunk))

myTracer :: String -> Tracer PingPongRole PingPong IO
myTracer st v = putStrLn (st <> show v)