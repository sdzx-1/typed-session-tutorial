{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}

module Run where

import Control.Concurrent.Class.MonadSTM
import qualified Control.Exception as E
import Control.Monad (void)
import Control.Monad.Class.MonadFork (forkIO, killThread)
import qualified Data.IntMap as IntMap
import Network.Socket
import Peer
import Protocol
import TypedSession.Codec
import qualified TypedSession.Codec as C
import TypedSession.Core
import TypedSession.Driver

getSocket :: HostName -> ServiceName -> IO Socket
getSocket host port = do
  addr <- resolve
  open addr
 where
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
    head <$> getAddrInfo (Just hints) (Just host) (Just port)

  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock

runTCPClient :: IO ()
runTCPClient = withSocketsDo $ do
  E.bracket
    ( do
        serverSock <- getSocket "127.0.0.1" "3000"
        pure serverSock
    )
    (\a -> close a)
    client
 where
  client serverSock = do
    let serverChannel = socketAsChannel serverSock
    clientDriver <-
      driverSimple
        (myTracer "client: ")
        encodeMsg
        (Decode decodeMsg)
        [(SomeRole SServer, serverChannel)]
        id
    void $ runPeerWithDriver clientDriver clientPeer

runTCPServer :: IO ()
runTCPServer = runTCPServer' Nothing "3000"

runTCPServer' :: Maybe HostName -> ServiceName -> IO ()
runTCPServer' mhost port = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close start
 where
  resolve = do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) mhost (Just port)

  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1024
    return sock

  start sock = do
    (client, _peer) <- accept sock
    let clientChannel = socketAsChannel client
    serverDriver <-
      driverSimple
        (myTracer "server: ")
        encodeMsg
        (Decode decodeMsg)
        [(SomeRole SClient, clientChannel)]
        id
    void $ runPeerWithDriver serverDriver serverPeer
    close client