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
    clientTvar <- newTVarIO IntMap.empty
    let serverChannel = socketAsChannel serverSock
        sendMap = IntMap.fromList [(singToInt SServer, C.send serverChannel)]
        clientDriver = driverSimple (myTracer "client: ") encodeMsg sendMap clientTvar id
    thid1 <- forkIO $ decodeLoop (myTracer "client: ") Nothing (Decode decodeMsg) serverChannel clientTvar
    void $ runPeerWithDriver clientDriver clientPeer
    killThread thid1

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
        sendMap = IntMap.fromList [(singToInt SClient, C.send clientChannel)]
    serverTvar <- newTVarIO IntMap.empty
    let serverDriver = driverSimple (myTracer "server: ") encodeMsg sendMap serverTvar id
    thid1 <- forkIO $ decodeLoop (myTracer "server: ") Nothing (Decode decodeMsg) clientChannel serverTvar
    void $ runPeerWithDriver serverDriver serverPeer
    killThread thid1
    close client