{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Network.Pal.Client where
import Foreign.Ptr
import Control.Concurrent
import Control.Concurrent.STM
import Network.Socket (SockAddr(..), inet_addr, PortNumber)

import Network.ENet

import Data.Binary

import Network.Pal.Shared
import Control.Monad




connectToHost :: HostConfig
              -> Ptr Host -> String -> PortNumber -> IO (Maybe (Ptr Peer))
connectToHost hostConfig host address port = do
    sockAddress <- SockAddrInet port <$> inet_addr address
    hostConnect host
        sockAddress
        (hcNumChannels hostConfig)
        datum
    where datum = 0

awaitConnection :: Ptr Host -> Ptr Peer -> Word32 -> IO ()
awaitConnection host peer maxTime = do

    maybeConnectionEvent <- hostService host maxTime
    case maybeConnectionEvent of
        Right (Just (Event Connect thePeer channelID packetLength packetPtr)) -> do
            putStrLn "Connected!"
            print (Connect, thePeer, channelID, packetLength, packetPtr)
        _other -> do
            peerReset peer
            error "Failed to connect :("

sendMessage :: (Binary a, Foldable t, Functor t)
            => Ptr Peer -> ChannelID -> t PacketFlag -> a -> IO ()
sendMessage peer channelID flags message =
    peerSend peer channelID =<<
        packetPoke (encodePacket flags message)

startClient :: Binary a
            => HostConfig
            -> String
            -> PortNumber
            -> IO (TChan ([PacketFlag], a), TChan a)
startClient hostConfig address port = do

    outgoingChan <- newTChanIO
    incomingChan <- newTChanIO

    _clientThread <- forkOS $ withENetDo $ do

        -- Create the host
        -- Nothing == this is a Client
        let noPublicAddress = Nothing

        host <- fromJustNote "Couldn't create host :(" <$>
            createHostWithConfig hostConfig noPublicAddress

        -- Connect to the server
        serverPeer <- fromJustNote "Couldn't connect to host :(" <$>
            connectToHost hostConfig host address port
        print serverPeer

        -- Await the connection event
        awaitConnection host serverPeer 5000

        clientLoop host serverPeer incomingChan outgoingChan

    return (outgoingChan, incomingChan)

clientLoop :: (Binary a)
           => Ptr Host
           -> Ptr Peer
           -> TChan a
           -> TChan ([PacketFlag], a) -> IO b
clientLoop host serverPeer incomingChan outgoingChan = forever $ do    -- Pass outgoing messages to the outgoing action
    atomically (tryReadTChan outgoingChan) >>= mapM_
        (\(flags, message) -> do
            sendMessage serverPeer
                (ChannelID 0)
                flags
                message
        )

    -- Write incoming messages to the incomingChan
    let maxWaitMillisec = 1
    maybeEvent <- hostService host maxWaitMillisec
    case maybeEvent of
        Right (Just event) -> do
            case evtType event of
                Receive -> do
                    Packet _flags contents <- packetPeek (evtPacket event)
                    let !message = decodeStrict contents
                    atomically $ writeTChan incomingChan message
                _ -> return ()
            print event
        Left anError -> putStrLn anError
        _ -> return ()
