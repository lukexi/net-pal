{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Network.Pal.Client where
import Foreign.Ptr
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Network.Socket (SockAddr(..), inet_addr, PortNumber)

import Network.ENet

import Data.Binary

import Network.Pal.Shared



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
        let noPublicAddress = Nothing -- Nothing == Client (address is for public connection)

        host <- fromJustNote "Couldn't create host :(" <$>
            createHostWithConfig hostConfig noPublicAddress

        -- Connect to the server
        serverPeer <- fromJustNote "Couldn't connect to host :(" <$>
            connectToHost hostConfig host address port

        -- Await the connection event
        awaitConnection host serverPeer 5000

        forever $ do
            -- Send incoming messages to the server
            atomically (tryReadTChan outgoingChan) >>= mapM_
                (\(flags, message) -> do
                    sendMessage
                        serverPeer
                        (ChannelID 0)
                        flags
                        message
                )
            let maxWaitMillisec = 1
            maybeEvent <- hostService host maxWaitMillisec
            case maybeEvent of
                Right (Just event) -> do
                    case evtType event of
                        Receive -> do
                            Packet _flags contents <- packetPeek (evtPacket event)
                            atomically $ writeTChan incomingChan $!
                                decodeStrict contents
                        _ -> return ()
                    print event
                Left anError -> putStrLn anError
                _ -> return ()
    return (outgoingChan, incomingChan)
