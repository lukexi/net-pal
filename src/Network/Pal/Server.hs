{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Network.Pal.Server (startServer, broadcastMessage, broadcastMessageExcept) where
import Network.Socket

import Network.ENet

import Network.Pal.Shared
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Foreign.Ptr
import Control.Monad


broadcastMessage :: (Binary a, Foldable t, Functor t)
                 => Ptr Host -> ChannelID -> t PacketFlag -> a -> IO ()
broadcastMessage host channelID flags message =
    hostBroadcast host channelID =<<
        packetPoke (encodePacket flags message)


broadcastMessageExcept :: (Binary a, Foldable t, Functor t)
                       => Ptr Host -> Ptr Peer -> ChannelID -> t PacketFlag -> a -> IO ()
broadcastMessageExcept host exceptPeer channelID flags message =
    hostBroadcastExcept host exceptPeer channelID =<<
        packetPoke (encodePacket flags message)



startServer :: Binary a
            => HostConfig
            -> PortNumber
            -> IO (TChan ([PacketFlag], Ptr Peer, a), TChan (Ptr Peer, a))
startServer hostConfig port = do

    outgoingChan <- newTChanIO
    incomingChan <- newTChanIO

    _serverThread <- forkOS . withENetDo $ do
        let address = Just (SockAddrInet port iNADDR_ANY)

        host <- fromJustNote "Couldn't create server." <$>
            createHostWithConfig hostConfig address

        serverLoop host incomingChan outgoingChan

    return (outgoingChan, incomingChan)

serverLoop :: (Binary a)
           => Ptr Host
           -> TChan (Ptr Peer, a)
           -> TChan ([PacketFlag], Ptr Peer, a)
           -> IO b
serverLoop host incomingChan outgoingChan = forever $ do
    -- Pass outgoing messages to the outgoing action
    atomically (tryReadTChan outgoingChan) >>= mapM_
        (\(flags, peer, message) -> do
            broadcastMessageExcept host
                peer
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
                        peer = evtPeer event
                    atomically $ writeTChan incomingChan (peer, message)
                _ -> return ()
            print event
        Left anError -> putStrLn anError
        _ -> return ()
