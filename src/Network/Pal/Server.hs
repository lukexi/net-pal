{-# LANGUAGE RecordWildCards #-}
module Network.Pal.Server where
import Network.Socket

import Network.ENet

import Network.Pal.Shared
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM
import Foreign.Ptr

broadcastMessage :: (Binary a, Foldable t, Functor t)
                 => Ptr Host -> ChannelID -> t PacketFlag -> a -> IO ()
broadcastMessage host channelID flags message =
    hostBroadcast host channelID =<<
        packetPoke (encodePacket flags message)

startServer :: Binary a
            => HostConfig
            -> PortNumber
            -> IO (TChan ([PacketFlag], a), TChan a)
startServer hostConfig port = do

    outgoingChan <- newTChanIO
    incomingChan <- newTChanIO

    _serverThread <- forkOS . withENetDo $ do
        let address = Just (SockAddrInet port iNADDR_ANY)

        host <- fromJustNote "Couldn't create server." <$>
            createHostWithConfig hostConfig address

        messageLoop host incomingChan outgoingChan
            (broadcastMessage host)
    return (outgoingChan, incomingChan)
