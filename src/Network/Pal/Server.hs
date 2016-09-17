{-# LANGUAGE RecordWildCards #-}
module Network.Pal.Server where
import Control.Monad
import Network.Socket

import Network.ENet

import Network.Pal.Shared
import Data.Binary
import Control.Concurrent
import Control.Concurrent.STM

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

        void . forever $ do
            atomically (tryReadTChan outgoingChan) >>= mapM_
                (\(flags, message) ->
                    broadcastMessage host (ChannelID 0) flags message
                )
            let maxWaitMillisec = 1
            maybeEvent <- hostService host maxWaitMillisec
            case maybeEvent of
                Right (Just event) -> do
                    case evtType event of
                        Receive -> do
                            Packet _flags contents <- packetPeek
                                (evtPacket event)
                            atomically
                                $ writeTChan incomingChan
                                $! decodeStrict contents
                        _ -> return ()
                Left anError -> do
                    putStrLn anError
                _ -> return ()
    return (outgoingChan, incomingChan)
