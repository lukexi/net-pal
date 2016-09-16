{-# LANGUAGE RecordWildCards #-}
module Network.Pal.Server where
import Control.Monad
import Network.Socket

import Network.ENet

import Network.Pal.Shared
import Data.Binary

startServer :: Binary a => HostConfig -> PortNumber -> (a -> IO ()) -> IO ()
startServer hostConfig port action = do

    withENetDo $ do
        let address = Just (SockAddrInet port iNADDR_ANY)

        host <- fromJustNote "Couldn't create server." <$>
            createHostWithConfig hostConfig address

        void . forever $ do
            let maxWaitMillisec = 1
            maybeEvent <- hostService host maxWaitMillisec
            case maybeEvent of
                Right (Just event) -> do
                    case evtType event of
                        Receive -> do
                            Packet _flags contents <- packetPeek (evtPacket event)
                            action $! decodeStrict contents
                        _ -> return ()
                Left anError -> do
                    putStrLn anError
                _ -> return ()
