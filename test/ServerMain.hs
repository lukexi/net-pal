{-# LANGUAGE RecordWildCards #-}



import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Network.Pal.Server
import Network.Pal.Shared

import Shared
import Network.ENet

serverHostConfig :: HostConfig
serverHostConfig = HostConfig
    { hcMaxClients = 32
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }



main :: IO ()
main = do
    (sendChan, receiveChan) <- startServer serverHostConfig 1234

    _ <- forever $ do
        maybeMessage <- atomically $ tryReadTChan receiveChan

        case maybeMessage of
            Nothing -> return ()
            Just (peer, message) -> do
                putStrLn $ "RECEIVED: " ++ show message
                threadDelay 1000000
                atomically $ writeTChan sendChan $
                    ( [Reliable]
                    , peer
                    , message :: Message
                    )
    return ()

