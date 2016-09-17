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
        message <- atomically $ tryReadTChan receiveChan
        putStrLn $ "RECEIVED: " ++ show message
        threadDelay 1000000
        atomically $ writeTChan sendChan $
            ( [Reliable]
            , EntityUpdate 1234 (1.2, 3.4)
            )
    return ()

