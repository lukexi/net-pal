{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Network.ENet

import Shared
import Network.Pal.Client
import Network.Pal.Shared

main :: IO ()
main = do
    (sendChan, receiveChan) <- startClient clientHostConfig "127.0.0.1" 1234

    forever $ do
        message <- atomically $ tryReadTChan receiveChan
        putStrLn $ "RECEIVED: " ++ show message
        threadDelay 1000000
        atomically $ writeTChan sendChan $
            ( [Reliable]
            , EntityUpdate 1234 (1.2, 3.4)
            )


clientHostConfig :: HostConfig
clientHostConfig = HostConfig
    { hcMaxClients = 1
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }
