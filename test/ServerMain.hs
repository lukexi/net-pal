{-# LANGUAGE RecordWildCards #-}

import Network.Pal.Server
import Network.Pal.Shared

import Shared

main :: IO ()
main = startServer serverHostConfig 1234 $ \message -> do
    print (message :: Message)
    return ()

serverHostConfig :: HostConfig
serverHostConfig = HostConfig
    { hcMaxClients = 32
    , hcNumChannels = 2
    , hcBandwidthIn = 0
    , hcBandwidthOut = 0
    }

