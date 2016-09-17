{-# LANGUAGE RecordWildCards #-}
module Network.Pal.Shared where
import Foreign.C
import Data.Word
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Network.Socket
import Foreign.Ptr
import Network.ENet
import Control.Concurrent.STM
import Control.Monad

data HostConfig = HostConfig
    { hcMaxClients :: CSize
    , hcNumChannels :: CSize
    , hcBandwidthIn :: Word32
    , hcBandwidthOut :: Word32
    }

fromJustNote :: String -> Maybe t -> t
fromJustNote note m = case m of
    Just a -> a
    Nothing -> error note

decodeStrict :: Binary a => ByteString -> a
decodeStrict = decode . L.fromStrict
encodeStrict :: Binary a => a -> ByteString
encodeStrict = L.toStrict . encode

createHostWithConfig :: HostConfig
                     -> Maybe SockAddr -> IO (Maybe (Ptr Host))
createHostWithConfig HostConfig{..} address = hostCreate address
    hcMaxClients hcNumChannels
    hcBandwidthIn hcBandwidthOut


reliablePacket :: Binary a => a -> Packet
reliablePacket contents = encodePacket [Reliable] contents

encodePacket :: (Binary a, Foldable t, Functor t)
             => t PacketFlag -> a -> Packet
encodePacket flags contents = Packet
    (makePacketFlagSet flags) (encodeStrict contents)


messageLoop host incomingChan outgoingChan outgoingAction = forever $ do
    -- Pass outgoing messages to the outgoing action
    atomically (tryReadTChan outgoingChan) >>= mapM_
        (\(flags, message) -> do
            outgoingAction
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
                    atomically $ writeTChan incomingChan $!
                        decodeStrict contents
                _ -> return ()
            print event
        Left anError -> putStrLn anError
        _ -> return ()
