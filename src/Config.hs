
module Config where


-- | Bot Config.
data Config
        = Config
        { configIrcHost         :: String       -- ^ Host name of IRC server.
        , configIrcPort         :: Integer      -- ^ Port number on IRC server.
        , configIrcChannel      :: String       -- ^ Send messages in this IRC channel.
        , configIrcNick         :: String       -- ^ Nick name for IRC messages.
        , configIrcName         :: String       -- ^ User name for IRC messages.

        , configHookPort        :: Integer      -- ^ Local port to listen to for Webhooks.
        }
        deriving (Show)

