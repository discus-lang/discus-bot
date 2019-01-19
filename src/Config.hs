
module Config where

data Config
        = Config
        { configIrcHost         :: String
        , configIrcPort         :: Integer
        , configIrcChannel      :: String
        , configIrcNick         :: String
        , configIrcName         :: String }
        deriving (Show)


configDefault :: Config
configDefault
        = Config
        { configIrcHost         = "irc.freenode.org"
        , configIrcPort         = 6667 :: Integer
        , configIrcChannel      = "#discus-lang"
        , configIrcNick         = "discus-bot"
        , configIrcName         = "discus-lang channel bot" }

