
import Config
import Discus.GitHub


main
 = startHookServer
 $ Config
        { configIrcHost         = "irc.freenode.org"
        , configIrcPort         = 6667
        , configIrcChannel      = "#discus-lang"
        , configIrcNick         = "discus-bot"
        , configIrcName         = "loves you"
        , configHookPort        = 4000 }
