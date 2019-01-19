
import Config
import Discus.GitHub
import qualified System.IO                      as S
import qualified Network.Socket                 as N
import qualified Control.Concurrent.Chan        as C


main
 = startHookServer
 $ Config
        { configIrcHost         = "irc.freenode.org"
        , configIrcPort         = 6667
        , configIrcChannel      = "#discus-test"
        , configIrcNick         = "discus-bot"
        , configIrcName         = "loves you"
        , configHookPort        = 4000 }
