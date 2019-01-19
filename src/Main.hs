
import Config
import Disco.IRC
import qualified System.IO                      as S
import qualified Network.Socket                 as N
import qualified Control.Concurrent.Chan        as C

main
 = N.withSocketsDo
 $ do
        let c = configDefault
        chanSend    <- ircConnect c
        goSpin chanSend


goSpin chanSend
 = do   S.putStr "> "
        S.hFlush S.stdout
        str <- S.getLine
        C.writeChan chanSend str
        goSpin chanSend
