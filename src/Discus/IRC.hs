
module Discus.IRC where
import Config
import qualified System.IO              as S
import qualified Control.Concurrent     as C
import qualified Network.Socket         as N


-- | Connect to the IRC server.
ircConnect :: Config -> IO (C.Chan String)
ircConnect c
 = N.withSocketsDo
 $ do
        let host   = configIrcHost c
        let port   = configIrcPort c

        -- Connect to the server.
        let hints  =  N.defaultHints { N.addrSocketType = N.Stream }
        addr : _   <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)

        s <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
        N.connect s $ N.addrAddress addr
        h <- N.socketToHandle s S.ReadWriteMode

        -- Send login details.
        ircWriteLine h $ "NICK " ++ configIrcNick c
        ircWriteLine h $ "USER " ++ configIrcNick c ++ " 0 * :" ++ configIrcName c
        ircWriteLine h $ "JOIN " ++ configIrcChannel c

        -- Start the receiver.
        _tidRecv <- C.forkIO $ goIrcRecvLoop h

        -- Start the sender.
        chanSend <- C.newChan
        _tidSend <- C.forkIO $ goIrcSendLoop h chanSend

        return chanSend


-- | Endlessly receive lines from the IRC server and write them to stdout.
goIrcRecvLoop :: S.Handle -> IO ()
goIrcRecvLoop h
 = do   str     <- ircReadLine h
        putStrLn str
        if null str
         then return ()
         else goIrcRecvLoop h


-- Endlessly wait for strings from a channel and write them to the IRC server.
goIrcSendLoop :: S.Handle -> C.Chan String -> IO ()
goIrcSendLoop h c
 = do   str <- C.readChan c
        ircWriteLine h str
        goIrcSendLoop h c


-- | Say a message in a channel on the IRC server.
ircSay :: Config -> S.Handle -> String -> IO ()
ircSay c h str
 =      S.hPutStr h $ "PRIVMSG " ++ configIrcChannel c ++ " :" ++ str


-- | Write a line to the IRC server.
ircWriteLine :: S.Handle -> String -> IO ()
ircWriteLine h str
 = do   S.hPutStr h $ str ++ "\r\n"
        S.hFlush h


-- | Read a line from the IRC server.
ircReadLine :: S.Handle -> IO String
ircReadLine h
 = do   str     <- S.hGetLine h

        -- Clean off the hard CRs at the end of lines.
        case reverse str of
         '\r' : rest    -> return $ reverse rest
         _              -> return str



