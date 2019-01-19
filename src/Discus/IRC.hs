
module Discus.IRC where
import Config
import qualified Data.List              as List
import qualified System.IO              as S
import qualified Control.Concurrent     as C
import qualified Network.Socket         as N


-- | Connect to the IRC server, send some messages then disconnect.
ircSendMessages :: Config -> [String] -> IO ()
ircSendMessages c msgs
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

        -- Wait until it looks like we've connected, then send our messages.
        goIrcRecvLoop c h msgs


-- | Read lines from the server until it looks like we've connected,
--   then send our messages.
goIrcRecvLoop :: Config -> S.Handle -> [String] -> IO ()
goIrcRecvLoop c h msgs
 = goRead
 where
  goRead
   = do mStr <- ircReadLine h
        case mStr of
         Nothing  -> return ()
         Just str -> goEat (stripCR str)

  goEat str
   = if null str
        then return ()
        else goSend str

  goSend str
   -- Wait until it looks like we've connected before sending our messages.
   | List.isPrefixOf (":" ++ configIrcNick c ++ " MODE") str
   = do mapM_ (ircPrivMsg c h) msgs
        ircWriteLine h "QUIT"
        goRead

   | otherwise = goRead


-- Endlessly wait for strings from a channel and write them to the IRC server.
goIrcSendLoop :: S.Handle -> C.Chan String -> IO ()
goIrcSendLoop h c
 = do   str <- C.readChan c
        ircWriteLine h str
        goIrcSendLoop h c


-- | Say a message in a channel on the IRC server.
ircPrivMsg :: Config -> S.Handle -> String -> IO ()
ircPrivMsg c h str
 =      ircWriteLine h $ "PRIVMSG " ++ configIrcChannel c ++ " :" ++ str


-- | Say a message in a channel on the IRC server.
ircNotice :: Config -> S.Handle -> String -> IO ()
ircNotice c h str
 =      ircWriteLine h $ "NOTICE " ++ configIrcChannel c ++ " :" ++ str


-- | Write a line to the IRC server.
ircWriteLine :: S.Handle -> String -> IO ()
ircWriteLine h str
 = do   S.hPutStr h $ str ++ "\r\n"
        putStrLn $ "> " ++ str
        S.hFlush S.stdout
        S.hFlush h


-- | Read a line from the IRC server.
ircReadLine :: S.Handle -> IO (Maybe String)
ircReadLine h
 = goCheck
 where
  goCheck
   = do bEOF <- S.hIsEOF h
        if bEOF
         then return Nothing
         else goRead

  goRead
   = do str <- S.hGetLine h
        putStrLn $ "< " ++ str
        S.hFlush S.stdout
        return $ Just $ stripCR str


-- | Strip trailing CR from the end of a line.
stripCR :: String -> String
stripCR str
 = case reverse str of
        '\r' : rest     -> reverse rest
        _               -> str


