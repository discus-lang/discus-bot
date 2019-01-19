
module Discus.GitHub where
import Config
import Discus.Github.Event
import Control.Monad
import qualified System.IO                      as S
import qualified Control.Exception              as E
import qualified Control.Concurrent             as C
import qualified Network.Socket                 as N
import qualified Text.Show.Pretty               as P


-- |
startHookServer :: Config -> IO ()
startHookServer c
 = N.withSocketsDo
 $ do   addr <- resolve (configHookPort c)
        E.bracket (open addr) N.close loop

 where
  resolve port
   = do let hints
                = N.defaultHints
                { N.addrFlags      = [N.AI_PASSIVE]
                , N.addrSocketType = N.Stream }

        addr : _ <- N.getAddrInfo (Just hints) Nothing (Just $ show port)
        return addr

  open addr
   = do s <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
        N.setSocketOption s N.ReuseAddr 1
        N.bind s (N.addrAddress addr)
        N.setCloseOnExecIfNeeded $ N.fdSocket s
        N.listen s 10
        return s

  loop s
   = forever $ do
        (sPeer, saPeer) <- N.accept s
        putStrLn $ "Connection from " ++ show saPeer
        hPeer <- N.socketToHandle sPeer S.ReadWriteMode
        talk hPeer
--        void $ C.forkFinally (talk hPeer) (\_ -> S.hClose hPeer)

  talk h
   = do -- Read the request payload.
        str <- S.hGetContents h

        -- Send the client an OK response so it knows we've got the message.
        -- If we don't do this then the client think we haven't received it.
        S.hPutStr h $ concatMap (\l -> l ++ "\r\n")
         [ "HTTP/1.1 200 OK"
         , "Content-Type: text/html; charset=UTF-8"
         , "Content-Length: 0"
         , "Connection: close"
         , "" ]
        S.hFlush h

        -- Log the request to the console.
        putStrLn $ "-- > http request " ++ replicate 60 '-'
        putStrLn $ P.ppShow $ parseEvent str
        putStrLn $ "-- < http request " ++ replicate 60 '-'
        S.hFlush S.stdout

        S.hClose h

