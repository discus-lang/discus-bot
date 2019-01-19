
module Discus.GitHub where
import Config
import Discus.Github.Request
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
   = do
        s <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
        N.setSocketOption s N.ReuseAddr 1
        N.bind s (N.addrAddress addr)

        let fd = N.fdSocket s
        N.setCloseOnExecIfNeeded fd
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
   = do str <- S.hGetContents h
        S.hPutStr h $ concatMap (\l -> l ++ "\r\n")
         [ "HTTP/1.1 200 OK"
         , "Content-Type: text/html; charset=UTF-8"
         , "Content-Length: 0"
         , "Connection: close"
         , "" ]
        S.hFlush h

        putStrLn $ "-- > http request " ++ replicate 60 '-'
        putStrLn $ P.ppShow $ parseRequest str
        putStrLn $ "-- < http request " ++ replicate 60 '-'
        S.hFlush S.stdout

        S.hClose h

