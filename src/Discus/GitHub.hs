
module Discus.GitHub where
import Config
import Discus.IRC
import Discus.GitHub.Event
import Control.Monad
import qualified System.IO                      as S
import qualified Network.Socket                 as N
import qualified Control.Exception              as E
import qualified Control.Concurrent             as C
import qualified Text.Show.Pretty               as P
import qualified Data.Char                      as Char


-- | Open a port to listen for events from github,
--   logging interesting ones to our IRC channel.
startHookServer :: Config -> IO ()
startHookServer c
 = N.withSocketsDo
 $ do   putStrLn $ "* Discus Bot Starting Up."
        addr <- resolve (configHookPort c)
        E.bracket (open addr) N.close loop
 where
  resolve port
   = do let hints = N.defaultHints
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
        putStrLn $ "* Waiting for request..."
        E.catch
         (do    (sPeer, saPeer) <- N.accept s
                putStrLn $ "! Received connection from " ++ show saPeer
                hPeer <- N.socketToHandle sPeer S.ReadWriteMode
                talk hPeer)

         (\(e :: E.SomeException) ->
          do    putStrLn $ "! Exception when serving HTTP request"
                putStrLn $ "  err = " ++ show e

                -- Put in 1s delay so we don't go into a hot loop if something
                -- goes wrong with the socket.
                --
                -- If a particular client keeps connecting and then resetting
                -- the connection then we should stall when connecting that
                -- client only, but for now just stall globally.
                --
                -- Without a better mechanism for this ppl could DOS us,
                -- but hey, we're only a lowly IRC Bot, so we'll fix this
                -- the first time someone can be bothered.
                C.threadDelay (1000 * 1000)
                loop s)

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
        let event = parseEvent str
        putStrLn $ "-- > http request " ++ replicate 60 '-'
        putStrLn $ P.ppShow event
        putStrLn $ "-- < http request " ++ replicate 60 '-'
        S.hFlush S.stdout

        (case formatEvent event of
          Just msgs
           -> do putStrLn "! Forwarding event to IRC"
                 ircSendMessages c msgs
          Nothing
           -> putStrLn "[unhandled github event]")

        S.hClose h


formatEvent :: Event -> Maybe [String]
formatEvent (EventPush sRepoName sRepoBranch commits)
 = let  -- Limit number of commits shown to 10, to avoid flooding the server.
        nShow   = 10
        csFirst = take nShow commits
        csRest  = drop nShow commits
   in   Just $ map (formatCommit sRepoName sRepoBranch) csFirst
             ++ if length csRest > 0
                 then ["[push] ... and " ++ show (length csRest) ++ " more"]
                 else []
formatEvent _
 = Just []

formatCommit sRepoName sRepoBranch (Commit sAuthor sMessage)
 =  "[push] "
 ++         colored aMagenta sRepoName
 ++ "/"  ++ sRepoBranch
 ++ " "  ++ colored aCyan    sAuthor
 ++ ": " ++ sMessage
 where  aReset   = Char.chr 0x0f : ""
        aMagenta = Char.chr 0x03 : "13"
        aCyan    = Char.chr 0x03 : "10"

        colored a str = a ++ str ++ aReset
