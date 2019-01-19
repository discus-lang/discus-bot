
module Discus.GitHub.Event where
import qualified Data.List              as List
import qualified Data.List.Split        as List
import qualified Data.Char              as Char
import qualified Derulo                 as D


---------------------------------------------------------------------------------------------------
-- | Events from Github.
--
--   We're expecting the format of these to change over time,
--   so we represent the decoded data from least to most well understood.
--
--   The result of parsing can some received text always produces an event of some form.
--
data Event
        = EventRaw   String
        | EventPost  Headers String
        | EventJSON  Headers D.JSON

        -- | A well formed JSON event from GitHub that we don't understand.
        | EventOther
        { eventOtherHeaders     :: Headers
        , eventOtherMethod      :: String
        , eventOtherBody        :: D.JSON }

        -- | Event due to commits being pushed to a repo.
        | EventPush
        { eventPushRepoName     :: String
        , eventPushRepoBranch   :: String
        , eventPushCommits      :: [Commit] }
        deriving Show


-- | HTTP Header fields.
type Headers = [(String, String)]


-- | Info about a commit that we care about.
data Commit
        = Commit
        { commitAuthor          :: String
        , commitMessage         :: String }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Parse a HTTP request from GitHub.
parseEvent :: String -> Event
parseEvent str0
 = goRaw str0
 where
        goRaw str
         | Just (hdrs, sBody)   <- parsePost str
         = goPost hdrs sBody
         | otherwise            = EventRaw str

        goPost hdrs sBody
         | Just sType   <- lookup "content-type" hdrs
         , "application/json" <- map Char.toLower sType
         , Just json    <- D.readJSON sBody
         = goJson hdrs json
         | otherwise            = EventPost hdrs sBody

        goJson hdrs json
         | Just sAgent  <- lookup "user-agent" hdrs
         , List.isPrefixOf "github-hookshot" (map Char.toLower sAgent)
         , Just sEvent  <- lookup "x-github-event" hdrs
         = case map Char.toLower sEvent of
                "push"  -> parsePush hdrs json
                _       -> EventOther hdrs sEvent json

         | otherwise
         = EventJSON hdrs json


---------------------------------------------------------------------------------------------------
-- | Parse a Push event from Github
parsePush :: Headers -> D.JSON -> Event
parsePush hdrs json
 | D.Object fsTop               <- json
 , Just (D.String sRef)         <- lookup "ref" fsTop
 , ssParts                      <- List.splitOn "/" sRef
 , sBranch : _                  <- reverse ssParts
 , Just (D.Object fsRepo)       <- lookup "repository" fsTop
 , Just (D.String sRepoName)    <- lookup "name" fsRepo
 , Just (D.Array  osCommit)     <- lookup "commits" fsTop
 , Just commits                 <- sequence $ map parseCommit osCommit
 = EventPush sRepoName sBranch commits

 | otherwise    = EventOther hdrs "push" json


---------------------------------------------------------------------------------------------------
-- | Parse a commit object.
parseCommit :: D.JSON -> Maybe Commit
parseCommit json
 | D.Object fsCommit                    <- json
 , Just (D.Object fsAuthor)             <- lookup "author" fsCommit
 , Just (D.String sAuthorName)          <- lookup "name" fsAuthor
 , Just (D.String sMessage)             <- lookup "message" fsCommit
 , sMessageHead : _ <- lines sMessage
 = Just $ Commit sAuthorName sMessageHead

 | otherwise    = Nothing


---------------------------------------------------------------------------------------------------
-- | Parse a HTTP Post request.
parsePost :: String -> Maybe ([(String, String)], String)
parsePost ss
 = goMethod $ lines ss
 where
        goMethod []             = Nothing
        goMethod (l : ls)
         | List.isPrefixOf "POST " l
         = goPost [] ls
         | otherwise            = Nothing

        goPost _ []             = Nothing
        goPost hdrs (l : ls)
         | "\r" <- l            = goBody hdrs ls

         | (tag, ':' : ' ' : cs) <- break (== ':') l
         , '\r' : cs'           <- reverse cs
         = goPost (hdrs ++ [(map Char.toLower tag, reverse cs')]) ls

         | otherwise            = Nothing

        goBody hdrs ls
         = Just (hdrs, unlines ls)
