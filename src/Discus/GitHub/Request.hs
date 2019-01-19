
module Discus.Github.Request where
import qualified Data.List      as List
import qualified Data.Char      as Char
import qualified Derulo         as D


-- | Requests from Github
data Request
        = RequestRaw   String
        | RequestPost Headers String
        | RequestJSON Headers D.JSON
        deriving Show

type Headers = [(String, String)]


-- | Parse a HTTP request from GitHub.
parseRequest :: String -> Request
parseRequest str0
 = goRaw str0
 where
        goRaw str
         | Just (hdrs, sBody)   <- parsePost str
         = goPost hdrs sBody
         | otherwise            = RequestRaw str

        goPost hdrs sBody
         | Just sType   <- lookup "content-type" hdrs
         , Just json    <- D.readJSON sBody
         = goJson hdrs json
         | otherwise            = RequestPost hdrs sBody

        goJson hdrs json
         = RequestJSON hdrs json

{-
 , Just sAgent   <- lookup "user-agent" hdrs
 , List.isPrefixOf "github-hookshot" sAgent
-}

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
