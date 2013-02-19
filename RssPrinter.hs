module RssPrinter where

import Data.Time.Clock
import Data.Time.Format
import Text.XML.HaXml
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Util
import System.Locale

import qualified PageParser as P

showGen g = verbatim $ g $ docContent (posInNewCxt "" Nothing) (xmlParse "" "")

title t = mkElem "title" [literal t]
pubDate date = mkElem "pubDate" [literal $ formatTime defaultTimeLocale datefmt date]
        -- Wed, 30 Jan 2009 12:00:00 +0000
  where datefmt = "%a, %d %b %Y %H:%M:%S %z"
categories = map (\cat -> mkElem "category" [literal cat])
entrytext e = mkElem "content:encoded" [literal e]

comment = mkElem "wp:comment" . mkComment
  where mkComment c = [ mkElem "wp:comment_author" [literal (P.commenter c)]
                      , mkElem "wp:comment_author_url" [literal (P.website c)]
                      , mkElem "wp:comment_content" [literal (P.comment c)]
                      ]

item (entry,fb) = mkElem "item" item_rss
  where item_rss = [ pubDate (P.time entry)
                   , title (P.title entry)
                   , entrytext (P.entry entry)
                   ] ++ categories (P.cats entry)
                     ++ map comment fb

toRss :: P.Blog -> String
toRss blog = showGen $ mkElem "rss" [mkElem "channel" $ map item $ P.posts blog]

