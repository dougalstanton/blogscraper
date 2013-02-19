{-# LANGUAGE NoMonomorphismRestriction #-}
module PageParser where

import Data.Char
import Data.List (isPrefixOf, isInfixOf, tails)
import Data.Monoid
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format
import System.Locale
import Text.XML.HaXml
import Text.XML.HaXml.Parse (xmlParse)
import Text.XML.HaXml.Posn

data Entry = Entry
             { title :: String
             , time  :: UTCTime
             , cats  :: [String]
             , auth  :: String
             , entry :: String
             } deriving Show
data Feedback = Feedback
             { commenter :: String
             , email     :: String
             , website   :: String
             , comment   :: String
             } deriving Show
data Blog = Blog { posts :: [(Entry, [Feedback])] }
                 deriving Show

instance Eq Entry where
  e1 == e2 = time e1 == time e2

instance Ord Entry where
  e1 <= e2 = time e1 <= time e2

instance Monoid Blog where
  mempty        = Blog []
  mappend b1 b2 = Blog $ merge (posts b1) (posts b2)

merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = if fst x <= fst y then x:merge xs (y:ys) else y:merge (x:xs) ys

-- Utility functions

attrvalue :: String -> String -> CFilter i
attrvalue a v = attrval (N a, AttValue [Left v])

attrclass, attrid :: String -> CFilter i
attrclass = attrvalue "class"
attrid = attrvalue "id"

body :: CFilter i
body = tag "html" /> tag "body" /> tag "div" /> attrid "content" /> attrclass "post"

parseFile fname = readFile fname >>= return . parse fname
parse name content = Blog [(parseEntry doc,parseComments doc)]
  where (Document _ _ e _) = xmlParse name content
        doc = CElem e noPos
parseEntry xml = Entry
               { title = trimtxt $ body /> attrclass "title" /> tag "a" /> txt $ xml
               , time = parseEntryDate (day,month,year,timestr)
               , auth = showContentDefault "unknown" $ author xml
               , cats = map verbatim $ categories /> txt $ xml
               , entry = processUrls $ cleanup $ verbatim $ post $ xml
               }
  where date = body /> attrclass "date"
        meta = body /> attrclass "meta" /> tag "p"
        author = meta /> (tag "a" `without` iscategory) /> txt
        categories = meta /> tag "a" `with` iscategory
        post = position 0 (body /> attrclass "entry") /> elm

        day = trimtxt $ date /> attrclass "day" /> txt $ xml
        month = trimtxt $ date /> attrclass "month" /> txt $ xml
        year = trimtxt $ date /> attrclass "year" /> txt $ xml
        timestr = trimtxt $ position 1 (meta /> txt) xml

        iscategory = attrvalue "rel" "category tag"

parseComments xml = map parseFeedback $ commentlist xml
  where commentlist = body /> tag "ol" `with` attrclass "commentlist" /> tag "li"

parseFeedback xml = Feedback
                  { commenter = showContent $ cite /> (tag "a" /> txt |>| txt) $ xml
                  , email     = ""
                  , website   = fixUrl $ verbatim $ cite /> extractUrl $ xml
                  , comment   = trimtxt $ elm /> tag "p" $ xml
                  }
  where cite = elm /> tag "div" /> tag "cite"
        extractUrl = iffind "href" literal none

processUrls :: String -> String
processUrls = unwords . map f . words
  where f ('h':'r':'e':'f':'=':q:xs) = let (url,rest) = break (==q) xs
                                       in "href=" ++ q:fixUrl url ++ rest
        f ('s':'r':'c':'=':q:xs) = let (url,rest) = break (==q) xs
                                   in "src=" ++ q:fixUrl url ++ rest
        f xs = xs

fixUrl :: String -> String
fixUrl = head . dropWhile (not . valid) . tails
  where -- URLs are either empty or have only one HTTP protocol
        valid []  = True
        valid url = "http://" `isPrefixOf` url && not ("http://" `isInfixOf` tail url)

showContent = concatMap verbatim
showContentDefault msg [] = msg
showContentDefault _   xs = showContent xs

cleanup = filter (\c -> c /= '\r' && c /= '\n') . trim
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
trimtxt = trim . showContent

parseEntryDate :: (String,String,String,String) -> Data.Time.Clock.UTCTime
parseEntryDate (d,m,y,hhmm) = readTime defaultTimeLocale "%d %b %Y at %l:%M %P under" str
  where str = unwords [d,m,y,hhmm]

