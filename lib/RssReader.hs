-------------------------------------------------------------------------------
-- |
-- Module      :  RssReader
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Aggregate Rss Feeds and print /Ticker Text/ to a handle. Requires
-- http and tagsoup packages.
--
-------------------------------------------------------------------------------

module RssReader (
    -- * Usage
    -- $usage
    ReaderConf(..),
    defaultReaderConf,
    spawnReader, loopForever, printToHandle
    ) where

import Control.Concurrent       (threadDelay)
import Control.Monad            (liftM, forever)
import Data.Char                (chr)
import Data.List                (isPrefixOf)
import Data.Maybe               (catMaybes, maybeToList)
import Network.HTTP             (getResponseBody, simpleHTTP, getRequest)
import System.Directory         (getHomeDirectory)
import System.FilePath          ((</>))
import System.IO                (Handle, hPutStrLn)
import System.Posix             (forkProcess)
import System.Posix.Types       (ProcessID)
import Text.HTML.TagSoup        (Tag(..), Attribute, parseTags, sections)
import Text.HTML.TagSoup.Entity (lookupEntity, htmlEntities)

-- $usage
--
-- To use, add feed urls (one per line, no whitespace) to a file at
-- @~\/.rssfeeds@ then copy the source code for this module into
-- @~\/.xmonad\/lib\/RssReader.hs@, then add the following to your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import RssReader
-- > import XMonad.Util.Run (spawnPipe)
-- >
-- > main :: IO()
-- > main = do
-- >     spawnPipe "dzen2 -p -tw 100" >>= spawnReader defaultReaderConf
-- >
-- >     xmonad $ defaultConfig
-- >         { ...
-- >         , ...
-- >         }
-- >
--

type URL  = String -- ^ Feed url
type Date = String -- ^ Publish date

-- | Defines an rss feed item
data RssItem = RssItem
    { pubDate     :: Date
    , title       :: String
    , link        :: URL
    , description :: String
    } deriving Show

-- | The main configuration
data ReaderConf = ReaderConf
    { sources     :: FilePath         -- ^ path to a file containing one feed url per line, @~\/.rssfeeds@ by default
    , limit       :: Int              -- ^ number of items to show per feed with 0 meaning unlimited
    , clickable   :: Bool             -- ^ make the title clickable in dzen, set to 'False' if you're not using dzen
    , titleFormat :: String -> String -- ^ this function will be applied to the item title when it's printed
    , descrFormat :: String -> String -- ^ this function will be applied to the item description /before/ it's made into ticker text
    , tickerWidth :: Int              -- ^ this determines the number of characters the ticker text description will take up
    , tickerSpeed :: Int              -- ^ number of microseconds between updates of the ticker text
    }

-- | A default reader config
defaultReaderConf :: ReaderConf
defaultReaderConf = ReaderConf
    { sources     = "~/.rssfeeds"
    , limit       = 0
    , clickable   = True
    , titleFormat = id
    , descrFormat = id
    , tickerWidth = 150
    , tickerSpeed = 250000
    }

-- | Spawn a reader given a configuration and handle to print to
spawnReader :: ReaderConf -> Handle -> IO ()
spawnReader conf h = loopForever (printToHandle conf h) >> return ()

-- | Forks and runs an IO action repeatedly forever in the background
--   using Control.Monad.forever
loopForever :: IO () -> IO ProcessID
loopForever = forkProcess . forever

-- | Pushes the generated text out to a handle
printToHandle :: ReaderConf -> Handle -> IO ()
printToHandle conf h = hPutStrSleep h (tickerSpeed conf) . formatOutputLines conf =<< readFeeds conf

-- | hPuts each string in the list, waiting s microseconds between
hPutStrSleep :: Handle -> Int -> [String] -> IO ()
hPutStrSleep _ _ []     = return ()
hPutStrSleep h s (x:xs) = hPutStrLn h x >> threadDelay s >> hPutStrSleep h s xs

-- | Returns a flat list of rssItems from all sources
readFeeds :: ReaderConf -> IO [RssItem]
readFeeds conf = liftM concat . mapM (readFeed conf) . lines =<< readSources (sources conf)

    where
        -- translate ~
        readSources ('~':'/':rest) = getHomeDirectory >>= \d -> readFile $ d </> rest
        readSources file           = readFile file

-- | Read one feed into a list of rssitems
readFeed :: ReaderConf -> URL -> IO [RssItem]
readFeed conf url = do
    c <- getHTTP url
    let items = getChildren "item" [] $ parseTags c
    return . limitItems (limit conf) . catMaybes $ map readItem items

    where
        -- 0 means unlimited
        limitItems 0 = id
        limitItems n = take n

        -- read one item tag
        readItem i = let title       = fix $ getText "title"       [] i
                         link        = fix $ getText "link"        [] i
                         pubDate     = fix $ getText "pubDate"     [] i
                         description = fix $ getText "description" [] i
                     -- required parts to be usable
                     in if all (/= "") [title, link, description]
                            then Just $ RssItem pubDate title link description
                            else Nothing

        fix = fixWhiteSpace . onlyPrintables . replaceEntities . stripBetween '<' '>'

-- | Get the html content of a given url
getHTTP :: URL -> IO String
getHTTP url = getResponseBody =<< simpleHTTP (getRequest url)

-- | Formats rssitems for printing to dzen
formatOutputLines :: ReaderConf -> [RssItem] -> [String]
formatOutputLines _    [] = ["Listed feeds contain no items :("]
formatOutputLines conf rs = concatMap formatOutputLine rs

    where
        -- per rssItem
        formatOutputLine r = zipWith (+-+) (repeat . ca (link r) $ tf r) (tickerText tw $ td r)

        -- config values
        tw = tickerWidth conf
        ts = tickerSpeed conf
        tf = titleFormat conf . title
        td = descrFormat conf . description

        -- clickable area
        ca l s = if clickable conf then "^ca(1, $BROWSER '" ++ l ++ "')" ++ s ++ "^ca()" else s

-- | Replace all html entities with their referenced values
replaceEntities :: String -> String
replaceEntities = replaceAll (namedList ++ numericList) maybeLookup

    where
        -- add the extra characters so the replaceAll works
        namedList   = map ((("&" ++) . (++";")) . fst) htmlEntities
        numericList = map ((("&#"++) . (++";")) . show) [1..255]

        maybeLookup :: String -> String
        maybeLookup = maybeToList . lookupEntity . dropAmp . reverse . dropSemi . reverse

            where
                -- strip characters so the lookup works
                dropAmp ('&':xs) = xs
                dropAmp xs       = xs

                dropSemi (';':xs) = xs
                dropSemi xs       = xs

-- Returns list of child tags given a parent node
getChildren :: String -> [Attribute String] -> [Tag String]-> [[Tag String]]
getChildren t a = map (drop 1 . takeWhile (/= TagClose t)) . sections (== TagOpen t a)

-- Returns all innertext of a given tag
getText :: String -> [Attribute String] -> [Tag String] -> String
getText t a = concatMap getText' . takeWhile (/= TagClose t) . drop 1 . dropWhile (/= TagOpen t a)

    where

        getText' (TagText s) = s
        getText' _           = []

-- Some useful formatting utils {{{
--
-- | an infix operator that concatenates two strings with a space in
--   between. this is useful with for example:
--
-- > fields = ["Name:", "Age:"]
-- > values = ["Pat", "25"]
-- > data   = zipWith (+-+) fields values
--
(+-+) :: String -> String -> String
s1 +-+ s2 = s1 ++ " " ++ s2
infixr 5 +-+

-- | removes tab, newline, and return (replacing them with space), then
--   compresses duplicate spaces, then trims spaces from the front and
--   back
fixWhiteSpace :: String -> String
fixWhiteSpace = trim . compressSpace

-- | strip all but printable characters
onlyPrintables :: String -> String
onlyPrintables [] = []
onlyPrintables (x:xs) =
    if x `elem` [' '..'~']
        then x : onlyPrintables xs
        else     onlyPrintables xs

-- | trim any spaces from the front and back of the input string
trim :: String -> String
trim = f . f
    where
        f = reverse . dropSpace

dropSpace :: String -> String
dropSpace = dropWhile (== ' ')

-- | compresses 1 or more spaces into a single space
compressSpace :: String -> String
compressSpace []       = []
compressSpace (' ':xs) = ' ' : compressSpace (dropSpace xs)
compressSpace (x:xs)   = x   : compressSpace xs

-- | remove any text appearing between c1 and c2 (inclusive)
stripBetween :: Char -> Char -> String -> String
stripBetween _  _   []     = []
stripBetween c1 c2  (x:xs) = 
    if x == c1
        then stripBetween c1 c2 . drop 1 $ dropWhile (/= c2) xs
        else x : stripBetween c1 c2 xs

-- | given a width and (possibly infinite) list, it will work out a list
--   of shorter lists that, when viewed one by one successively, appear
--   as the original list rolling by right to left (like a stock ticker)
tickerText :: Int -> [a] -> [[a]]
tickerText _ []     = []
tickerText w (x:xs) = (x : take (w - 1) xs) : tickerText w xs

-- | works with [a], but designed for strings. it's best explained by an
--   example:
--
-- > scrubFile = do
-- >   dirty <- readFile "myfile"
-- >   putStrLn $ replaceAll ["my@email.com","mypassword"] (\_ -> "") dirty
--
replaceAll :: (Eq a) 
           => [[a]]        -- ^ the list of all /from/s that need replacing
           -> ([a] -> [a]) -- ^ a function, how to get the /to/ given any /from/
           -> [a]          -- ^ the input string
           -> [a]          -- ^ result
replaceAll _      _ [] = []
replaceAll []     _ s  = s
replaceAll (x:xs) f s  = replace x (f x) $ replaceAll xs f s

-- | taken from http://bluebones.net/2007/01/replace-in-haskell/
replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _    _  [] = []
replace _    [] s  = s
replace []   _  s  = s
replace from to s@(x:xs) =
    if from `isPrefixOf` s
        then to ++ replace from to (drop (length from) s)
        else x : replace from to xs

-- }}}
