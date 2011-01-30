-------------------------------------------------------------------------------
-- |
-- Module      :  Dzen
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides data types and functions to easily define and spawn dzen
-- bars.
--
-------------------------------------------------------------------------------

module Dzen (
    -- * Usage
    -- $usage
      DzenConf(..)
    , TextAlign(..)
    , defaultDzen
    , spawnDzen
    , spawnToDzen
    -- * API
    , dzen
    , dzenArgs
    ) where

import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, createSession)
import Data.List (intercalate)

-- $usage
--
-- To use, copy the source code for this module into
-- @~\/.xmonad\/lib\/Dzen.hs@ and add the following to your
-- @~\/.xmonad\/xmonad.hs@:
--
-- >
-- > import Dzen
-- > import XMonad.Hooks.DynamicLog
-- >
-- > main :: IO ()
-- > main = do
-- >     d <- spawnDzen someDzen
-- >
-- >     xmonad $ defaultConfig
-- >         { ...
-- >         , logHook = myLogHook d
-- >         }
-- >
-- > myLogHook h = dynamicLogWithPP $ defaultPP
-- >     { ...
-- >     , ppOutput = hPutStrLn h
-- >     }
--
-- If you want to feed some other process into a dzen you can use the 
-- following:
--
-- > spawnToDzen "conky" someDzen
--
-- Where someDzen is a 'DzenConf' (see 'defaultDzen' for an example).
--

-- | A data type to fully describe a spawnable dzen bar, take a look at
--   @\/usr\/share\/doc\/dzen2\/README@ to see what input is acceptable. 
--   Options are wrapped in 'Just', so using 'Nothing' will not add that 
--   option to the @dzen2@ executable. @exec@ and @addargs@ can be 
--   empty.
data DzenConf = DzenConf 
    { x_position :: Maybe Int       -- ^ x position
    , y_position :: Maybe Int       -- ^ y position
    , width      :: Maybe Int       -- ^ width
    , height     :: Maybe Int       -- ^ line height
    , alignment  :: Maybe TextAlign -- ^ alignment of title window
    , font       :: Maybe String    -- ^ font
    , fg_color   :: Maybe String    -- ^ foreground color
    , bg_color   :: Maybe String    -- ^ background color
    , exec       :: [String]        -- ^ exec flags, ex: [\"onstart=lower\", ...]
    , addargs    :: [String]        -- ^ additional arguments, ex: [\"-p\", \"-tw\", \"5\"]
    }

-- | A simple data type for the text alignment of the dzen bar
data TextAlign = LeftAlign | RightAlign | Centered

-- | 'show' 'TextAlign' makes it suitable for use as a dzen argument
instance Show TextAlign where
    show LeftAlign  = "l"
    show RightAlign = "r"
    show Centered   = "c"

-- | Spawn a dzen by configuraion and return its handle, behaves 
--   exactly as spawnPipe but takes a DzenConf argument.
spawnDzen :: DzenConf -> IO Handle
spawnDzen d = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    forkProcess $ do
        createSession
        dupTo rd stdInput
        -- why does this not work?
        --executeFile "dzen2" True (dzenArgs d) Nothing
        executeFile "/bin/sh" False ["-c", dzen d] Nothing
    return h

-- | Spawn a process sending its stdout to the stdin of the dzen
spawnToDzen :: String -> DzenConf -> IO ()
spawnToDzen x d = do
    (rd, wr) <- createPipe
    setFdOption rd CloseOnExec True
    setFdOption wr CloseOnExec True
    hin  <- fdToHandle rd
    hout <- fdToHandle wr
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering

    -- the dzen
    forkProcess $ do
        createSession
        dupTo rd stdInput
        -- why does this not work?
        --executeFile "dzen2" True (dzenArgs d) Nothing
        executeFile "/bin/sh" False ["-c", dzen d] Nothing

    -- the input process
    forkProcess $ do
        createSession
        dupTo wr stdOutput
        executeFile "/bin/sh" False ["-c", x] Nothing

    return ()

-- | The full dzen command as a string
dzen :: DzenConf -> String
dzen = unwords . (:) "dzen2" . dzenArgs

-- | The right list of arguments for \"dzen2\"
dzenArgs :: DzenConf -> [String]
dzenArgs d =  addOpt ("-fn", fmap quote $ font       d)
           ++ addOpt ("-fg", fmap quote $ fg_color   d)
           ++ addOpt ("-bg", fmap quote $ bg_color   d)
           ++ addOpt ("-ta", fmap show  $ alignment  d)
           ++ addOpt ("-x" , fmap show  $ x_position d)
           ++ addOpt ("-y" , fmap show  $ y_position d)
           ++ addOpt ("-w" , fmap show  $ width      d)
           ++ addOpt ("-h" , fmap show  $ height     d)
           ++ addExec (exec d)
           ++ addargs d

    where
        quote = ("'"++) . (++"'")

        addOpt (_  , Nothing ) = []
        addOpt (opt, Just arg) = [opt, arg]

        addExec [] = []
        addExec es = ["-e", quote $ intercalate ";" es]

-- | A default dzen configuration. Similar colors to default decorations 
--   and prompts in other modules. Added options @-p@ and @-e 
--   \'onstart=lower\'@ are useful for dzen-as-statusbar.
defaultDzen :: DzenConf
defaultDzen = DzenConf
    { x_position  = Nothing
    , y_position  = Nothing
    , width       = Nothing
    , height      = Nothing
    , alignment   = Just LeftAlign
    , font        = Just "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , fg_color    = Just "#FFFFFF"
    , bg_color    = Just "#333333"
    , exec        = ["onstart=lower"]
    , addargs     = ["-p"]
    }
