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
    , dzen
    , spawnDzen
    , spawnToDzen
    ) where

import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, createSession)

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
-- Where someDzen is a 'DzenConf'
--

-- | A data type to fully describe a spawnable dzen bar, take a look at
--   @\/usr\/share\/doc\/dzen2\/README@ to see what input is acceptable
data DzenConf = DzenConf 
    { x_position :: Int       -- ^ x position
    , y_position :: Int       -- ^ y position
    , width      :: Int       -- ^ width
    , height     :: Int       -- ^ line height
    , alignment  :: TextAlign -- ^ alignment of title window
    , font       :: String    -- ^ font
    , fg_color   :: String    -- ^ foreground color
    , bg_color   :: String    -- ^ background color
    , exec       :: String    -- ^ exec flags, ex: \"onstart=lower\"
    , addargs    :: [String]  -- ^ additional arguments, ex: [\"-p\", \"-tw\", \"5\"]
    }

-- | A simple data type for the text alignment of the dzen bar
data TextAlign = LeftAlign | RightAlign | Centered

-- | 'show' 'TextAlign' makes it suitable for use as a dzen argument
instance Show TextAlign where
    show LeftAlign  = "l"
    show RightAlign = "r"
    show Centered   = "c"

-- | Spawn a dzen by configuraion and return it's handle, behaves 
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
        executeFile "/bin/sh" False ["-c", dzen d] Nothing

    -- the input process
    forkProcess $ do
        createSession
        dupTo wr stdOutput
        executeFile "/bin/sh" False ["-c", x] Nothing

    return ()

-- | Build the right list of arguments for a dzen
dzenArgs :: DzenConf -> [String]
dzenArgs d = [ "-fn", quote $ font       d
             , "-fg", quote $ fg_color   d
             , "-bg", quote $ bg_color   d
             , "-ta", show  $ alignment  d
             , "-x" , show  $ x_position d
             , "-y" , show  $ y_position d
             , "-w" , show  $ width      d
             , "-h" , show  $ height     d
             ] ++ addexec (exec d) ++ addargs d

    where
        quote = ("'"++) . (++ "'")
        addexec s = if null s then [] else ["-e", quote s]

-- | Return the full string executable for the given configuration
dzen :: DzenConf -> String
dzen = unwords . (:) "dzen2" . dzenArgs

-- | A default dzen configuration. Similar colors to default decorations 
--   and prompts in other modules.
defaultDzen :: DzenConf
defaultDzen = DzenConf
    { x_position  = 0
    , y_position  = 0
    , width       = 1280
    , height      = 17
    , alignment   = LeftAlign
    , font        = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , fg_color    = "#FFFFFF"
    , bg_color    = "#333333"
    , exec        = "onstart=lower"
    , addargs     = ["-p"]
    }
