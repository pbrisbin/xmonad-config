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
    DzenConf(..), TextAlign(..),
    defaultDzen, 
    dzenArgs, spawnDzen
    ) where

import System.IO       (Handle)
import XMonad.Util.Run (spawnPipe)

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
-- >     d <- spawnDzen defaultDzen
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
-- If you just want to spawn a dzen and don't care about its handle you
-- can use the following:
--
-- > spawn $ show defaultDzen
--

-- | A simple data type for the text alignment of the dzen bar
data TextAlign = LeftAlign | RightAlign | Centered

-- | 'show' 'TextAlign' makes it suitable for use as a dzen argument
instance Show TextAlign where
    show LeftAlign  = "l"
    show RightAlign = "r"
    show Centered   = "c"

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

-- | 'show' 'DzenConf' will give you a proper executable
instance Show DzenConf where
    show d = unwords $ "dzen2" : dzenArgs d

-- | Build a list of args based on a 'DzenConf'
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
        -- only add exec if flags are defined
        addexec s = if null s then [] else ["-e", quote s]
        quote = ("'"++) . (++ "'")

-- | A wrapper for 'spawnPipe' to spawn a 'DzenConf' and return its handle
spawnDzen :: DzenConf -> IO Handle
spawnDzen = spawnPipe . show

-- | A default dzen bar, uses the same colors/font as defaultXPConfig
--   from XMonad.Prompt. Maybe some day we'll have Dzen.Themes too...
defaultDzen :: DzenConf
defaultDzen = DzenConf
    { x_position  = 0
    , y_position  = 0
    , width       = 500
    , height      = 17
    , alignment   = LeftAlign
    , font        = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , fg_color    = "#FFFFFF"
    , bg_color    = "#333333"
    , exec        = "onstart=lower"
    , addargs     = ["-p"]
    }

