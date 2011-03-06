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
-- I'm not sure if the Percent stuff works on a non-Xinerama setup.
--
-- You must compile dzen with support for Xinerama if you want to define 
-- a dzen for a specific screen (it uses the -xs option).
--
-------------------------------------------------------------------------------

module Dzen (
    -- * Usage
    -- $usage
      DzenConf(..)
    , DzenWidth(..)
    , TextAlign(..)
    , ScreenNum
    -- * Spawning
    , spawnDzen
    , spawnToDzen
    -- * API
    , dzen
    , dzenArgs
    -- * Example Dzens
    , defaultDzen
    , nothingDzen
    ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile, forkProcess, createSession)

import Graphics.X11.Xlib     (openDisplay)
import Graphics.X11.Xinerama (xineramaQueryScreens, xsi_width)

-- $usage
--
-- To use, copy the source code for this module into
-- @~\/.xmonad\/lib\/Dzen.hs@ and add the following to your
-- @~\/.xmonad\/xmonad.hs@:
--
-- >
-- > import Dzen
-- > import XMonad.Hooks.DynamicLog hiding (dzen)
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
--   empty for the same purpose.
data DzenConf = DzenConf 
    { x_position :: Maybe DzenWidth -- ^ x position
    , y_position :: Maybe Int       -- ^ y position
    , screen     :: Maybe ScreenNum -- ^ screen number (0 based, Nothing implies 0)
    , width      :: Maybe DzenWidth -- ^ width
    , height     :: Maybe Int       -- ^ line height
    , alignment  :: Maybe TextAlign -- ^ alignment of title window
    , font       :: Maybe String    -- ^ font
    , fg_color   :: Maybe String    -- ^ foreground color
    , bg_color   :: Maybe String    -- ^ background color
    , exec       :: [String]        -- ^ exec flags, ex: [\"onstart=lower\", ...]
    , addargs    :: [String]        -- ^ additional arguments, ex: [\"-p\", \"-tw\", \"5\"]
    }

-- | Xinerama screen number
type ScreenNum = Int

-- | Define a width and x argument as straight pixel, or percentages
data DzenWidth = Pixels Int | Percent Double

-- | A simple data type for the text alignment of the dzen bar
data TextAlign = LeftAlign | RightAlign | Centered

-- | 'show' 'TextAlign' makes it suitable for use as a dzen argument
instance Show TextAlign where
    show LeftAlign  = "l"
    show RightAlign = "r"
    show Centered   = "c"

-- | Spawn a dzen by configuraion and return its handle, behaves 
--   exactly as spawnPipe but takes a 'DzenConf' as argument.
spawnDzen :: DzenConf -> IO Handle
spawnDzen d = do
    (rd, wr) <- createPipe
    setFdOption wr CloseOnExec True
    h <- fdToHandle wr
    hSetBuffering h LineBuffering
    forkProcess $ do
        dz <- dzen d
        createSession
        dupTo rd stdInput
        executeFile "/bin/sh" False ["-c", dz] Nothing
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
        dz <- dzen d
        createSession
        dupTo rd stdInput
        executeFile "/bin/sh" False ["-c", dz] Nothing

    -- the input process
    forkProcess $ do
        createSession
        dupTo wr stdOutput
        executeFile "/bin/sh" False ["-c", x] Nothing

    return ()

-- | The full computed dzen command for a 'DzenConf'
dzen :: DzenConf -> IO String
dzen d = return . unwords . (:) "dzen2" =<< dzenArgs d

-- | The computed list of arguments for a 'DzenConf'
dzenArgs :: DzenConf -> IO [String]
dzenArgs d = do
    x <- mkWidth (screen d) (x_position d)
    w <- mkWidth (screen d) (width d)

    let s = fmap (+1) $ screen d -- the -xs arg is 1 index

    return $ addOpt ("-fn", fmap quote $ font       d)
          ++ addOpt ("-fg", fmap quote $ fg_color   d)
          ++ addOpt ("-bg", fmap quote $ bg_color   d)
          ++ addOpt ("-ta", fmap show  $ alignment  d)
          ++ addOpt ("-y" , fmap show  $ y_position d)
          ++ addOpt ("-h" , fmap show  $ height     d)
          ++ addOpt ("-xs", fmap show  $ s           )
          ++ addOpt ("-x" , fmap show  $ x           )
          ++ addOpt ("-w" , fmap show  $ w           )
          ++ addExec (exec d)
          ++ addargs d

    where
        quote = ("'"++) . (++"'")

        addOpt (_  , Nothing ) = []
        addOpt (opt, Just arg) = [opt, arg]

        addExec [] = []
        addExec es = ["-e", quote $ intercalate ";" es]

-- | Return the width of ScreenNum s (0 index), return 0 if screen  
--   doesn't exist
screenWidth :: ScreenNum -> IO Double
screenWidth s = do
    dsp <- openDisplay ""
    mss <- xineramaQueryScreens dsp
    return $ case mss of
        Nothing -> 0
        Just [] -> 0
        Just ss -> if s >= 0 && s < length ss -- prevent bad index
            then fromIntegral . xsi_width $ ss !! s else 0

-- | Given a 'DzenWidth', give back the Maybe Int that can be used as an 
--   argument for dzen2 -w or -x.
mkWidth :: Maybe ScreenNum -> Maybe DzenWidth -> IO (Maybe Int)
mkWidth Nothing w                   = mkWidth (Just 0) w
mkWidth _       Nothing             = return Nothing
mkWidth (Just s) (Just (Pixels x))  = return $ Just x
mkWidth (Just s) (Just (Percent c)) = return . go =<< screenWidth s
    where
        go 0  = Nothing
        go sw = Just . round $ (c/100) * sw

-- | A default dzen configuration. Similar colors to default decorations 
--   and prompts in other modules. Added options @-p@ and @-e 
--   \'onstart=lower\'@ are useful for dzen-as-statusbar.
defaultDzen :: DzenConf
defaultDzen = nothingDzen
    { alignment   = Just LeftAlign
    , font        = Just "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
    , fg_color    = Just "#FFFFFF"
    , bg_color    = Just "#333333"
    , exec        = ["onstart=lower"]
    , addargs     = ["-p"]
    }

-- | A dzen with all options as 'Nothing' or the empty list.
nothingDzen :: DzenConf
nothingDzen = DzenConf
    { x_position = Nothing
    , y_position = Nothing
    , screen     = Nothing
    , width      = Nothing
    , height     = Nothing
    , alignment  = Nothing
    , font       = Nothing
    , fg_color   = Nothing
    , bg_color   = Nothing
    , exec       = []
    , addargs    = []
    }
