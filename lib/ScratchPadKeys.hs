-------------------------------------------------------------------------------
-- |
-- Module      :  ScratchPadKeys
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module very similar to X.U.NamedScratchpad but with my own datatype
-- which forgoes the name and adds a keybind. A list of keybinds is
-- generated in /EZConfig/ notation for the scratchpads in the list and
-- an overall managehook is generated exactly as in the NamedScratchpad
-- module.
--
-------------------------------------------------------------------------------

module ScratchPadKeys (
    -- * Usage
    -- $usage
    ScratchPad(..),
    manageScratchPads,
    scratchPadKeys,
    spawnScratchpad,
    -- * Scrachpads
    -- $scratchpads
    scratchPadList,
    scratchMixer, scratchMusic,
    scratchTerminal, scratchTop,
    -- * ManageHooks
    -- $managehooks
    centerScreen, bottomEdge
    ) where

import XMonad
import XMonad.Actions.DynamicWorkspaces  (addHiddenWorkspace)
import XMonad.ManageHook                 (composeAll)
import XMonad.Hooks.ManageHelpers        (doRectFloat)
import Control.Arrow                     ((&&&))
import Control.Monad                     (filterM, when)

import qualified XMonad.StackSet as W

-- $usage
--
-- To use, you'll need to have myManageHook and myKeys defined. myKeys
-- will need to be using /EZConfig/ notation. Then, add the source code
-- for this module to @~\/.xmonad\/lib\/ScratchPadKeys.hs@ and add the
-- following to your @~\/.xmonad\/xmonad.hs@:
--
-- > import ScratchPadKeys
-- > import XMonad.Util.EZConfig (additionalKeysP)
-- >
-- > main :: IO ()
-- > main = xmonad $ defaultConfig
-- >    { ...
-- >    , manageHook = myManageHook
-- >    , ...
-- >    } `additionalKeysP` myKeys
-- >
-- > myManageHook :: ManageHook
-- > myManageHook = [ ...
-- >                , ...
-- >                ] <+> manageScratchPads scratchPadList
-- >
-- > myKeys :: [(String, X())]
-- > myKeys = [ ...
-- >          , ...
-- >          ] ++ scratchPadKeys scratchPadList
--
-- You can define your own scratchpads and scratchpad list or use the
-- one(s) provided by this module.
--

-- | A single scratchpad definition
data ScratchPad = ScratchPad
    { keybind :: String     -- ^ The keybind to use in EZConfig notation, ex: \"M4-t\"
    , cmd     :: String     -- ^ The command to spawn the application
    , query   :: Query Bool -- ^ The query to find it once it's spawned
    , hook    :: ManageHook -- ^ the way to manage it when it's visible
    }

-- | Produce a managehook to manage all scratchpads in the passed list
manageScratchPads :: [ScratchPad] -> ManageHook
manageScratchPads = composeAll . fmap (\c -> query c --> hook c)

-- | Produce a list of keybinds in /EZConfig/ notation for all
--   scratchpads in the passed list
scratchPadKeys :: [ScratchPad] -> [(String, X())]
scratchPadKeys = fmap (keybind &&& spawnScratchpad)

-- | Summon, banish, or spawn a single 'ScratchPad'
spawnScratchpad :: ScratchPad -> X ()
spawnScratchpad sp = withWindowSet $ \s -> do
    filterCurrent <- filterM (runQuery $ query sp) . 
        maybe [] W.integrate . W.stack . W.workspace $ W.current s

    case filterCurrent of
        (x:_) -> do
            when 
                (null . filter ((== "NSP") . W.tag) $ W.workspaces s) $ 
                addHiddenWorkspace "NSP"

            windows $ W.shiftWin "NSP" x
        [] -> do
            filterAll <- filterM (runQuery $ query sp) $ W.allWindows s

            case filterAll of
                (x:_) -> windows $ W.shiftWin (W.currentTag s) x
                []    -> spawn $ cmd sp

-- $scratchpads
--
-- The scratchpads I use; defined here both for my use and as an example
-- of what can be done.
--

-- | All here-defined scratchpads in a list
scratchPadList :: [ScratchPad]
scratchPadList = [scratchMixer, scratchMusic, scratchTop, scratchTerminal]

-- | ossxmix center screen
scratchMixer :: ScratchPad
scratchMixer = ScratchPad
    { keybind  = "M4-x"
    , cmd      = "ossxmix"
    , query    = className =? "Ossxmix"
    , hook     = centerScreen 0.65
    }

-- | ncmpcpp center screen
scratchMusic :: ScratchPad
scratchMusic = ScratchPad
    { keybind  = "M4-n"
    , cmd      = "urxvtc -name sp-ncmpcpp -e ncmpcpp"
    , query    = resource =? "sp-ncmpcpp"
    , hook     = centerScreen 0.65
    }

-- | htop center screen
scratchTop :: ScratchPad
scratchTop = ScratchPad
    { keybind = "M4-h"
    , cmd     = "urxvtc -name sp-htop -e htop"
    , query   = resource =? "sp-htop"
    , hook    = centerScreen 0.65
    }

-- | A terminal along the bottom edge
scratchTerminal :: ScratchPad
scratchTerminal = ScratchPad
    { keybind  = "M4-t"
    , cmd      = "urxvtc -name sp-term"
    , query    = resource =? "sp-term"
    , hook     = bottomEdge 0.15
    }

-- $managehooks
--
-- Some convenient managehooks that I use in my scratchpad definitions.
--

-- | Floating, center screen with a given height
centerScreen :: Rational -> ManageHook
centerScreen h = doRectFloat $ W.RationalRect ((1 - h)/2) ((1 - h)/2) h h

-- | Floating, bottom edge with a given height
bottomEdge :: Rational -> ManageHook
bottomEdge h = doRectFloat $ W.RationalRect 0 (1 - h) 1 h
