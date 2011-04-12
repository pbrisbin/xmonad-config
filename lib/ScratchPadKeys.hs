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
-- The command record is also an X () and not a String, this allows me
-- to pre define some scratchpads here which will auto-magically use
-- your user-defined terminal.
--
-------------------------------------------------------------------------------

module ScratchPadKeys
    ( -- * Usage
      -- $usage
      ScratchPad(..)
    , manageScratchPads
    , scratchPadKeys
    , spawnScratchpad
    -- * Scrachpads
    -- $scratchpads
    , scratchPadList
    , scratchTerminal
    , scratchMixer
    , scratchMail
    , scratchMusic
    , scratchTop
    -- * ManageHooks
    -- $managehooks
    , centerScreen
    , bottomEdge
    ) where

import XMonad
import XMonad.Actions.DynamicWorkspaces  (addHiddenWorkspace)
import XMonad.Hooks.ManageHelpers        (doRectFloat)
import Control.Arrow                     ((&&&))
import Control.Monad                     (filterM, unless)

import Utils (runInTerminal)

import qualified XMonad.StackSet as W

-- $usage
--
-- To use, you'll need to have myManageHook and myKeys defined. myKeys
-- will need to be using /EZConfig/ notation. Then, add the source code
-- for this module to @~\/.xmonad\/lib\/ScratchPadKeys.hs@ and add the
-- following to your @~\/.xmonad\/xmonad.hs@:
--
-- Note: You will also need @'Utils.runInTerminal'@ which can be found 
-- in @lib\/Utils.hs@.
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
    , cmd     :: X ()       -- ^ The X action to take ex: spawn \"myapp\"
    , query   :: Query Bool -- ^ The query to find it once it's spawned
    , hook    :: ManageHook -- ^ the way to manage it when it's visible
    }

-- | Produce a managehook to manage all scratchpads in the passed list
manageScratchPads :: [ScratchPad] -> ManageHook
manageScratchPads = composeAll . fmap (\c -> query c --> hook c)

-- | Produce a list of keybinds in /EZConfig/ notation for all
--   scratchpads in the passed list
scratchPadKeys :: [ScratchPad] -> [(String, X ())]
scratchPadKeys = fmap (keybind &&& spawnScratchpad)

-- | Summon, banish, or spawn a single 'ScratchPad'
spawnScratchpad :: ScratchPad -> X ()
spawnScratchpad sp = withWindowSet $ \s -> do
    filterCurrent <- filterM (runQuery $ query sp) . 
        maybe [] W.integrate . W.stack . W.workspace $ W.current s

    case filterCurrent of
        (x:_) -> do
            unless 
                (any ((== "NSP") . W.tag) $ W.workspaces s) $ 
                addHiddenWorkspace "NSP"

            windows $ W.shiftWin "NSP" x
        [] -> do
            filterAll <- filterM (runQuery $ query sp) $ W.allWindows s

            case filterAll of
                (x:_) -> windows $ W.shiftWin (W.currentTag s) x
                []    -> cmd sp

-- $scratchpads
--
-- The scratchpads I use; defined here both for my use and as an example
-- of what can be done.
--

-- | All here-defined scratchpads in a list
scratchPadList :: [ScratchPad]
scratchPadList = [scratchMixer, scratchMail, scratchMusic, scratchTop, scratchTerminal]

-- | A terminal along the bottom edge
scratchTerminal :: ScratchPad
scratchTerminal = ScratchPad
    { keybind  = "M4-t"
    , cmd      = runInTerminal ["-name", "sp-term"]
    , query    = resource =? "sp-term"
    , hook     = bottomEdge 0.15
    }

-- | ossxmix center screen
scratchMixer :: ScratchPad
scratchMixer = ScratchPad
    { keybind  = "M4-x"
    , cmd      = spawn "ossxmix"
    , query    = className =? "Ossxmix"
    , hook     = centerScreen 0.65
    }

-- | mutt center screen
scratchMail :: ScratchPad
scratchMail = mkTermSP "mutt" "M4" $ centerScreen 0.65

-- | ncmpcpp center screen
scratchMusic :: ScratchPad
scratchMusic = mkTermSP "ncmpcpp" "M4" $ centerScreen 0.65

-- | htop center screen
scratchTop :: ScratchPad
scratchTop = mkTermSP "htop" "M4" $ centerScreen 0.65

-- | Makes an in-term scratchpad given executable, modifier, and hook. 
--   Uses modifier and the first letter of the executable as the 
--   keybind. Your terminal must suppor @-name@ and @-e@ and you must 
--   ensure unique keybinds result.
mkTermSP :: String -- ^ executable
         -> String -- ^ modifier, ex: \"M\", \"M4-C\", etc
         -> ManageHook -> ScratchPad
mkTermSP x m h = ScratchPad
    { keybind = m ++ "-" ++ take 1 x
    , cmd     = runInTerminal ["-name", "sp-" ++ x, "-e", x]
    , query   = resource =? ("sp-" ++ x)
    , hook    = h
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
