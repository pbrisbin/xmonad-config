{-# OPTIONS -fno-warn-missing-signatures #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Utils
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parts of my config that are general-purpose enough to be useful 
-- outside the scope of my config.
--
-- This could be imported and used on its own without having to use \"my\" 
-- config entirely.
--
-- haddocks: <http://pbrisbin.com/xmonad/docs/Utils.html>
--
-------------------------------------------------------------------------------

module Utils 
    ( 
    -- * Config entries
      pbWorkspaces
    , pbManageHook
    , pbLayout
    , pbPP

    -- * Urgency
    , SpawnSomething(..)
    , pbUrgencyHook
    , pbUrgencyConfig

    -- * Utilities
    , matchAny
    , name
    , role
    , hideNSP
    , yeganesh
    , runInTerminal
    , spawnInScreen
    , cleanStart
    ) where

import XMonad

import XMonad.Hooks.DynamicLog      (dzenPP, PP(..), pad, dzenColor)
import XMonad.Hooks.ManageDocks     (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook     (UrgencyHook(..), UrgencyConfig(..), urgencyConfig, SuppressWhen(OnScreen))
import XMonad.Layout.LayoutHints    (layoutHints)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

import qualified XMonad.StackSet as W

-- | The setup I like: a main, web and chat plus the rest numbered.
pbWorkspaces :: [WorkspaceId]
pbWorkspaces = ["1-main","2-web","3-chat"] ++ map show [4..9 :: Int]

-- | Default plus docks, dialogs and smarter full screening.
pbManageHook :: ManageHook
pbManageHook = composeAll $ concat
    [ [ manageDocks                                       ]
    , [ manageHook defaultConfig                          ]
    , [ isDialog      --> doCenterFloat                   ]
    , [ isFullscreen  --> doF W.focusDown <+> doFullFloat ]
    ]

-- | Match a string against any one of a window's class, title, name or 
--   role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- | Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_NAME"

-- | Match against @WM_ROLE@.
role :: Query String
role = stringProperty "WM_ROLE"

-- Default plus hinting and avoidStruts
pbLayout = avoidStruts . layoutHints $ layoutHook defaultConfig

-- | @dzenPP@ plus sorting by Xinerama, softer title/layout colors, 
--   hiding of the NSP workspace and a nice @ppLayout@ if you happen to 
--   use @'pbLayout'@.
--
-- > logHook = dynamicLogWithPP $ pbPP { ppOutput = hPutStrLn d }
--
pbPP :: PP
pbPP = dzenPP
    { ppHidden = hideNSP
    , ppSort   = getSortByXineramaRule
    , ppTitle  = dzenColor "#909090" "" . pad
    , ppLayout = dzenColor "#909090" "" . pad . \s ->
        case s of
            "Hinted Tall"          -> "/ /-/"
            "Hinted Mirror Tall"   -> "/-,-/"
            "Hinted Full"          -> "/   /"
            _                      -> pad s
    }

-- | Hide the "NSP" workspace.
hideNSP :: WorkspaceId -> String
hideNSP ws = if ws /= "NSP" then pad ws else ""

-- | Spawn any command on urgent; discards the workspace information.
data SpawnSomething = SpawnSomething String deriving (Read, Show)

instance UrgencyHook SpawnSomething where
    urgencyHook (SpawnSomething s) _ = spawn s

-- | Ding! on urgent via ossplay and a sound from Gajim.
pbUrgencyHook :: SpawnSomething
pbUrgencyHook = SpawnSomething "ossplay -q /usr/share/gajim/data/sounds/message2.wav"

-- | Default but still show urgent on visible non-focused workspace.
--
-- > xmonad $ withUrgencyHookC pbUrgencyHook pbUrgencyConfig $ defaultConfig
--
pbUrgencyConfig :: UrgencyConfig
pbUrgencyConfig = urgencyConfig { suppressWhen = OnScreen }

-- | Spawns yeganesh <http://dmwit.com/yeganesh/>, set the environment 
--   variable @$DMENU_OPTIONS@ to customize dmenu appearance, this is a 
--   good @M-p@ replacement.
yeganesh :: MonadIO m => m ()
yeganesh = spawn "exe=`dmenu_path | yeganesh -- $DMENU_OPTIONS` && eval \"exec $exe\""

-- | Execute a command in the user-configured terminal.
--
-- > runInTerminal [ "screen", "-S", "my-session", "-R", "-D", "my-session" ]
--
runInTerminal :: [String] -> X ()
runInTerminal args = asks config >>= \c@XConfig { terminal = t } -> spawn $ unwords (t:args)

-- | Spawn in accordance with <http://pbrisbin.com/posts/screen_tricks>.
spawnInScreen :: String -> X ()
spawnInScreen c = runInTerminal [ "-title", c, "-e bash -cl", "\"SCREEN_CONF=" ++ c, "screen -S", c, "-R -D", c ++ "\"" ]

-- | Kill (@-9@) any running dzen and conky processes before executing 
--   the default restart command, this is a good @M-q@ replacement.
cleanStart :: MonadIO m => m ()
cleanStart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && "
                  ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                  ++ "xmonad --recompile && xmonad --restart"
