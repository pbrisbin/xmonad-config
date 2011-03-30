-------------------------------------------------------------------------------
-- |
-- Module      :  xmonad.hs
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- http://github.com/pbrisbin/xmonad-config/tree/simpler
--
-------------------------------------------------------------------------------

import XMonad

import System.IO                    (Handle, hPutStrLn)
import XMonad.Hooks.DynamicLog      (dynamicLogWithPP, dzenPP, PP(..), pad)
import XMonad.Hooks.ManageDocks     (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook     (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Util.EZConfig         (additionalKeysP)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

import qualified XMonad.StackSet as W

import Dzen           -- http://pbrisbin.com/xmonad/docs/Dzen.html
import ScratchPadKeys -- http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html

main :: IO ()
main = do
    spawn "conky"

    d <- spawnDzen defaultDzen
        { font     = Just "Verdana-8"
        , fg_color = Just "#303030"
        , bg_color = Just "#909090"
        }

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal   = myTerminal
        , workspaces = myWorkspaces
        , logHook    = myLogHook d
        , manageHook = myManageHook
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        } `additionalKeysP` myKeys

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1-main","2-web","3-chat"] ++ map show ([4..9] :: [Int])

myManageHook :: ManageHook
myManageHook = composeAll $ concat
    [ [ manageDocks                                         ]
    , [ manageHook defaultConfig                            ]
    , [ manageScratchPads scratchPadList                    ]
    , [ classOrName  v --> a          | (v, a ) <- myFloats ]
    , [ classOrTitle v --> doShift ws | (v, ws) <- myShifts ]
    , [ isDialog       --> doCenterFloat                    ]
    , [ isFullscreen   --> doF W.focusDown <+> doFullFloat  ]
    ]

    where

        classOrName  x = className =? x <||> stringProperty "WM_NAME" =? x
        classOrTitle x = className =? x <||> title                    =? x

        myFloats  = [ ("Zenity"    , doFloat      )
                    , ("VirtualBox", doFloat      )
                    , ("rdesktop"  , doFloat      )
                    , ("Xmessage"  , doCenterFloat)
                    , ("XFontSel"  , doCenterFloat)
                    , ("bashrun"   , doCenterFloat)
                    ]

        myShifts  = [ ("Uzbl"     , "2-web" )
                    , ("Uzbl-core", "2-web" )
                    , ("Jumanji"  , "2-web" )
                    , ("Chromium" , "2-web" )
                    , ("irssi"    , "3-chat")
                    ]

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dzenPP
    { ppOutput = hPutStrLn h
    , ppSort   = getSortByXineramaRule
    , ppHidden = \ws -> if ws /= "NSP" then pad ws else ""
    , ppLayout = \s  -> case s of
        "Tall"          -> "/ /-/"
        "Mirror Tall"   -> "/-,-/"
        "Full"          -> "/   /"
        _               -> pad s
    }

myKeys :: [(String, X())]
myKeys = [ ("M-p"   , spawn "launcher"        ) -- dmenu app launcher
         , ("M-S-p" , spawn "bashrun"         ) -- gmrun replacement
         , ("M4-b"  , spawn "$BROWSER"        ) -- open web client
         , ("M4-l"  , spawn "slock"           ) -- lock screen
         , ("M4-a"  , spawn "msearch all"     ) -- search current playlist via dmenu
         , ("M4-g"  , spawn "goodsong"        ) -- note current song as 'good'
         , ("M4-S-g", spawn "goodsong -p"     ) -- play a random 'good' song
         , ("M4-i"  , spawnInScreen "irssi"   ) -- open/attach IRC client in screen
         , ("M4-r"  , spawnInScreen "rtorrent") -- open/attach rtorrent in screen 
         , ("M-q"   , myRestart               ) -- restart xmonad
         ] ++ scratchPadKeys scratchPadList

    where

        spawnInScreen c = spawn . unwords $ myTerminal : ["-title", c, "-e bash -cl", "\"SCREEN_CONF=" ++ c, "screen -S", c, "-R -D", c ++ "\""]

        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && "
                         ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                         ++ "xmonad --recompile && xmonad --restart"
