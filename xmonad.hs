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
-- Now available in git http://github.com/pbrisbin/xmonad-config
--
-------------------------------------------------------------------------------

import XMonad

import System.IO                      (Handle, hPutStrLn)
import XMonad.Hooks.DynamicLog        (dynamicLogWithPP, defaultPP, PP(..), 
                                        pad, shorten, dzenColor, dzenStrip)
import XMonad.Hooks.ManageDocks       (manageDocks)
import XMonad.Hooks.ManageHelpers     (isDialog, isFullscreen, doFullFloat, 
                                        doCenterFloat)
import XMonad.Hooks.UrgencyHook       (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Util.EZConfig           (additionalKeysP)
import XMonad.Util.WorkspaceCompare   (getSortByXineramaRule)
import qualified XMonad.StackSet as W

import Dzen           -- http://pbrisbin.com/xmonad/docs/Dzen.html
import ScratchPadKeys -- http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html

main :: IO ()
main = do
    d <- spawnDzen myStatusBar
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal   = myTerminal
        , workspaces = myWorkspaces
        , manageHook = myManageHook
        , logHook    = myLogHook d
        } `additionalKeysP` myKeys

myTerminal :: String
myTerminal = "urxvtc"

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1-main","2-web","3-chat"] ++ map show ([4..9] :: [Int])

myFont, colorBG, 
    colorFG, colorFG2, 
    colorFG3, colorFG4:: String

myFont     = "Verdana-8"
colorBG    = "#303030"
colorFG    = "#909090"
colorFG2   = "#bbbbbb"
colorFG3   = "#c4df90"
colorFG4   = "#ffff80"

myStatusBar :: DzenConf
myStatusBar = defaultDzen
    { font     = Just myFont
    , fg_color = Just colorFG
    , bg_color = Just colorBG
    }

myManageHook :: ManageHook
myManageHook = mainManageHook <+> manageDocks <+> manageScratchPads scratchPadList

    where
        mainManageHook = composeAll $ concat
            [ [ classOrName  v --> a             | (v,a)  <- myFloats ]
            , [ classOrTitle v --> doShift ws    | (v,ws) <- myShifts ]
            , [ isDialog       --> doCenterFloat                      ]
            , [ isFullscreen   --> doF W.focusDown <+> doFullFloat    ]
            ]

        classOrName  x = className =? x <||> stringProperty "WM_NAME" =? x
        classOrTitle x = className =? x <||> title                    =? x

        myFloats  = [ ("MPlayer"   , doFloat      )
                    , ("Zenity"    , doFloat      )
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
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor colorFG3 "" . pad
    , ppVisible         = dzenColor colorFG2 "" . pad
    , ppUrgent          = dzenColor colorFG4 "" . pad . dzenStrip . noScratchPad
    , ppHidden          = noScratchPad
    , ppLayout          = renameLayouts
    , ppSort            = getSortByXineramaRule
    , ppHiddenNoWindows = namedOnly
    , ppSep             = replicate 4 ' '
    , ppWsSep           = []
    , ppTitle           = shorten 100
    , ppOutput          = hPutStrLn h
    }

    where
        namedOnly    ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""
        noScratchPad ws = if ws /= "NSP"                then pad ws else ""

        renameLayouts s = case s of
            "Tall"          -> "/ /-/"
            "Mirror Tall"   -> "/-,-/"
            "Full"          -> "/   /"
            _               -> s

myKeys :: [(String, X())]
myKeys = [ ("M-p"     , spawn "launcher"   ) -- dmenu app launcher
         , ("M-S-p"   , spawn "bashrun"    ) -- gmrun replacement
         , ("M4-b"    , spawn "$BROWSER"   ) -- open web client
         , ("M4-l"    , spawn "slock"      ) -- W-l to lock screen
         , ("M4-a"    , spawn "msearch all") -- search current playlist via dmenu
         , ("M4-g"    , spawn "goodsong"   ) -- note current song as 'good'
         , ("M4-S-g"  , spawn "goodsong -p") -- play a random 'good' song
         , ("M4-i"    , myIRC              ) -- open/attach IRC client in screen
         , ("M4-r"    , myTorrents         ) -- open/attach rtorrent in screen 
         , ("M-q"     , myRestart          ) -- restart xmonad

         -- See http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html and 
         -- http://pbrisbin.com/posts/scratchpad_everything/
         ] ++ scratchPadKeys scratchPadList

    where
        -- see http://pbrisbin.com/posts/screen_tricks/
        myIRC      = spawnInScreen "irssi"
        myTorrents = spawnInScreen "rtorrent"

        spawnInScreen s = spawn $ unwords [ myTerminal, "-title", s, "-e bash -cl", command s ]

            where
                -- a quoted command to pass off to bash -cl
                command s' = ("\""++) . (++"\"") $ unwords ["SCREEN_CONF=" ++ s', "screen -S", s', "-R -D", s']

        -- kill all conky/dzen2 before executing default restart command
        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                            "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                            "xmonad --recompile && xmonad --restart"
