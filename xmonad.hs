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

import Dzen                         (DzenConf(..), spawnDzen, defaultDzen)
import ScratchPadKeys               (scratchPadList, manageScratchPads, scratchPadKeys, runInTerminal)
import System.IO                    (Handle, hPutStrLn)
import XMonad.Hooks.DynamicLog      (dynamicLogWithPP, dzenPP, PP(..), pad, dzenColor)
import XMonad.Hooks.ManageDocks     (avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook     (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Layout.LayoutHints    (layoutHints)
import XMonad.Util.EZConfig         (additionalKeysP)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

import qualified XMonad.StackSet as W

main :: IO ()
main = do
    d <- spawnDzen defaultDzen { font = Just "Verdana-8" }
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal    = "urxvtc"
        , workspaces  = myWorkspaces
        , manageHook  = myManageHook <+> manageHook defaultConfig
        , layoutHook  = avoidStruts . layoutHints $ layoutHook defaultConfig
        , logHook     = myLogHook d
        , startupHook = spawn "conky"
        } `additionalKeysP` myKeys

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1-main","2-web","3-chat"] ++ map show [4..9 :: Int]

myManageHook :: ManageHook
myManageHook = composeAll $ concat
    [ [ manageDocks                                       ]
    , [ manageScratchPads scratchPadList                  ]
    , [ isDialog      --> doCenterFloat                   ]
    , [ isFullscreen  --> doF W.focusDown <+> doFullFloat ]
    , [ matchAny    v --> a | (v, a ) <- myActions        ]
    ]

    where
        -- match on class, title, name or role
        matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

        name = stringProperty "WM_NAME"
        role = stringProperty "WM_ROLE"

        myActions = [ ("Zenity"    , doFloat         )
                    , ("rdesktop"  , doFloat         )
                    , ("Xmessage"  , doCenterFloat   )
                    , ("XFontSel"  , doCenterFloat   )
                    , ("gmrun"     , doCenterFloat   )
                    , ("Uzbl"      , doShift "2-web" )
                    , ("Uzbl-core" , doShift "2-web" )
                    , ("Chromium"  , doShift "2-web" )
                    , ("irssi"     , doShift "3-chat")
                    ]

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dzenPP
    { ppOutput = hPutStrLn h
    , ppSort   = getSortByXineramaRule
    , ppTitle  = dzenColor "#909090" ""
    , ppHidden = \ws -> if ws /= "NSP" then pad ws else ""
    , ppLayout = dzenColor "#909090" "" . pad . \s  -> case s of
        "Hinted Tall"          -> "/ /-/"
        "Mirror Hinted Tall"   -> "/-,-/"
        "Hinted Full"          -> "/   /"
        _                      -> pad s
    }

myKeys :: [(String, X())]
myKeys = [ ("M-p"                   , yeganesh                ) -- dmenu app launcher
         , ("M4-b"                  , spawn "$BROWSER"        ) -- open web client
         , ("M4-l"                  , spawn "slock"           ) -- lock screen
         , ("M4-a"                  , spawn "msearch all"     ) -- search current playlist via dmenu
         , ("M4-g"                  , spawn "goodsong"        ) -- note current song as 'good'
         , ("M4-S-g"                , spawn "goodsong -p"     ) -- play a random 'good' song
         , ("<XF86AudioMute>"       , spawn "ossvol -t"       ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1"     ) -- volume down
         , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1"     ) -- volume up
         , ("M4-i"                  , spawnInScreen "irssi"   ) -- open/attach IRC client in screen
         , ("M4-r"                  , spawnInScreen "rtorrent") -- open/attach rtorrent in screen
         , ("M-q"                   , myRestart               ) -- restart xmonad
         ] ++ scratchPadKeys scratchPadList

    where
        yeganesh = spawn "exe=`dmenu_path | yeganesh -- $DMENU_OPTIONS` && eval \"exec $exe\""

        spawnInScreen c = runInTerminal [ "-title", c, "-e bash -cl", "\"SCREEN_CONF=" ++ c, "screen -S", c, "-R -D", c ++ "\"" ]

        -- kill conkys and dzen2s for they will be restarted
        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && "
                         ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                         ++ "xmonad --recompile && xmonad --restart"
