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
-- http://github.com/pbrisbin/xmonad-config/
--
-------------------------------------------------------------------------------

import XMonad

import Utils -- <http://pbrisbin.com/xmonad/docs/Utils.html>

import Dzen                       (DzenConf(screen), spawnDzen, spawnToDzen, defaultDzenXft)
import ScratchPadKeys             (scratchPadList, manageScratchPads, scratchPadKeys)
import System.IO                  (hPutStrLn)
import XMonad.Hooks.DynamicLog    (dynamicLogWithPP, PP(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook   (withUrgencyHookC)
import XMonad.Util.EZConfig       (additionalKeysP)

main :: IO ()
main = do
    d <- spawnDzen defaultDzenXft { screen = Just 0 }
    spawnToDzen "conky -c ~/.dzen_conkyrc" defaultDzenXft { screen = Just 1 }
    xmonad $ withUrgencyHookC pbUrgencyHook pbUrgencyConfig $ defaultConfig
        { terminal    = "urxvtc"
        , workspaces  = pbWorkspaces
        , layoutHook  = pbLayout
        , manageHook  = pbManageHook <+> myManageHook
        , logHook     = dynamicLogWithPP $ pbPP { ppOutput = hPutStrLn d }
        , startupHook = spawn "conky"
        } `additionalKeysP` myKeys

myManageHook :: ManageHook
myManageHook = composeAll [ matchAny v --> a | (v,a) <- myActions ] <+> manageScratchPads scratchPadList

    where myActions = [ ("rdesktop"  , doFloat         )
                      , ("Xmessage"  , doCenterFloat   )
                      , ("Gmrun"     , doCenterFloat   )
                      , ("Uzbl"      , doShift "2-web" )
                      , ("Uzbl-core" , doShift "2-web" )
                      , ("Chromium"  , doShift "2-web" )
                      , ("Firefox"   , doShift "2-web" )
                      , ("irssi"     , doShift "3-chat")
                      ]

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
         , ("M-q"                   , cleanStart              ) -- restart xmonad
         ] ++ scratchPadKeys scratchPadList
