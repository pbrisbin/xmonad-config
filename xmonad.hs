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

-- <http://pbrisbin.com/xmonad/docs/Utils.html>
import Utils
import Dzen (DzenConf(..), TextAlign(..), defaultDzenXft,
                spawnDzen, spawnToDzen)

import ScratchPadKeys             (scratchPadList, manageScratchPads, scratchPadKeys)
import System.IO                  (hPutStrLn)
import XMonad.Hooks.DynamicLog    (dynamicLogWithPP, PP(..))
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook   (withUrgencyHookC)
import XMonad.Util.EZConfig       (additionalKeysP)

main :: IO ()
main = do
    d <- spawnDzen defaultDzenXft { screen = Just 0 }
    spawnToDzen "conky -c ~/.conky/dzen" conkyBar
    xmonad $ withUrgencyHookC pbUrgencyHook pbUrgencyConfig $ defaultConfig
        { terminal    = "urxvtc"
        , modMask     = mod4Mask
        , workspaces  = pbWorkspaces
        , layoutHook  = pbLayout
        , manageHook  = pbManageHook <+> myManageHook
        , logHook     = dynamicLogWithPP $ pbPP { ppOutput = hPutStrLn d }
        , startupHook = spawn "conky -c ~/.conky/conkyrc"
        } `additionalKeysP` myKeys

    where
        conkyBar :: DzenConf
        conkyBar = defaultDzenXft
            { screen    = Just 1
            , alignment = Just RightAlign
            , fgColor   = Just "#606060"
            }

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
         , ("M1-b"                  , spawn "$BROWSER"        ) -- open web client
         , ("M1-l"                  , spawn "slock"           ) -- lock screen
         , ("M1-s"                  , spawn "msearch all"     ) -- search current playlist via dmenu
         , ("M1-g"                  , spawn "goodsong"        ) -- note current song as 'good'
         , ("M1-S-g"                , spawn "goodsong -p"     ) -- play a random 'good' song
         , ("<XF86AudioMute>"       , spawn "ossvol -t"       ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1"     ) -- volume down
         , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1"     ) -- volume up
         , ("M1-i"                  , spawnInScreen "irssi"   ) -- open/attach IRC client in screen
         , ("M1-r"                  , spawnInScreen "rtorrent") -- open/attach rtorrent in screen
         , ("M-q"                   , cleanStart              ) -- restart xmonad
         ] ++ scratchPadKeys scratchPadList
