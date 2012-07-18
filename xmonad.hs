-------------------------------------------------------------------------------
-- |
--
-- xmonad.hs, pbrisbin 2012
--
-- <https://github.com/pbrisbin/xmonad-config>
--
-------------------------------------------------------------------------------
import XMonad

import XMonad.Hooks.DynamicLog      (PP(..), dynamicLogWithPP, dzenColor, dzenPP, pad)
import XMonad.Hooks.ManageDocks     (manageDocks, avoidStruts)
import XMonad.Hooks.ManageHelpers   (isDialog, isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.UrgencyHook     (NoUrgencyHook(..), withUrgencyHook)
import XMonad.Layout.LayoutHints    (layoutHints)
import XMonad.Util.EZConfig         (additionalKeysP)
import XMonad.Util.Run              (spawnPipe, hPutStrLn)
import XMonad.Util.WorkspaceCompare (getSortByXineramaRule)

main :: IO ()
main = do
    d <- spawnPipe "dzen2 -p -xs 1 -ta l -fn Verdana-8 -e 'onstart=lower'"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal   = "urxvtc"
        , modMask    = mod4Mask
        , layoutHook = avoidStruts . layoutHints
                     $ layoutHook defaultConfig

        , manageHook = manageHook defaultConfig
                        <+> manageDocks
                        <+> composeAll
                            [ isDialog     --> doCenterFloat
                            , isFullscreen --> doFullFloat
                            ]

        , logHook = dynamicLogWithPP $ dzenPP
                        { ppOutput = hPutStrLn d
                        , ppSort   = getSortByXineramaRule
                        , ppHidden = hideNSP
                        , ppTitle  = pad . dzenColor "#bbb" ""
                        , ppLayout = const ""
                        }

        , startupHook = do
            spawn "conky -c ~/.xmonad/data/conky/main"
            spawn "conky -c ~/.xmonad/data/conky/dzen | dzen2 -p -xs 2 -ta r -fn Verdana-8 -e 'onstart=lower'"
        }

        `additionalKeysP`
            [ ("<XF86AudioMute>"       , spawn "ossvol -t"  )
            , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1")
            , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1")
            , ("M-p"                   , yeganesh           )
            , ("M-q"                   , restart            )
            ]

    where
        hideNSP :: WorkspaceId -> String
        hideNSP ws = if ws /= "NSP" then pad ws else ""

        yeganesh :: MonadIO m => m ()
        yeganesh = spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && $x &"

        restart :: MonadIO m => m ()
        restart = spawn $  "for pid in `pgrep conky`; do kill -9 $pid; done && "
                        ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                        ++ "xmonad --recompile && xmonad --restart"
