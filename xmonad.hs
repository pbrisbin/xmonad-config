-------------------------------------------------------------------------------
-- |
--
-- xmonad.hs, pbrisbin 2012
--
-- <https://github.com/pbrisbin/xmonad-config>
--
-------------------------------------------------------------------------------
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

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
                            , namedScratchpadManageHook scratchpads
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
            , ("M1-x"                  , namedScratchpadAction scratchpads "ossxmix")
            , ("M1-m"                  , namedScratchpadAction scratchpads "mail"   )
            , ("M1-h"                  , namedScratchpadAction scratchpads "htop"   )
            ]

    where
        scratchpads :: [NamedScratchpad]
        scratchpads =
            [ NS { name  = "ossxmix"
                 , cmd   = "ossxmix"
                 , query = className =? "Ossxmix"
                 , hook  = customFloating $ W.RationalRect (1/20) (1/10) (1/2) (1/2)
                 }
            , NS { name  = "mail"
                 , cmd   = "urxvtc -name sp-mail -e mutt"
                 , query = resource =? "sp-mail"
                 , hook  = customFloating $ W.RationalRect (1/10) (1/5) (1/2) (1/2)
                 }
            ]

        hideNSP :: WorkspaceId -> String
        hideNSP ws = if ws /= "NSP" then pad ws else ""

        yeganesh :: MonadIO m => m ()
        yeganesh = spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && $x &"

        restart :: MonadIO m => m ()
        restart = spawn $  "for pid in `pgrep conky`; do kill -9 $pid; done && "
                        ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                        ++ "xmonad --recompile && xmonad --restart"
