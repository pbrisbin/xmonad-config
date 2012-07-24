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
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

main :: IO ()
main = do
    conf <- statusBar "dzen2 -p -xs 1 -ta l -fn Verdana-8 -e 'onstart=lower'"
                dzenPP
                    { ppSort   = getSortByXineramaRule
                    , ppHidden = hideNSP
                    , ppUrgent = ppUrgent dzenPP . hideNSP
                    , ppTitle  = pad . dzenColor "#bbb" ""
                    , ppLayout = const ""
                    }
                (\XConfig { modMask = m } -> (m, xK_b))
                $ defaultConfig
                    { terminal   = "urxvtc"
                    , modMask    = mod4Mask
                    , manageHook = composeAll
                                        [ isDialog     --> doCenterFloat
                                        , isFullscreen --> doFullFloat
                                        , namedScratchpadManageHook scratchpads
                                        , manageHook defaultConfig
                                        ]
                    , startupHook = do
                        spawn "conky -c ~/.xmonad/data/conky/main"
                        spawn "conky -c ~/.xmonad/data/conky/dzen | dzen2 -p -xs 2 -ta r -fn Verdana-8 -e 'onstart=lower'"
                    }
                    `additionalKeysP`
                        [ ("<XF86AudioMute>"       , spawn "ossvol -t"  )
                        , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1")
                        , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1")
                        , ("M1-x"                  , namedScratchpadAction scratchpads "ossxmix")
                        , ("M1-m"                  , namedScratchpadAction scratchpads "mail"   )
                        , ("M-p"                   , spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && exec $x")
                        , ("M-a"                   , spawn $  "for pid in `pgrep conky`; do kill -9 $pid; done && "
                                                           ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
                                                           ++ "xmonad --recompile && xmonad --restart")
                        ]

    xmonad $ withUrgencyHook NoUrgencyHook conf

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
