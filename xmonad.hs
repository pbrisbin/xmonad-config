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
import XMonad.StackSet (RationalRect(..))
import Control.Arrow ((***))

main :: IO ()
main = do
    conf <- statusBar "dzen2 -p -xs 1 -ta l -fn Verdana-8 -e 'onstart=lower'"
                dzenPP
                    { ppHidden = pad
                    , ppTitle  = pad . dzenColor "#bbb" "" . dzenEscape
                    , ppLayout = const ""
                    , ppSort   = do
                        xsort <- getSortByXineramaRule
                        return (xsort . namedScratchpadFilterOutWorkspace)
                    }
                toggleStrutsKey
                $ withScratchpads scratchpads
                $ withUrgencyHook NoUrgencyHook
                $ defaultConfig
                    { terminal   = "urxvtc"
                    , modMask    = mod4Mask
                    , manageHook = composeAll
                        [ isFullscreen --> doFullFloat
                        , manageHook defaultConfig
                        ]
                    }
                    `additionalKeysP`
                        [ ("<XF86AudioMute>"       , spawn "ossvol -t"  )
                        , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1")
                        , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1")
                        , ("M-p"                   , spawn "x=$(yeganesh -x -- $DMENU_OPTIONS) && exec $x")
                        , ("M-q"                   , spawn "killall dzen2; xmonad --recompile && xmonad --restart")
                        ]

    xmonad conf


scratchpads :: [(String, NamedScratchpad)]
scratchpads = [ ("M1-x", NS { name  = "ossxmix"
                            , cmd   = "ossxmix"
                            , query = className =? "Ossxmix"
                            , hook  = customFloating $ RationalRect (1/20) (1/10) (1/2) (1/2)
                            })
              , ("M1-m", NS { name  = "mail"
                            , cmd   = "urxvtc -name sp-mail -e mutt"
                            , query = resource =? "sp-mail"
                            , hook  = customFloating $ RationalRect (1/10) (1/5) (1/2) (1/2)
                            })
              ]


withScratchpads :: [(String, NamedScratchpad)] -> XConfig l -> XConfig l
withScratchpads sps conf@XConfig { manageHook = mHook } = conf
    { manageHook = mHook <+> namedScratchpadManageHook list } `additionalKeysP` keys

    where
        list :: [NamedScratchpad]
        list = map snd sps

        keys :: [(String, X ())]
        keys = map (id *** (namedScratchpadAction list . name)) sps


-- | The unexported X.H.DynamicLog.toggleStrutsKey
toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask = modm } = (modm, xK_b)
