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

-- Imports {{{

-- my lib
import Dzen           -- http://pbrisbin.com/xmonad/docs/Dzen.html
import ScratchPadKeys -- http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html
import SendFile       -- http://pbrisbin.com/xmonad/docs/SendFile.html
import RssReader      -- http://pbrisbin.com/xmonad/docs/RssReader.html

-- xmonad
import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

-- xmonad-contrib
import XMonad.Actions.CycleWS            (toggleWS)
import XMonad.Actions.FindEmptyWorkspace (tagToEmptyWorkspace, viewEmptyWorkspace)
import XMonad.Actions.GroupNavigation    (Direction(..), historyHook, nextMatch)
import XMonad.Actions.Warp               (Corner(..), banishScreen)
import XMonad.Actions.WithAll            (killAll)
import XMonad.Hooks.DynamicLog hiding    (dzen)
import XMonad.Hooks.EwmhDesktops         (ewmh)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM                  (Property(..), withIM)
import XMonad.Layout.LayoutCombinators   ((|||), JumpToLayout(..))
import XMonad.Layout.LayoutHints         (layoutHintsWithPlacement)
import XMonad.Layout.NoBorders           (Ambiguity(..), With(..), lessBorders)
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.ResizableTile       (ResizableTall(..), MirrorResize(..))
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.Loggers               (Logger, maildirNew, dzenColorL, wrapL, shortenL)
import XMonad.Util.Run                   (spawnPipe)
import XMonad.Util.WindowProperties      (getProp32s)
import XMonad.Util.WorkspaceCompare      (getSortByXineramaRule)

import qualified XMonad.Prompt as P

-- general haskell stuff
import Data.Char             (toLower)
import Data.List             (isPrefixOf)
import System.IO             (Handle, hPutStrLn, hGetContents)
import System.Process        (runInteractiveCommand)
import System.FilePath.Posix (splitFileName)

-- }}}

-- Main {{{
main :: IO ()
main = do
    d <- spawnDzen myLeftBar

    spawn "conky"
    spawn $ "conky -c ~/.dzen_conkyrc | " ++ dzen myRightBar
    spawnDzen myRssBar >>= spawnReader myReaderConf

    -- ewmh just makes wmctrl work
    xmonad $ ewmh $ withUrgencyHookC myUrgencyHook myUrgencyConfig $ defaultConfig
        { terminal           = myTerminal
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , logHook            = myLogHook d
        } `additionalKeysP` myKeys

-- }}}

-- Options {{{
myTerminal           = "urxvtc"
myBorderWidth        = 3
myNormalBorderColor  = colorFG
myFocusedBorderColor = colorFG4

-- if you change workspace names, be sure to update them throughout
myWorkspaces = ["1-main","2-web","3-chat"] ++ map show [4..9]

-- aur/dzen2-svn is required for an xft font
myFont = "Verdana-8"

-- background/foreground and various levels of emphasis
colorBG  = "#303030"
colorFG  = "#606060"
colorFG2 = "#909090"
colorFG3 = "#c4df90"
colorFG4 = "#cc896d"
colorFG5 = "#c4df90"
colorFG6 = "#ffffba"

-- status bar sizes
leftBarWidth  = 920
rssBarWidth   = 1000
rightBarWidth = 1680

-- }}}

-- Layouts {{{
--
-- See http://pbrisbin.com/posts/xmonads_im_layout/
--
myLayout = avoidStruts $ onWorkspace "3-chat" imLayout standardLayouts

    where
        -- use standardLayouts just like any other workspace
        imLayout = withIM (1/10) (Role "roster") standardLayouts

        -- a simple Tall, Wide, Full setup but hinted, resizable, and
        -- with smarter borders
        standardLayouts = smart $ tiled ||| Mirror tiled ||| full

        tiled = hinted $ ResizableTall 1 (1/100) golden []
        full  = hinted Full

        -- master:slave set at the golden ratio
        golden = toRational $ 2/(1 + sqrt 5 :: Double)

        -- just like smartBorders but better for a xinerama setup
        smart = lessBorders $ Combine Union OnlyFloat OtherIndicated

        -- like hintedTile but applicable to any layout
        hinted l = layoutHintsWithPlacement (0,0) l

-- }}}

-- ManageHook {{{
myManageHook :: ManageHook
myManageHook = mainManageHook <+> manageDocks <+> manageFullScreen <+> manageScratchPads scratchPadList

    where
        -- the main managehook
        mainManageHook = composeAll $ concat
            [ [ resource  =? r     --> doIgnore         |  r    <- myIgnores ]
            , [ className =? c     --> doShift "2-web"  |  c    <- myWebs    ]
            , [ title     =? t     --> doShift "3-chat" |  t    <- myChats   ]
            , [ className =? c     --> doShift "3-chat" | (c,_) <- myIMs     ]
            , [ className =? c     --> doFloat          |  c    <- myFloats  ]
            , [ className =? c     --> doCenterFloat    |  c    <- myCFloats ]
            , [ name      =? n     --> doCenterFloat    |  n    <- myCNames  ]
            , [ classNotRole (c,r) --> doFloat          | (c,r) <- myIMs     ]
            , [ isDialog           --> doCenterFloat                         ]
            ]

        -- fullscreen but still allow focusing of other WSs
        manageFullScreen = isFullscreen --> doF W.focusDown <+> doFullFloat

        -- a special query to find an im window that's not my buddy list
        classNotRole :: (String,String) -> Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r

        role = stringProperty "WM_WINDOW_ROLE"
        name = stringProperty "WM_NAME"

        myIMs     = [("Gajim.py","roster")]
        myIgnores = ["desktop","desktop_window"]
        myChats   = ["irssi","mutt" ]
        myWebs    = ["Uzbl","Uzbl-core","Jumanji","Chromium"]
        myFloats  = ["MPlayer","Zenity","VirtualBox","rdesktop"]
        myCFloats = ["Xmessage","Save As...","XFontSel"]
        myCNames  = ["bashrun"]

-- }}}

-- StatusBars {{{
--
-- See http://pbrisbin.com/xmonad/docs/Dzen.html
--
myLeftBar :: DzenConf
myLeftBar = defaultDzen
    { width       = leftBarWidth
    , font        = myFont
    , fg_color    = colorFG
    , bg_color    = colorBG
    }

myRssBar :: DzenConf
myRssBar = myLeftBar
    { x_position = leftBarWidth
    , width      = rssBarWidth
    }

myRightBar :: DzenConf
myRightBar = myLeftBar
    { x_position = leftBarWidth + rssBarWidth
    , width      = rightBarWidth
    , alignment  = RightAlign
    }

-- }}}

-- RssReader {{{
--
-- See http://pbrisbin.com/xmonad/docs/RssReader.html
--
myReaderConf :: ReaderConf
myReaderConf = defaultReaderConf
    -- stealing some dynamicLog functions here
    { titleFormat = wrap "" ":" . dzenColor colorFG2 "" . dzenEscape
    , descrFormat = dzenEscape . shorten 200
    }

-- }}}

-- Prompt {{{
promptConfig :: P.XPConfig
promptConfig = P.defaultXPConfig 
    { P.font        = myFont
    , P.bgColor     = colorBG
    , P.fgColor     = colorFG2
    , P.fgHLight    = colorFG4
    , P.bgHLight    = colorBG
    , P.borderColor = colorFG
    }

-- }}}

-- LogHook {{{
myLogHook :: Handle -> X ()
myLogHook h = do
    -- the main logHook
    dynamicLogWithPP $ defaultPP
        { ppCurrent         = dzenFG colorFG5 . pad
        , ppVisible         = dzenFG colorFG2 . pad
        , ppHidden          = dzenFG colorFG2 . noScratchPad
        , ppHiddenNoWindows = namedOnly
        , ppUrgent          = dzenFG colorFG4 . pad . dzenStrip . noScratchPad
        , ppSep             = replicate 4 ' '
        , ppWsSep           = []
        , ppTitle           = shorten 100 . map toLower . highlightBase colorFG6
        , ppLayout          = dzenFG colorFG2 . renameLayouts . stripIM
        , ppSort            = getSortByXineramaRule
        , ppExtras          = [myMail, myUpdates]
        , ppOutput          = hPutStrLn h
        }

    -- update window focus history
    historyHook

    where
        -- don't show 4-9 if they're empty, never show NSP
        namedOnly    ws = if any (`elem` ws) ['a'..'z'] then pad ws else ""
        noScratchPad ws = if ws /= "NSP"                then pad ws else ""

        -- if the title appears to be a filename, highlight the basename
        highlightBase c s = 
            case splitFileName s of
                ("", baseName)      -> s
                (dirName, baseName) -> dirName ++ dzenFG c baseName

        -- L needed for loggers
        dzenFG  c = dzenColor  c ""
        dzenFGL c = dzenColorL c "" 

        -- custom loggers
        myMail    = wrapL "mail: "    "" . dzenFGL colorFG6 $ maildirNew "/home/patrick/Mail/GMail/INBOX"
        myUpdates = wrapL "updates: " "" . dzenFGL colorFG6 $ countOutputLines "pacman -Qu"
        
        -- count the lines of output of an arbitary command
        countOutputLines :: String -> Logger
        countOutputLines c = io $ do
            (_, out, _, _) <- runInteractiveCommand c
            doCount out `catch` const (return Nothing)
        
            where
                -- 0 returns nothing
                doCount h = hGetContents h >>= \c ->
                    case length $ lines c of
                        0 -> return Nothing
                        n -> return $ Just $ show n

        renameLayouts s = case s of
            "Hinted ResizableTall"          -> "/ /-/"
            "Mirror Hinted ResizableTall"   -> "/-,-/"
            "Hinted Full"                   -> "/   /"
            _                               -> s

        stripIM s = if "IM " `isPrefixOf` s then drop (length "IM ") s else s

-- }}}

-- SpawnHook {{{
--
-- Spawn any arbitrary command on urgent
--
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
    urgencyHook (MySpawnHook s) _ = spawn s

myUrgencyHook :: MySpawnHook
myUrgencyHook = MySpawnHook "ossplay -q /usr/share/gajim/data/sounds/message2.wav" 

myUrgencyConfig :: UrgencyConfig
myUrgencyConfig = UrgencyConfig OnScreen (Repeatedly 1 30)

-- }}}

-- KeyBindings {{{
myKeys :: [(String, X())]
myKeys = [ ("M-p"                   , spawn "launcher"               ) -- dmenu app launcher
         , ("M-S-p"                 , spawn "bashrun"                ) -- gmrun replacement
         , ("M4-f"                  , sendFile                       ) -- prompt for and send a file via mutt
         , ("M4-b"                  , myBrowser                      ) -- open web client
         , ("M4-e"                  , myEject                        ) -- open/close tray 
         , ("M4-l"                  , myLock                         ) -- W-l to lock screen
         , ("M4-i"                  , myIRC                          ) -- open/attach IRC client in screen
         , ("M4-r"                  , myTorrents                     ) -- open/attach rtorrent in screen 
         , ("M4-a"                  , spawn "msearch all"            ) -- search current playlist via dmenu
         , ("M4-g"                  , spawn "goodsong"               ) -- note current song as 'good'
         , ("M4-S-g"                , spawn "goodsong -p"            ) -- play a random 'good' song
         , ("<Print>"               , spawn "sshot"                  ) -- take a screenshot

         -- extended workspace navigations
         , ("M-<Esc>"               , toggleWS                       ) -- switch to the most recently viewed ws
         , ("M-<Backspace>"         , focusUrgent                    ) -- focus most recently urgent window
         , ("M-S-<Backspace>"       , clearUrgents                   ) -- make urgents go away
         , ("M-0"                   , viewEmptyWorkspace             ) -- go to next empty workspace
         , ("M-S-0"                 , tagToEmptyWorkspace            ) -- send window to empty workspace and view it

         -- extended window movements
         , ("M-o"                   , mirrorShrink                   ) -- shink slave panes vertically
         , ("M-i"                   , mirrorExpand                   ) -- expand slave panes vertically
         , ("M-f"                   , jumpToFull                     ) -- jump to full layout
         , ("M-b"                   , banishScreen LowerRight        ) -- banish the mouse
         , ("M-<Tab>"               , nextMatch History (return True)) -- recreates a "normal" Alt-Tab

         -- non-standard screen navigation
         , ("M-h"                   , focusScreen 0                  ) -- focus left screen
         , ("M-l"                   , focusScreen 1                  ) -- focus rght screen
         , ("M-S-h"                 , shrink                         ) -- shrink master (was M-h)
         , ("M-S-l"                 , expand                         ) -- expand master (was M-l)

         -- media keys
         , ("<XF86AudioPlay>"       , spawn "mpc toggle"             ) -- play/pause mpd
         , ("<XF86AudioStop>"       , spawn "mpc stop"               ) -- stop mpd
         , ("<XF86AudioPrev>"       , spawn "mpc prev"               ) -- prev song
         , ("<XF86AudioNext>"       , spawn "mpc next"               ) -- next song
         , ("<XF86AudioMute>"       , spawn "ossvol -t"              ) -- toggle mute
         , ("<XF86AudioLowerVolume>", spawn "ossvol -d 1"            ) -- volume down 
         , ("<XF86AudioRaiseVolume>", spawn "ossvol -i 1"            ) -- volume up
         , ("M-<XF86AudioPlay>"     , mplayer "pause"                ) -- play/pause mplayer
         , ("M-<XF86AudioStop>"     , mplayer "stop"                 ) -- stop mplayer
         , ("M-<XF86AudioPrev>"     , mplayer "seek -10"             ) -- seek back 10s
         , ("M-<XF86AudioNext>"     , mplayer "seek 10"              ) -- seek forward 10s

         -- kill, reconfigure, exit
         , ("M4-S-c"                , killAll                        ) -- close all windows on this ws
         , ("M-q"                   , myRestart                      ) -- restart xmonad
         , ("M-S-q"                 , spawn "leave"                  ) -- logout menu

         -- See http://pbrisbin.com/xmonad/docs/ScratchPadKeys.html
         ] ++ scratchPadKeys scratchPadList

    where

        shrink = sendMessage Shrink
        expand = sendMessage Expand

        mirrorShrink = sendMessage MirrorShrink
        mirrorExpand = sendMessage MirrorExpand

        focusScreen n = screenWorkspace n >>= flip whenJust (windows . W.view)
        jumpToFull    = sendMessage $ JumpToLayout "Hinted Full"

        myBrowser  = spawn "$BROWSER"
        myLock     = spawn "slock"
        myEject    = spawn "eject -T"

        -- see http://pbrisbin.com/xmonad/docs/SendFile.html
        sendFile   = sendFilePrompt promptConfig "~/.mutt/alias"

        -- see http://pbrisbin.com/posts/screen_tricks/
        myIRC      = spawnInScreen "irssi"
        myTorrents = spawnInScreen "rtorrent"

        spawnInScreen s = spawn $ unwords [ myTerminal, "-title", s, "-e bash -cl", command s ]

            where
                -- a quoted command to pass off to bash -cl
                command s = ("\""++) . (++"\"") $ unwords ["SCREEN_CONF=" ++ s, "screen -S", s, "-R -D", s]

        -- see http://pbrisbin.com/posts/controlling_mplayer/
        mplayer s = spawn $ unwords [ "echo", s, "> $HOME/.mplayer_fifo" ]

        -- kill all conky/dzen2 before executing default restart command
        myRestart = spawn $ "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
                            "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                            "xmonad --recompile && xmonad --restart"

-- }}}
