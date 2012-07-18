My xmonad configuration
=======================

Very stock, with some minor tweaks.

Such as:

-   urxvtc as terminal
-   Super key as Modifier
-   Fullscreen Manage hook
-   Dual monitor status bars (requires dzen2 with xinerama patch)
-   Volume keys (requires my ossvol script)
-   Yeganesh as app launcher

### Try it

Backup your original configuration:

    mv ~/.xmonad ~/xmonad.bak

Pull down mine:

    git clone https://github.com/pbrisbin/xmonad-config ~/.xmonad

Verify everything compiles before actually restarting:

    cd ~/.xmonad && ghci -Wall xmonad.hs

This will notify you of any problems.

Press `Ctrl-d` to exit ghci, then `M-q` to restart xmonad.
