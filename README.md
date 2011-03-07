# My xmonad configuration

[Screenshot](http://pbrisbin.com/static/screenshots/current_desktop.png)

## Custom modules

* Dzen: Easier dzen definitions
* ScratchPadKeys: Importable scratchpads
* SendFile: XPrompt to send a file attachment via mutt
* RssReader: A statusbar Rss Reader

## In use with

From arch repos Via `pacman -S`:

* ghc 7.0.2

From hackage via `cabal install`:

* mtl 2.0.1.0
* X11 1.5.0
* X11-xft 0.3
* utf8-string 0.3.6
* xmonad 0.9.2
* xmonad-contrib 0.9.2

Usually, you can prefix with *haskell-* to install via pacman.

## Try it

Backup your original configuration:

    mv ~/.xmonad ~/.xmonad.bak

Pull down mine:

    git clone git://github.com/pbrisbin/xmonad-config.git ~/.xmonad

I use a few modules you may not have installed (ex: http and tagsoup), 
so you should verify everything compiles before actually restarting:

    cd ~/.xmonad
    ghci -ilib xmonad.hs

This will notify you of any missing modules.

Press `Ctrl-d` to exit ghci, then `M-q` to restart xmonad.
