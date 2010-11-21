# My xmonad configuration

[Screenshot](http://pbrisbin.com/static/screenshots/current_desktop.png)

## Custom modules

* Dzen: Easier dzen definitions
* ScratchPadKeys: Importable scratchpads
* SendFile: XPrompt to send a file attachment via mutt
* RssReader: A statusbar Rss Reader

## In use with

* ghc 6.12.3-1
* haskell-mtl 1.1.0.2-5
* haskell-x11 1.5.0.0-5
* haskell-x11-xft 0.3-17
* haskell-utf8-string 0.3.6-5
* xmonad-darcs 20101026
* xmonad-contrib-darcs 20101026

## Try it

Backup your original configuration:

    mv ~/.xmonad ~/.xmonad.bak

Pull down mine:

    git clone git://github.com/pbrisbin/xmonad-config.git ~/.xmonad

I use a few modules you may not have installed (ex: http and 
regex-posix), so you should verify everything compiles before actually 
restarting:

    cd ~/.xmonad
    ghci -ilib xmonad.hs

This will notify you of any missing modules, install them from the aur 
(haskell-* packages) or using `cabal install`.

Press `Ctrl-D` to exit ghci, then `M-q` to restart xmonad.
