# My xmonad configuration

Very stock but with a few tasteful tweaks. Most of the config actually 
lies withing the Utils module.

### Custom modules

* Dzen: Easier dzen definitions
* ScratchPadKeys: Importable scratchpads

Other modules exist in `./lib` which I no longer use.

### Compiles on

* ghc 6 or 7
* xmonad 0.9 or darcs
* xmonad-contrib 0.9 or darcs

### Try it

Backup your original configuration:

    mv ~/.xmonad ~/xmonad.bak

Pull down mine:

    git clone git://github.com/pbrisbin/xmonad-config.git ~/.xmonad

Verify everything compiles before actually restarting:

    cd ~/.xmonad && ghci -ilib xmonad.hs

This will notify you of any problems.

Press `Ctrl-d` to exit ghci, then `M-q` to restart xmonad.
