# My (simpler) xmonad configuration

My master branch feels bloated and slow. This branch is a minimalist 
version with only the elements I really need/want -- I'll be running 
with this branch for a while and it may eventually become master.

## Custom modules

* Dzen: Easier dzen definitions
* ScratchPadKeys: Importable scratchpads

## Compiles on

* ghc 6 or 7
* xmonad 0.9 or darcs
* xmonad-contrib 0.9 or darcs

## Try it

Backup your original configuration:

    mv ~/.xmonad ~/xmonad.bak

Pull down mine and checkout this simpler branch:

    git clone git://github.com/pbrisbin/xmonad-config.git ~/.xmonad
    cd ~/.xmonad && git checkout simpler

Verify everything compiles before actually restarting:

    ghci -ilib xmonad.hs

This will notify you of any problems.

Press `Ctrl-d` to exit ghci, then `M-q` to restart xmonad.
