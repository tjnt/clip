# clipc / clipd

-----

Simple clipboard history for use in conjunction with fzf.

https://github.com/junegunn/fzf

### clipd - Clipboard monitoring process

* Monitor the clipboard and save if changes are made.
* The storage destination is a single file of SQLite.

### clipc - Client application

* Client to display and select clipboard history list.
* Minimum interface assuming cooperation with fzf.

## Install

use stack

```
cd clip
stack install
```

## Setting Example

### startup daemon process

Start the process from the .xinitrc or window manager startup process.

```sh
# .xinitrc

clipd &
```

or

```haskell
-- ex xmonad.hs

myStartupHook = do
    ...
    spawnOnce "clipd"
```

### bash alias

A shell function that passes the history list to fzf and updates the clipboard by selection.

```sh
# ex .zshrc

alias clip='clip_fzf'

clip_fzf() {
  local recid=$(clipc --list | fzf | awk '{print $1}')
  [[ -n $recid ]] && clipc --select $recid
}
```

## Usage and Options

### clipd

```
Usage: clipd [OPTION...]
  -d FILE  --database=FILE  database file path
  -i MSEC  --interval=MSEC  interval (msec)
  -v       --verbose        verbose output
)
```

### clipc

```
Usage: clipc [OPTION...]
  -l       --list           list clipboard history
  -c       --clear          clear clipboard history
  -s ID    --select=ID      select clipboard record
  -d FILE  --database=FILE  database file path
  -v       --verbose        verbose output
)
```

## Technologies and libraries

* [stack](https://docs.haskellstack.org/en/stable/README/): Build tool.
* [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple): SQLite client library.
* [Clipboard](https://hackage.haskell.org/package/Clipboard-2.3.1.0): Clipboard interface.

## License

BSD License (3-clause BSD License)
