This is a branch attempting to isolate a bug with Text on GHCJS

```bash
nix-build -A ghcjs.frontend && open result/bin/frontend.jsexe/index.html
```

where `open` is a shell script I have in `~/bin`:

```bash
#!/bin/sh

gio open "$@" >& /dev/null
```

Then, when the page is loaded in a browser, go to the javascript console.  If you see a cors error message along the lines of

```
hthttps://eu1.testnet.chainweb.com:80/chainweb/0.0/testnet04/chain/8/pact
```

then you've reproduced the bug.  An alternative nix workflow that is faster for iteration purposes, but  should also provide a build environment that is very consistent to an environment that we have tested is the following:

```
nix-shell -A ghcjs.frontend.env
cabal new-configure --ghcjs
cabal new-build && open dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/frontend-1.5.1.2/x/frontend/build/frontend/frontend.jsexe/index.html
```
