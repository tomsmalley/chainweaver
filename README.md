This is a branch attempting to isolate a bug with Text on GHCJS.  See [ghcjs/ghcjs#748](https://github.com/ghcjs/ghcjs/issues/748).

```bash
nix-build -A ghcjs.frontend && ./result/bin/frontend
```

This will either:
1) print "OK", meaning no bug (without dedupe, or with some different optimisation options in frontend.cabal)
2) print some scrambled JSString.

An alternative nix workflow that is faster for iteration purposes, but  should also provide a build environment that is very consistent to an environment that we have tested is the following:

```
nix-shell -A ghcjs.frontend.env
cabal new-configure --ghcjs
cabal new-build frontend && node dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/frontend-1.5.1.2/x/frontend/build/frontend/frontend.jsexe/all.js
```
