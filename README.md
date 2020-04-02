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
file:///htpi/v1/localhttps://eu1.testnet.chainweb.com:80/chainweb/0.0/testnet04/chain/8/pact
```

then you've reproduced the bug.
