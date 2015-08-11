### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Set the environment variable `GHC_PACKAGE_PATH`.  
Mine for example is: `export GHC_PACKAGE_PATH=~/.stack/snapshots/x86_64-linux/lts-2.21/7.8.4/pkgdb:`

It doesn't work with the default/usual yesod scaffold.  
I have a yesod-bin "fork" that generates the expected scaffold at https://github.com/urbanslug/yesod-bin/

With the new scaffold run `stack build` to install dependencies (a one off thing).  
run `wai-devel` after that to compile your code and start a devel server at localhost:3000

### Issue
On my PC (not tested elsewhere).  
When recompiling code, ide-backend fails to find the expected dependencies therefore recompiles fail.

[WAI]: www.yesodweb.com/book/web-application-interface
