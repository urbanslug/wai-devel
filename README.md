### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Set the environment variable `GHC_PACKAGE_PATH`.  
Mine for example is: `export GHC_PACKAGE_PATH=~/.stack/snapshots/x86_64-linux/lts-2.22/7.8.4/pkgdb:`

It doesn't work with the default/usual yesod scaffold.  
I have a yesod-bin "fork" that generates an expected scaffold at: https://github.com/urbanslug/yesod-bin/

With the new scaffold run `stack init` then `stack build` to install your wai app's dependencies (a one off thing).  
run `wai-devel` after that to compile your code and start a devel server at localhost:3000
Watch your app run.

### Issue
There are no issues. If you find any issues report as a bug.

[WAI]: www.yesodweb.com/book/web-application-interface
