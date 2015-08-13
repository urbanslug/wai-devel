### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Set the environment variable `GHC_PACKAGE_PATH`.  
Mine for example is: `export GHC_PACKAGE_PATH=~/.stack/snapshots/x86_64-linux/lts-2.22/7.8.4/pkgdb:`

I have only tested it with the yesod scaffold but it should work with any other app that:

* sets *wai_app* and *wai_port*
* has Application.develMain as an entry point for a development server


It doesn't work with the default yesod scaffold.  
I have a yesod fork that does nothing but generate the expacted scaffold at: https://github.com/urbanslug/yesod/

Instructions are given after you run yesod init in the binary from the fork above.

### Issue
There are no issues. If you find any issues please report.

[WAI]: www.yesodweb.com/book/web-application-interface
