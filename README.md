### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Works with a fresh yesod scaffold.  
It gives your application a port to listen on via the environment variable PORT.  
Therefore, your application should "get" the environment variable PORT to find a port which it shall listen on.
Set the environment variable `GHC_PACKAGE_PATH`

### Issues

- I have recieved a report from @haBuu that he has to run `stack exec wai-devel` in order to run wai-devel.
I can't replicate this problem currently but any issues you might have running wai-devel might be solved by running it via stack exec as shown above.
- Having an issue with ide-backend not using the ghc option `-dumpdir dir` so your project directory gets littered with .dump-hi files.
- Travis builds for 7.10 (lts-3.0) are failing at this step `- ./travis_long stack --no-terminal --skip-ghc-check test --only-snapshot` with this error that is not (wai-devel's) fault: `After installing Cabal, the package id couldn't be found (via ghc-pkg describe Cabal). This shouldn't happen, please report as a bug`  
However, it builds and passes tests locally for GHC-7.10 So that is more of a travis-stack issue that you aren't likely to run into.

### Coming next

- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.

[WAI]: www.yesodweb.com/book/web-application-interface
