### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Works with a fresh yesod scaffold.  
It gives your application a port to listen on via the environment variable PORT.  
Therefore, your application should "get" the environment variable PORT to find a port which it shall listen on.

Due to it's dependence on stack you can pass the `STACK_YAML=...` variable to wai-devel. \o/ win
e.g `STACK_YAML=stack-7.10 wai-devel`

Sorry cabal binary users. Support for you is coming soon.

### Issues

- Travis builds for 7.10 (lts-3.0) are failing at this step `- ./travis_long stack --no-terminal --skip-ghc-check test --only-snapshot` with this error that is not (wai-devel's) fault: `After installing Cabal, the package id couldn't be found (via ghc-pkg describe Cabal). This shouldn't happen, please report as a bug`  
However, it builds and passes tests locally for GHC-7.10 So that is more of a travis-stack issue that you aren't likely to run into.

### Coming next

- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.
- Proper support for cabal binary users and their sandboxes.
