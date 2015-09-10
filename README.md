### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Installation

- cd wai-devel
- stack build
- stack install

The wai-devel binary will now be in your ~/.local/bin you can use it freely.


### Usage
Works with a fresh yesod scaffold.  
It gives your application a port to listen on via the environment variable PORT.  
Therefore, your application should "get" the environment variable PORT to find a port which it shall listen on.

Due to it's dependence on stack you can pass the `STACK_YAML=...` variable to wai-devel. \o/ win
e.g `STACK_YAML=stack-7.10 wai-devel`

Sorry cabal binary users. Support for you is coming soon.

### Issues

The build for the latest LTS fails on travis but works locally.
This is due to differences in the build plan.


### Coming next

- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.
- Proper support for cabal binary users and their sandboxes.

[WAI]: http://www.yesodweb.com/book/web-application-interface
