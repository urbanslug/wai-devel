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

run stack build before running wai-devel for your application for the first time.

Sorry cabal binary users. Support for you is coming soon.

### Issues

Fixed recompiles to only recompile the changed file but broke recompiles in case
the change is in a TH dependency file such as shakesperean templates.


### Coming next

- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.
- Proper support for cabal binary users and their sandboxes.

[WAI]: http://www.yesodweb.com/book/web-application-interface
