This project is not under maintenance.

### wai-devel

[![Join the chat at https://gitter.im/urbanslug/wai-devel](https://badges.gitter.im/urbanslug/wai-devel.svg)](https://gitter.im/urbanslug/wai-devel?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/urbanslug/wai-devel.svg?branch=master)](https://travis-ci.org/urbanslug/wai-devel)
[![Hackage](https://img.shields.io/hackage/v/wai-devel.svg)](https://hackage.haskell.org/package/wai-devel)
[![GitHub license](https://img.shields.io/github/license/mashape/apistatus.svg)]()

Development server for [WAI] compliant haskell web applications.

### Installation  
#### From hackage
- stack install wai-devel  
or  
- cabal install wai-devel  

#### From source  
- cd wai-devel
- stack build
- stack install  
or  
- cd wai-devel
- cabal build
- cabal install  

The wai-devel binary will now be in your ~/.local/bin you can use it freely.


### Usage
See the [wiki].

### Contributing
No contributing page yet. Just read the haddocks.  
See the [wai-devel minimal reproduction in the wiki].

### Issues
Doesn't recompile on changes in Template Haskell files. Here is the [specific issue].

### Coming next
- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.


[WAI]: http://www.yesodweb.com/book/web-application-interface
[wiki]: https://github.com/urbanslug/wai-devel/wiki
[specific issue]: https://github.com/fpco/ide-backend/issues/313
[wai-devel minimal reproduction in the wiki]: https://github.com/urbanslug/wai-devel/wiki/Minimal-reproduction.
