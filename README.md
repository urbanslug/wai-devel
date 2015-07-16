### Yesod-devel
[WAI] compliant development server for haskell web frameworks.
**NOTE: NOT limited to yesod.**



### Usage
At the core of your WAI application have an **Application.hs** file whose `main` function is of type `IO ()`.

If the yesod-devel binary is in your PATH call `yesod-devel` from terminal.

It will start a development server at port 3000.

An expample web application is in the [examples/] subdir.

To run this you need the github (latest) version of ide-backend.
More specifically from [this version] onwards.


### Up next
Functionality to read configs from a config file.


[WAI]: www.yesodweb.com/book/web-application-interface
[this version]: https://github.com/fpco/ide-backend/tree/19561d9ff5f496d6556f38992bc8d08896d54091
[examples/]: https://github.com/urbanslug/yesod-devel/tree/master/examples
