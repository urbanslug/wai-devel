### wai-devel
[WAI] compliant development server for haskell web frameworks.
**NOTE: NOT limited to yesod.**



### Usage
Works with a scaffolded yesod app.

You can either modify your scaffold or create one from yesod-bin at https://github.com/urbanslug/yesod/yesod-bin

Set the environment variable `GHC_PACKAGE_PATH`.
Mine for example is: `export GHC_PACKAGE_PATH=~/.stack/snapshots/x86_64-linux/lts-2.21/7.8.4/pkgdb:`


##### Modifying your scaffold.

1. Delete app/devel.hs
2. Edit Application.hs `develMain` usually L128 to

```haskell
-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper' getApplicationDev

develMainHelper' :: IO (Settings, Application) -> IO ()
develMainHelper' getSettingsApp = do

    (settings, app) <- getSettingsApp
    port <- getEnv "wai_port"
    host <- getEnv "wai_host"

    let settings'  = setPort (3001 :: Port)  settings
        settings'' = setHost ("127.0.0.1" :: HostPreference) settings'

    runSettings settings'' app
```

Run wai-devel in the root of your web application.

wai-devel runs the develMain function in Application.hs and launches the scaffold.

    


[WAI]: www.yesodweb.com/book/web-application-interface
[this version]: https://github.com/fpco/ide-backend/tree/19561d9ff5f496d6556f38992bc8d08896d54091
[examples/]: https://github.com/urbanslug/yesod-devel/tree/master/examples
