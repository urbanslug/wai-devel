### wai-devel
[WAI] compliant development server for haskell web frameworks.

### Usage
Works with a fresh yesod scaffold.  
It gives your application a port to listen on via the environment variable PORT.  
Therefore, your application should "get" the environment variable PORT to find a port which it shall listen on.
Set the environment variable `GHC_PACKAGE_PATH`

### Issue

I have recieved a report from @haBuu that he has to run `stack exec wai-devel` in order to run wai-devel.
I can't replicate this problem currently but any issues you might have running wai-devel might be solved by running it via stack exec as shown above.

### Coming next

- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.

[WAI]: www.yesodweb.com/book/web-application-interface
