#deploy to test site first
rsconnect::deployApp(appDir = ".", account = "bcgov-env", 
                     appName = "ssdtools_test", forceUpdate = TRUE)


#deploy to live site
rsconnect::deployApp(appDir = ".", account = "bcgov-env", 
                     appName = "ssdtools", forceUpdate = TRUE)

