# install cran ssdtools
pak::pak("ssdtools")
# get cran versions of poisson pkgs
pak::pak("err")
pak::pak("universals")
pak::pak("chk")

# deploy to bcgov test site
rsconnect::deployApp(account = "bcgov-env", appName = "shinyssdtools_test", 
                     forceUpdate = TRUE)

# deploy to bcgov live site
rsconnect::deployApp(account = "bcgov-env", appName = "ssdtools", 
                     forceUpdate = TRUE)

# deploy to poissonconsulting test site
rsconnect::deployApp(account = "poissonconsulting", appName = "shinyssdtools-dev", 
                     forceUpdate = TRUE)

# deploy to poissonconsulting live site
rsconnect::deployApp(account = "poissonconsulting", appName = "shinyssdtools", 
                     forceUpdate = TRUE)
