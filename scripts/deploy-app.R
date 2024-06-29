# install ssdtools
remotes::install_github("poissonconsulting/ssdtools")
# get cran versions of poisson pkgs
pak::pak("err")
pak::pak("universals")
pak::pak("chk")

# build user guide/about
rmarkdown::render("inst/extdata/user-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/user-fr.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-en.md", output_format = "html_fragment")
rmarkdown::render("inst/extdata/about-fr.md", output_format = "html_fragment")

# deploy to poissonconsulting test site
rsconnect::deployApp(account = "poissonconsulting", appName = "shinyssdtools-dev", 
                     forceUpdate = TRUE)

# deploy to poissonconsulting live site
rsconnect::deployApp(account = "poissonconsulting", appName = "shinyssdtools", 
                     forceUpdate = TRUE)

# deploy to bcgov test site
rsconnect::deployApp(account = "bcgov-env", appName = "shinyssdtools_test", 
                     forceUpdate = TRUE)

# deploy to bcgov live site
rsconnect::deployApp(account = "bcgov-env", appName = "ssdtools", 
                     forceUpdate = TRUE)

