# microbs.lu — Developer Guide (for anyone who edits the package)

## Inspect what’s inside
```r
library(microbs.lu)
help(package = "microbs.lu")           # function index
getNamespaceExports("microbs.lu")      # exported functions
?load_microbs_flux_Data                # function help (use any name here)
```

> Check out this for a [detailed guide](https://r-pkgs.org/) on how to work with an R package.

## Make a change → install locally
- Clone the repo and open `microbs.lu.Rproj` in RStudio.
- Edit files under `R/` (one function per file is ideal).
- Document with roxygen comments (`#' @param`, `#' @return`, `#' @export`, etc.).
- Regenerate docs & NAMESPACE, then install & reload:
```r
install.packages(c("devtools","roxygen2"))  # first time
devtools::document()
devtools::install()     # or Ctrl/Cmd + Shift + B in RStudio
```
-Run tests (there is a tests/ dir in your repo):
```r
devtools::test()
```
- Check the package:
```r
devtools::check()       # runs R CMD check with best practices
```
> If check is clean, commit & push. Versioning: bump `Version`: in `DESCRIPTION` and update README.md for any user-visible changes.

## Build a tarball / binary
From R:
```r
devtools::build()   # creates source tar.gz
```
or from terminal (I never did this personally)
```sh
R CMD build .
R CMD INSTALL microbs.lu_X.Y.Z.tar.gz
```
> (I never did this personally, but these are standard R build/install flows.)

## Reproducible environments (not usually done by me)
If this project will run on multiple PCs/servers, add renv:
```r
install.packages("renv")
renv::init()         # creates a project-local library and lockfile
# install your deps, then:
renv::snapshot()     # saves exact versions
# On a new machine:
renv::restore()      # reproduces the same package set
```
This prevents “works on my machine” surprises and pins versions for the pipeline.

## Installing from GitHub
This workflow for installation is when the git repo is private, but as of now, it is public so you don't need a token.
```r
Sys.setenv(GITHUB_PAT = "Ask_Prasad_for_a_token")
# install.packages("devtools")
devtools::install_github("prasadadhav/microbs_lu_r_package")
```

## Debugging like a pro (quick recipes)
- First and foremost: **READ** the error message! It holds the hints for the issue. Don't hesitate to google or ask an AI assistant with the exact error text before diving deeper into the debugging tools.
- Run a single function with debugonce() to step through:
```r
debugonce(load_microbs_raw_ddPCR_Data); load_microbs_raw_ddPCR_Data()
```
- Drop into an interactive browser anywhere by inserting `browser()` in the function.
- After an error:
```r
traceback()               # base R
# or if it’s a tidyverse error:
# rlang::last_trace()
```
- Print key objects at boundaries (loaded → calc → created → dashboard) to verify shapes and date ranges.
```r
str(df_flux_data)          # an example on how to quickly look at data
```

