
# microbs.lu

<!-- badges: start -->
[![Install from GitHub](https://img.shields.io/badge/install%20from-GitHub-blue)](https://github.com/prasadadhav/microbs_lu_r_package)
<!-- badges: end -->

`microbs.lu` is an R package used to process qPCR and ddPCR wastewater surveillance data for the Luxembourg microbs.lu dashboard.
It loads raw PCR data, applies the required calculations and checks, and prepares output files used for dashboard reporting.

## Installation

### Recommended: install a released version

For routine data processing, install a tagged release rather than the current development version.

Current release:

```r
install.packages(
  "https://github.com/prasadadhav/microbs_lu_r_package/releases/download/v0.0.2.3102a/microbs.lu_0.0.2.3102a.tar.gz",
  repos = NULL,
  type = "source"
)
```

Then load the package:

```r
library(microbs.lu)
```

Check the installed version:

```r
packageVersion("microbs.lu")
```

### Install a different version

Previous versions are available from the [GitHub Releases](https://github.com/prasadadhav/microbs_lu_r_package/releases) page.

To install a specific version, use the `.tar.gz` file attached to that release. For example:

```r
install.packages(
  "https://github.com/prasadadhav/microbs_lu_r_package/releases/download/v0.0.2.3102/microbs.lu_0.0.2.3102.tar.gz",
  repos = NULL,
  type = "source"
)
```

Installing another version replaces the currently installed version of `microbs.lu`.

### Development version

Only use the current `main` branch when testing unreleased changes:

```r
# install.packages("remotes")
remotes::install_github("prasadadhav/microbs_lu_r_package")
```

## Documentation

- [Operator guide](README_operator.md) — running the routine data-processing workflow.
- [Developer guide](DEV_GUIDE.md) — modifying, testing, building and maintaining the package.
- [Release history](https://github.com/prasadadhav/microbs_lu_r_package/releases) — released versions and changes.

## Repository structure

```text
R/          Package source code
tests/      Automated tests
man/        Generated function documentation
vignettes/  Longer package documentation
```

For package changes, work from the source repository, run the tests and package checks, then create a tagged release for users to install.
