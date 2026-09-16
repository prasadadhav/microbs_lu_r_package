# microbs.lu Maintainer Guide

This guide is for colleagues who need to maintain, modify, test, and install the `microbs.lu` R package.

The goal is not to turn every user into a software engineer. The goal is to make package changes safely and repeatably.

---

## 1. Golden rules

1. Do not edit the installed package files on your computer.
2. Do not edit `main` directly unless the maintainer explicitly asks you to.
3. Work on a branch for every change.
4. Test changes on dummy/safe data before touching production data.
5. Run the package checks before asking others to install your version.
6. Calculation changes need known expected values, not only visual inspection of Excel outputs.
7. Update `DESCRIPTION`, `NEWS.md`, and user documentation when the change affects users.

---

## 2. One-time setup

Install R, RStudio, and Git or GitHub Desktop.

In R:

```r
install.packages(c("devtools", "remotes", "roxygen2", "testthat"))
```

Optional, but useful on Windows if package installation/building fails:

```r
install.packages("pkgbuild")
pkgbuild::check_build_tools(debug = TRUE)
```

Clone the repository:

```bash
git clone https://github.com/prasadadhav/microbs_lu_r_package.git
cd microbs_lu_r_package
```

Open the project in RStudio:

```text
microbs.lu.Rproj
```

Install the current package locally:

```r
devtools::install()
library(microbs.lu)
packageVersion("microbs.lu")
```

---

## 3. Before every change

Start from the latest version:

```bash
git checkout main
git pull
```

Create a new branch:

```bash
git checkout -b short-description-of-change
```

Examples:

```bash
git checkout -b add-new-dilution-suffix
git checkout -b update-ddpcr-calculation
git checkout -b improve-excel-highlighting
```

Check which branch you are on:

```bash
git branch
```

---

## 4. The maintainer loop

After editing any file under `R/`, use this loop in RStudio:

```r
devtools::load_all()
```

Run a small example using safe/dummy data.

If roxygen comments changed, regenerate documentation:

```r
devtools::document()
```

Run tests:

```r
devtools::test()
```

Run the full package check:

```r
devtools::check()
```

Install locally for a final smoke test:

```r
devtools::install()
library(microbs.lu)
packageVersion("microbs.lu")
```

---

## 5. Example A: add a new dilution

File:

```text
R/03_LoadData.R
```

Function area:

```r
load_microbs_raw_ddPCR_Data()
```

Look for the dilution mapping:

```r
dplyr::case_when(
    grepl("D$", Sample) ~ 2,
    grepl("T$", Sample) ~ 3,
    grepl("Q$", Sample) ~ 4,
    ...
    TRUE ~ 0
)
```

Add the new mapping before `TRUE ~ 0`.

Example only:

```r
grepl("y$", Sample) ~ 21,  # 21x dilution; confirm suffix with team first
```

Checklist:

- Confirm the new suffix is not already used.
- Confirm it does not conflict with real sample IDs.
- Confirm sample-name trimming still works after the suffix is removed/interpreted.
- Run on a tiny dummy dataset with one normal sample and one diluted sample.
- Confirm the output `dilution` column is correct.
- If the calculation must multiply by this dilution, update the calculation logic too.

Useful quick check:

```r
devtools::load_all()
# Run the relevant loading function on dummy data
# Confirm the dilution column manually with View() or dplyr::filter()
```

---

## 6. Example B: change calculations

File:

```text
R/06_Calculations.R
```

Relevant calculation area:

```r
copies_L <- ((copies_uL * 20 / 5) * 80) * (1000 / 40)
copies_day <- copies_L * Flow_rate * 1000
copies_inhab <- (copies_day / inhab) * 100000
```

Before editing, write the formula in plain language:

```text
copies per litre = [explain each factor]
copies per day = copies per litre × flow rate × 1000
copies per 100,000 inhabitants = copies per day / population × 100,000
```

Checklist:

- Change one formula/factor at a time.
- Create a two-row dummy input with known expected outputs.
- Check `copies_L`, `copies_day`, and `copies_inhab` separately.
- Check positive/negative handling is unchanged unless intentionally changed.
- Update `NEWS.md` and user-facing documentation if outputs change.

A simple test idea:

```r
testthat::test_that("ddPCR copies_day uses expected flow conversion", {
  copies_L <- 10
  Flow_rate <- 2
  expected <- 10 * 2 * 1000
  testthat::expect_equal(copies_L * Flow_rate * 1000, expected)
})
```

This is only a minimal example. For a real calculation change, test the actual function output using a small known input dataset.

---

## 7. Example C: Excel writing and formatting

File:

```text
R/06_Calculations.R
```

Relevant functions from `openxlsx`:

```r
openxlsx::createWorkbook()
openxlsx::addWorksheet()
openxlsx::writeData()
openxlsx::freezePane()
openxlsx::createStyle()
openxlsx::addStyle()
openxlsx::saveWorkbook()
```

Checklist:

- Close the Excel file before running the function.
- Confirm the workbook is created.
- Confirm the expected sheets exist.
- Confirm the number of rows is correct.
- Confirm highlighted rows match dilution or warning rules.
- Confirm the filename includes the expected timestamp/version pattern.

Useful checks:

```r
file.exists("path/to/output.xlsx")

wb <- openxlsx::loadWorkbook("path/to/output.xlsx")
names(wb)
```

---

## 8. Commit and push

Check what changed:

```bash
git status
git diff
```

Add only relevant files:

```bash
git add R/03_LoadData.R R/06_Calculations.R tests/ DESCRIPTION NEWS.md README.md README_operator.md TRAINING_MAINTAINER_GUIDE.md
```

Commit:

```bash
git commit -m "Describe the package change clearly"
```

Push:

```bash
git push -u origin branch-name
```

Then open a pull request on GitHub.

The pull request should say:

- What changed.
- Why it changed.
- Which dummy data or real scenario was tested.
- Whether outputs changed.
- Which version/tag colleagues should install after merge.

---

## 9. Versioning and release notes

Update `DESCRIPTION` when the package changes:

```text
Version: 0.0.2.3103
```

Use a simple rule:

- Small internal fix: increase the last number.
- User-visible calculation/output change: increase the version and document clearly.
- Bigger workflow change: create a GitHub release/tag after testing.

Update `NEWS.md`:

```md
# microbs.lu 0.0.2.3103

- Added support for [new dilution suffix].
- Updated [calculation/Excel output] because [reason].
- Tested using [dummy dataset/date/example].
```

---

## 10. Install latest version from GitHub

For most users:

```r
install.packages("remotes")
remotes::install_github("prasadadhav/microbs_lu_r_package", upgrade = "never")
library(microbs.lu)
packageVersion("microbs.lu")
```

If the repository becomes private again, set a GitHub token first:

```r
Sys.setenv(GITHUB_PAT = "YOUR_TOKEN")
remotes::install_github("prasadadhav/microbs_lu_r_package", upgrade = "never")
```

Prefer `remotes::install_github()` for installation because it is lighter than loading the full `devtools` package just to install from GitHub.

---

## 11. Install a branch, tag, or exact commit

Install a branch:

```r
remotes::install_github(
  "prasadadhav/microbs_lu_r_package",
  ref = "branch-name",
  upgrade = "never"
)
```

Install a tag:

```r
remotes::install_github(
  "prasadadhav/microbs_lu_r_package",
  ref = "v0.0.2.3103",
  upgrade = "never"
)
```

Install a specific commit:

```r
remotes::install_github(
  "prasadadhav/microbs_lu_r_package",
  ref = "commit-sha-here",
  upgrade = "never"
)
```

Check what is installed:

```r
packageVersion("microbs.lu")
```

Restart R after switching versions.

---

## 12. Roll back to a previous working version

If the latest version breaks something, reinstall the last known good tag or commit:

```r
remove.packages("microbs.lu")
remotes::install_github(
  "prasadadhav/microbs_lu_r_package",
  ref = "last-good-tag-or-commit",
  upgrade = "never"
)
library(microbs.lu)
packageVersion("microbs.lu")
```

Record the rollback in the issue or team notes:

```text
Problem:
Installed version:
Rolled back to:
Dataset tested:
Remaining issue:
```

---

## 13. Common mistakes

| Mistake | Why it causes trouble | Safer habit |
|---|---|---|
| Editing installed package files | Changes cannot be reproduced or shared | Edit the cloned source repo |
| Working directly on `main` | Easy to break the shared version | Use a branch |
| Skipping `devtools::check()` | Problems appear later on another machine | Check before merge/release |
| Only checking Excel visually | Calculation errors can look plausible | Use known expected values |
| Not updating version | Users cannot know what they installed | Update `DESCRIPTION` |
| Installing latest without recording ref | Hard to reproduce later | Install from a tag/commit for stable workflows |

---

## 14. Minimal checklist before telling colleagues to install

Before announcing a new version, confirm:

- [ ] The change is on GitHub.
- [ ] The change is merged into the intended branch.
- [ ] `devtools::test()` passes.
- [ ] `devtools::check()` has no serious errors.
- [ ] `DESCRIPTION` version is updated.
- [ ] `NEWS.md` explains the change.
- [ ] A tag/release or commit SHA is available for installation.
- [ ] Installation was tested on at least one clean R session.
- [ ] A dummy or known-good dataset gives expected output.

---

## 15. Handy commands

```r
# In R
getwd()
devtools::load_all()
devtools::document()
devtools::test()
devtools::check()
devtools::install()
remotes::install_github("prasadadhav/microbs_lu_r_package", upgrade = "never")
packageVersion("microbs.lu")
```

```bash
# In terminal
pwd
git status
git pull
git checkout -b branch-name
git diff
git add file-name
git commit -m "message"
git push -u origin branch-name
```
