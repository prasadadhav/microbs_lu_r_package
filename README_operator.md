# microbs.lu — Operator’s Quickstart (for non-technical users)

What this does: Loads new qPCR/ddPCR files, merges with historical data, runs checks & calculations, and exports per-virus sheets for the dashboard.

## One-time setup (10–20 min)

### Install R + RStudio
Use a recent R (check DESCRIPTION → Depends: R (>= …) to confirm) and current RStudio.

### Install the package from GitHub
Currently the repo is made public, but if it is made private ask the admin to create a GitHub Personal Access Token (classic) with repo:read and store it as GITHUB_PAT (RStudio: Tools → Global Options → Git/SVN → “Enable credentials”, or in ~/.Renviron as GITHUB_PAT=xxxxx).

In R
```r
install.packages(c("remotes","devtools"))   # if needed
Sys.setenv(GITHUB_PAT = "YOUR_TOKEN")
remotes::install_github("prasadadhav/microbs_lu_r_package")
library(microbs.lu)
```

### Folder layout (must exist and be writable)
Exactly this structure under your working directory:
```bash
Data_Treatment/
  00_flux_data/
  00_standard_curve/
  0_raw_data_ddPCR/
  0_raw_data_qPCR/
  1_loaded_data/
  1_ckeck_data/     <- note the spelling: "ckeck"
  2_calc_data/
  3_created_data/
  4_data_4_dashboard/
microbs_runtime.R   <- the script you run
```

> Windows drive paths: The script points at L:/Units & Programmes/.... Make sure that drive is mapped and you have write permissions. If running from a server job, prefer a UNC path like `\\\\server\\share\\...`

## How to run the weekly pipeline
Open `data_treatment.R` and edit only these toggles at the top:
```r
bool_archive <- FALSE   # set TRUE only after you’ve confirmed outputs look good
quiet_mode   <- FALSE   # if a future version supports silent mode
```


Then run the script top to bottom (RStudio: Source with Echo). It will:
- Set paths using the set_microbs_* helpers (printed for your confirmation).
- Load Flux & Standard Curve: `load_microbs_flux_Data()`, `load_microbs_stdCurve_Data()`
- Load old raw: `load_microbs_old_raw_ddPCR_Data()`, `load_microbs_old_raw_qPCR_Data()`
- Load new raw → 1_loaded_data: `load_microbs_raw_ddPCR_Data()`, `load_microbs_raw_qPCR_Data()`
    - If bool_archive == TRUE, it will also call: `archive_microbs_raw_ddPCR_Data()`, `archive_microbs_raw_qPCR_Data()`, and archive the existing “loaded” files.
- (Optional checks: CURRENTLY UNIMPLEMENTED): `check_microbs_raw_ddPCR_Data()`, `check_microbs_raw_qPCR_Data()`
- Calculations → 2_calc_data: `calculations_microbs_ddPCR()`, `calculations_microbs_qPCR()`. 
    - Archives old calc files if bool_archive == TRUE.
- Create per-virus outputs → 3_created_data: `create_microbs_flu_file()`, `create_microbs_rsv_file()`, `create_microbs_sars_file()`
- Dashboard exports → 4_data_4_dashboard: `dashboard_microbs_flu_export()`, `dashboard_microbs_hRSV_export()`, `dashboard_microbs_SARS_CoV_export()`

Success check: The dashboard folder should have updated files for Influenza A, RSV, and SARS-CoV-2 with today’s timestamp; logs in the console should show the latest flux date reported by:
```r
df_flux_data <- load_microbs_flux_Data()
tail(df_flux_data$Sample_Date, 1)
```

### Archiving rule of thumb
- First run each week: set `bool_archive <- FALSE` until you visually confirm outputs.
- Second run (or after confirming): flip to `TRUE` to archive the files the script just consumed/overwrote. This keeps folders tidy and recoverable.

## Common pitfalls and quick fixes

| Symptom                                                  | Likely cause                                               | Fix                                                                                                                            |
| -------------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| “file/dir not found”                                     | Wrong working dir; drive L: not mapped; network path moved | Re-run the first lines that print the paths; confirm `get_microbs_wdirectory()` and subpaths exist. Use UNC if scheduled jobs. |
| Can’t write Excel / “file in use”                        | Excel has file open; network lock                          | Close the file everywhere; retry. If stuck, save with a new timestamp, or archive and regenerate.                              |
| No new samples imported                                  | Raw filenames/columns not in expected pattern              | Compare a “known-good” raw file from last week; rename headers to match; re-run just the *Load new raw* block.                 |
| Dates look wrong                                         | Locale/format mismatch                                     | Ensure raw dates are unambiguous (ISO `YYYY-MM-DD` best). If Excel dates appear numeric, format cells as Date before saving.   |
| RSV/Flu/SARS sheet missing columns                       | Standard curve or flux sheet absent                        | Verify `00_standard_curve/` and `00_flux_data/` contain the latest sheets; re-run from the top.                                |
| “object not found” in a `create_…` or `dashboard_…` call | Calculation step skipped                                   | Re-run the `calculations_microbs_*()` block, then the `create_…` / `dashboard_…` block.                                        |


> Tip: If something fails, don’t delete folders. Re-run just the block you need. The script is organized so each stage reads what it needs from earlier stages and overwrites outputs safely.