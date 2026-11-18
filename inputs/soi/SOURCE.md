# IRS SOI exempt-organization extracts, harmonized (already frozen)

**Frozen copy:** `s3://lecy/nonprofit-density/inputs/soi/`, made 2026-11-17, with
its own `README.md` (pipeline description), `MANIFEST.csv` (size and sha256 of every
file) and `scripts/`. This entry records what the build reads and a provenance gap
found on 2026-11-17 that the S3 README does not mention.

| Field | Value |
|---|---|
| Publisher | Internal Revenue Service, Statistics of Income, "Annual Extract of Tax-Exempt Organization Financial Data" (filing years 2012–2024), harmonized with `Nonprofit-Open-Data-Collective/soi-extract-harmonization`, plus NCCS Unified BMF V1.1 attributes |
| Upstream URLs | https://www.irs.gov/statistics/soi-tax-stats-annual-extract-of-tax-exempt-organization-financial-data; zips `https://www.irs.gov/pub/irs-soi/{yy}eofinextract990[ez\|pf].zip` (2012–17), `{yy}eoextract990[ez\|pf].zip` (2018–24); BMF `https://nccsdata.s3.amazonaws.com/harmonized/bmf/unified/BMF_UNIFIED_V1.1.csv` |
| Hub location | `<MV_INPUTS>/SOI/` (`01_data/00_raw/zip/` dated 2025-09-27; `04_final/` dated 2025-10-20; the frame 2025-10-16) |
| Licence | IRS extracts: public domain. Unified BMF fields: NCCS, ODC-BY 1.0 (attribute NCCS) |
| Redistribute in release | Yes, with NCCS attribution for the BMF-derived columns |

## What the build reads (the minimal subset)

| File | Read by | Rows / years | Columns |
|---|---|---|---|
| `02_sample-framework/SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED.CSV` (1.72 GB) | `R/build/B_npo/04_npo_soi.R` | `TAXYEAR` 2010–2024, `MISSING_YEAR != 1` | 16 of the file's columns: `EIN2, TAXYEAR, V501C, SOURCE, MISSING_YEAR, TOTREV, TOTEXP, REVCONT, TOTASS, TAXPEND, NTEE_IRS, NTEEV2, CENSUS_BLOCK_FIPS, LATITUDE, LONGITUDE, BMF_SUBSECTION_CODE` |
| `01_data/01_unzipped/irs-990-pf-soi-extract-YYYY.csv` (10 files, about 560 MB) | `R/build/B_npo/03b_npo_pf_soi.R` | Filing years 2012–2016, 2020–2024 | `EIN, TAX_PRD, TOTRCPTPERBKS, TOTEXPNSPBKS, CONTRPDPBKS, TOTASSETSEND, FAIRMRKTVALAMT` |
| `01_data/04_final/pc_taxyear_YYYY.csv` (13 files, about 4.4 GB) | `R/build/B_npo/09_npo_grants.R` | Tax years 2011–2023 | `EIN2, TAXYEAR, F9_09_EXP_GRANT_US_ORG_TOT, F9_09_EXP_GRANT_US_INDIV_TOT, F9_09_EXP_GRANT_FRGN_TOT, F9_00_TAX_PERIOD_END_DATE` |

A column-reduced parquet of each would be a small fraction of the size. Ship the full
files as frozen inputs, and the reduced ones as a convenience, if download size matters.

## Provenance gap (2026-11-17)

- **The sample frame cannot be regenerated from the saved scripts.**
  - `03_AUGMENT-SAMPLE-FRAME-W-REVOCATIONS-MISSING.R` merges only `EIN2, ORG_RULING_YEAR, NTEEV2, CENSUS_CBSA_FIPS, CENSUS_BLOCK_FIPS` from the BMF.
  - The file also has `NTEE_IRS, NTEE_NCCS, LATITUDE, LONGITUDE, BMF_SUBSECTION_CODE, ORG_NAME_CURRENT, REVOKED_YEAR_X`. Four of these (`NTEE_IRS, LATITUDE, LONGITUDE, BMF_SUBSECTION_CODE`) are read by 04.
  - `02_HARMONIZE-…` builds `TERMINATE_DOA_X`, but the file has `LTD_X, DOA_X, LTD_DOA_X`.
  - So the file was made by an edited version of the scripts that was not saved. Script 03 is also an exploratory working script (plots, several redefinitions of `flag_revocation()`).
- **The scripts are not runnable as saved.** They `setwd()` into another machine's Dropbox, and use folder names (`renamed`, `by_year2`, `final`) that differ from the frozen layout (`02_renamed`, `03_by_year2`, `04_final`).
- **The revocation inputs** to 03 sit outside the hub: `00 - URBAN/00-DATA/revocations/COMBINED_REVOCATIONS_JUN_2023_FEB_2025.CSV` and `2025-09-REVOCATIONS-ORGS.csv`. `REVOCATIONS-2010-TO-2025.CSV` in the frozen copy is probably derived from them.
- **What this means for the release:** the frozen file and its sha256 are the record, and the scripts document intent, not an exact recipe. Say this in the S3 README, or recover the edited script versions and commit them to `soi-extract-harmonization`.
