# NCCS CORE files, harmonized (990, 990-EZ, 990-PF), TY2000–2019

| Field | Value |
|---|---|
| Publisher | Urban Institute, National Center for Charitable Statistics |
| Files | `CORE-YYYY-501C3-CHARITIES-PZ-HRMN.csv` (public charities, 990 + EZ); `CORE-YYYY-501CE-NONPROFIT-PZ-HRMN.csv` (other 501(c)); `CORE-YYYY-501C3-PRIVFOUND-PF-HRMN-V0.csv` (foundations) |
| Original URLs | `https://nccsdata.s3.us-east-1.amazonaws.com/harmonized/core/501c3-pz/CORE-%d-501C3-CHARITIES-PZ-HRMN.csv`; `…/harmonized/core/501ce-pz/CORE-%d-501CE-NONPROFIT-PZ-HRMN.csv`; `…/harmonized/core/501c3-pf/marts/CORE-%d-501C3-PRIVFOUND-PF-HRMN-V0.csv`. Unversioned. Remote last-modified 2025-03-04 (PZ, CE) and 2025-04-22 (PF); stamps for every file in `data/diagnostics/manifest_remote_versions.csv` |
| Years used | TY2000–2019 read. In the splice: 990/EZ 2000–2010 and 990-PF 2000–2015; 2011–19 for overlap diagnostics |
| Obtained | Downloaded by script to a temp file. The needed columns are kept as parquet in `$MV_BUILD/raw/nccs-core-hrmn/` (`core_{pz,ce}_<y>_hrmn.parquet`, `core_pf_<y>_v0.parquet`, `core_pz_<y>_formtype.parquet`, `core_pz_2010_grants.parquet`), and the raw CSV is deleted |
| Read by | `R/build/B_npo/03_npo_core.R`; `R/build/B_npo/09_npo_grants.R` (2010 PZ grants) |
| Licence | ODC-BY 1.0 (NCCS terms §3.5) |
| Redistribute in release | Yes, with attribution |
| Cite as | Urban Institute, National Center for Charitable Statistics, harmonized CORE files, tax years 2000–2019. |
| Frozen copy | `inputs/nccs-core/` |

**What to freeze.** The raw CSVs were deleted after reduction, so the cache holds
only the column-reduced parquets (about 650 MB in total). Two options:
1. Freeze the parquets as they are. They are the exact inputs to the rest of the build, but a user cannot rerun 03 from them.
2. Download the raw CSVs again, and check that 03 reproduces the cached parquets. Freeze the CSVs if it does. NCCS has not changed these files since 2025-03/04, according to the remote stamps.

Option 2 is better for the record. **Do not use the `-V1` "marts" PZ/CE files:**
their money fields are corrupted (DATA-MANIFEST.md 2.4), and 03 stops if
max |revenue| < 1.
