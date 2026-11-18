# NCCS e-file data (IRS 990 XML, parsed), via panel990

| Field | Value |
|---|---|
| Publisher | Urban Institute, National Center for Charitable Statistics. IRS 990 e-file XML, parsed by `ef2` 2.0.0 |
| Release | `efile_v3_1` (990, 990-EZ) and `efilepf_v3_1` (990-PF), published 2026-10-09 |
| Original URL | `https://nccs-efile.s3.us-east-1.amazonaws.com/public/` (tables P00, P01, P09, P10, PF01, PF02) |
| Reader | `panel990` 0.0.0.9000 at GitHub commit `7045d97c0022b097dba9401f7c0a4aa8e0b48e3e` (pinned in `renv.lock`) |
| Years used | 990/EZ tax years 2020–2024; 990-PF 2021–2024; grants (P09) 2020–2024 |
| Obtained | Downloaded by `panel990::panelize()` / `panelize_pf()`. Per-year table caches go in `$MV_BUILD/raw/panel990/<year>/` and `990PF/`, the panels in `efile_{990,pf,grants}_<year>.parquet`, and the download logs (URL, bytes, status) in `raw/panel990/logs/` |
| Read by | `R/build/B_npo/05_npo_efile.R`; `R/build/B_npo/09a_efile_grants.R` |
| Licence | NCCS data products: ODC-BY 1.0 (nccs.urban.org/nccs/terms §3.5). Check that the e-file catalog entry does not name a different licence; the terms page does not mention the e-file buckets by name. IRS XML: public domain |
| Redistribute in release | Yes, with attribution (pending the catalog check) |
| Cite as | Urban Institute, National Center for Charitable Statistics, NCCS e-file data, release efile_v3_1 / efilepf_v3_1 (2026-10-09). |
| Frozen copy | `inputs/nccs-efile/`: the raw per-year table caches (2020–24 and 990PF, about 1.7 GB) and `logs/` |

**What to freeze.** Two options:
- The raw table caches, which let `panelize()` rerun offline.
- Just the `efile_*_<year>.parquet` panels (about 1.1 GB), which are what 06 reads.

The raw caches are better, because the panels depend on the panel990 version as well as
the release. The 2009–2019 caches (about 620 MB) were used only for the paper-vs-e-file
diagnostic in `R/build/F_checks/10_source_coverage.R`. They are not needed for the
database.
