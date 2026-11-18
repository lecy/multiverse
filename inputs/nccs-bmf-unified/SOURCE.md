# NCCS unified BMF, geocoded (already frozen)

**Frozen copy:** `s3://lecy/nonprofit-density/inputs/nccs-bmf/unified-bmf-geocoded/2026-09-21/`,
made 2026-11-17, with its own `SOURCE.md`. Key facts:

| Field | Value |
|---|---|
| File | `bmf_unified_geocoded.parquet`, 641,496,865 bytes, sha256 `c296860c572822404dc55861af89ea13ce11b48f51030313d97d067f1766a3ad` |
| Original URL | `https://nccsdata.s3.us-east-1.amazonaws.com/geocoding/unified-bmf/latest/bmf_unified_geocoded.parquet` (a moving `latest` path; remote last-modified 2026-09-21) |
| Obtained | Downloaded 2026-11-12 by `panel990::panelize(bmf = TRUE)` in 05 |
| Read by | `R/build/B_npo/05_npo_efile.R` and `09a_efile_grants.R` (through `panel990`); `R/build/B_npo/02b_npo_bmf_monthly.R` (geocodes) |
| Licence | NCCS, ODC-BY 1.0 |
| Rebuild | Put the file at `$MV_BUILD/raw/panel990/bmf/bmf_unified_geocoded.parquet` before running 05; panel990 then reuses it |

The S3 folder is `inputs/nccs-bmf/unified-bmf-geocoded/…`. The other NCCS BMF inputs
are documented here as `nccs-bmf-legacy`, `nccs-bmf-monthly` and
`nccs-bmf-long-panel`. When they are frozen, put them under the same parent
(`inputs/nccs-bmf/legacy/`, `…/monthly/`, `…/long-panel/`) for consistency.
