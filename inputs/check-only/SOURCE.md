# Inputs read only by checks (not needed to build the database)

These are read by `R/build/F_checks/` scripts that tested alternatives. They don't
affect any output in `database/`. Freeze them only if the release includes the
checks.

| Input | Source and URL | Read by | Licence |
|---|---|---|---|
| IRS Form 1023-EZ approvals, 2014–2023 (+ 2024 xlsx) | NCCS mirror `raw/1023-ez-approvals/f1023ez-approvals-baseline-2014-2023.zip` (470 MB); 2022–23 byte-identical to the IRS files. The IRS link for 2024 returned 404 on 2026-11-17. Cached in `$MV_BUILD/raw/irs_1023ez/` | `10b_ntee_1023ez_check.R` | IRS: public domain; NCCS mirror: ODC-BY |
| NCCS NTEE-resolved crosswalk | `https://nccsdata.s3.dualstack.us-east-1.amazonaws.com/crosswalks/ntee-resolved/ntee_resolved_crosswalk.parquet` (last modified 2026-09-21; 99 MB) | `10c_ntee_resolved_check.R` | ODC-BY |
| NCCS e-file 2009–2019 table caches | as in `../nccs-efile/` | `10_source_coverage.R` (paper vs e-file shares) | ODC-BY |
| FEC 2020 and MEDSL 2024 official county returns | `$MV_BUILD/raw/cov/elections/{fec,medsl_2024}/` | `16b_verify_elections.R` | Public / CC0 |
| Old interpolated CBSA population | `<MV_INPUTS>/population/cbsa_population_2000_2025.csv` | comparison diagnostic in `01_geo_spine.R` | Project file |
