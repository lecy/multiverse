# BLS QCEW county files (private sector and NAICS 813; government ownerships)

| Field | Value |
|---|---|
| Publisher | U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages (QCEW) |
| Files | `qcew_<year>_10.csv` (all industries, total covered) and `qcew_<year>_813.csv` (NAICS 813, religious, grantmaking, civic, professional and similar organizations), 2000–2024 (50 files, 139 MB) |
| Original URLs | **Not recorded in the code.** DATA-MANIFEST.md says "2000–2013 annual by-industry zips, 2014+ open-data API". Those are, on 2026-11-17: `https://data.bls.gov/cew/data/files/<year>/csv/<year>_annual_by_industry.zip` (the `10` and `813` slices), and `https://data.bls.gov/cew/data/api/<year>/a/industry/{10,813}.csv` (both URL patterns respond). |
| Years used | 2000–2024, annual averages |
| Obtained | **Pre-placed in `$MV_BUILD/raw/cov/qcew/`. No script downloads them**; 20 warns and skips missing years |
| Read by | `R/build/D_cov/20_urban_economy.R` (private establishments, employment, wages, with and without 813); `R/build/D_cov/22_cov_government.R` (ownership codes 1–3, government jobs and wages) |
| Licence | Public domain ("everything BLS publishes is in the public domain", bls.gov/opub/copyright-information.htm) |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Bureau of Labor Statistics, Quarterly Census of Employment and Wages, county annual averages, 2000–2024. |
| Frozen copy | `inputs/bls-qcew/` |

**Open question:** which of the two URL patterns produced each cached year, and on
what date, is unknown. The 2000 file omits suppressed county rows instead of flagging
them (DATA-MANIFEST.md 6.19), which fits the zip source. **To do:** add a downloader
to 20 that rebuilds `qcew_<year>_{10,813}.csv` from the two patterns, and check it
against the cached files.
