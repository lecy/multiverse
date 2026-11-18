# BEA regional economic accounts (county)

| Field | Value |
|---|---|
| Publisher | U.S. Bureau of Economic Analysis, Regional Economic Accounts |
| Tables | CAINC1 (personal income, population), CAINC5N (earnings by NAICS industry, 2001+), CAINC5S (SIC, for 2000), CAEMP25N / CAEMP25S (employment), CAGDP1 (GDP), CAINC35 (transfer receipts) |
| Original URL | `https://apps.bea.gov/regional/zip/<TABLE>.zip` (unversioned: always the latest release) |
| Release used | Downloaded 2026-11 (see DATA-MANIFEST.md, section 3) |
| Years used | Income, earnings and GDP 2000–2024; employment and transfers 2000–2022 |
| Obtained | Downloaded by script, cached in `$MV_BUILD/raw/cov/bea/` (15 files, 404 MB) |
| Read by | `R/build/D_cov/12_cov_bea.R` |
| Licence | Public domain ("unless stated otherwise, BEA material is in the public domain", bea.gov/help/faq/147) |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Bureau of Economic Analysis, Regional Economic Accounts, tables CAINC1, CAINC5N, CAINC5S, CAEMP25N, CAEMP25S, CAGDP1, CAINC35 (release of November 2026). |
| Frozen copy | `inputs/bea-regional/` |

**Drift risk:** high. BEA revises every county series with each annual release, and
the URL does not change. Only the frozen copy reproduces the database.
