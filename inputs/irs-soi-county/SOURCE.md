# IRS SOI county income data

| Field | Value |
|---|---|
| Publisher | Internal Revenue Service, Statistics of Income, County Data |
| Files | `<year>countyincome.zip` (state Excel files, TY2000–09); `2010countydata.zip` (`10incyallagi.csv`); `<yy>incyallnoagi.csv` (TY2011–23; fallback name `<yy>cyallnoagi.csv`) |
| Original URL | `https://www.irs.gov/pub/irs-soi/` + the file names above |
| Years used | TY2000–2023. Charitable contributions (N/A19700) from TY2010 |
| Obtained | Downloaded by script with a browser user agent (`fetch_ua()`), cached in `$MV_BUILD/raw/cov/irs_soi/` (543 files after unzipping, 95 MB) |
| Read by | `R/build/D_cov/15_cov_irs_soi.R` |
| Licence | U.S. government work, public domain |
| Redistribute in release | Yes (the downloaded zips and CSVs; the unzipped state workbooks can be rebuilt) |
| Cite as | Source: Internal Revenue Service, Statistics of Income, County Income Data, tax years 2000–2023. |
| Frozen copy | `inputs/irs-soi-county/` |

**Note:** a missing file returns NULL without stopping the script (15, lines 66–67).
Check that every year is present before freezing. TCJA (TY2018) changes the meaning of
the itemized-contribution series (DATA-MANIFEST.md 6.12).
