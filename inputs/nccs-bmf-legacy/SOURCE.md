# NCCS legacy BMF snapshots, 2000–2022

| Field | Value |
|---|---|
| Publisher | Urban Institute, National Center for Charitable Statistics (NCCS), from the IRS Exempt Organizations Business Master File |
| Files | `BMF-YYYY-MM-501CX-NONPROFIT-PX.csv`. The hub holds 88 snapshots (1989-06 to 2022-08, mostly dated 2024-04-06); **the build reads 23** (below) |
| Original URL | Not recorded. The files match the NCCS legacy BMF layout (`EIN, FIPS, NTEECC, …, OUTNCCS, nteeFinal`) and were downloaded unmodified from the NCCS data archive. Record the URL (NCCS catalog / `nccsdata.s3` legacy path) when freezing |
| Hub location | `<MV_INPUTS>/00-NCCS/bmf/` (`RAW$bmf_snap`) |
| Years used | 2000–2022 (2023–24 come from the monthly BMF, see `../nccs-bmf-monthly/`) |
| Read by | `R/build/B_npo/02_npo_bmf_counts.R` (registered stock, locations, NTEE); its outputs `ein_location_bmf.csv.gz` feed 06, 23 and 25 |
| Columns | `EIN, FIPS, STATE, CITY, ZIP5, SUBSECCD, FNDNCD, NTEEFINAL, NTEECC, OUTNCCS, RULEDATE, LATITUDE, LONGITUDE` (matched case-insensitively) |
| Licence | NCCS data: ODC-BY 1.0 (nccs.urban.org/nccs/terms, §3.5). Underlying IRS data: public domain |
| Redistribute in release | Yes, with attribution |
| Cite as | Urban Institute, National Center for Charitable Statistics, legacy Business Master Files, 2000–2022. |
| Frozen copy | `inputs/nccs-bmf-legacy/` (the 23 files only, about 7–7.5 GB) |

## Snapshots used

For each year the build takes the latest snapshot from April of that year to January
of the next. The list below was worked out from the hub's file names and agrees with
DATA-MANIFEST.md 2.1. **Confirm it against `data/diagnostics/bmf_snapshots.csv`
before freezing.**

```
2000-05  2002-01  2003-01  2003-11  2004-12  2006-01  2007-01  2008-01
2009-01  2010-01  2010-11  2011-12  2012-12  2013-12  2014-12  2015-12
2016-08  2017-12  2018-12  2019-08  2020-04  2022-01  2022-08
```

Known issues, handled in 02:
- 2016–18 omit most new organizations (DATA-MANIFEST.md 6.10).
- 2019+ code `SUBSECCD` / `FNDNCD` as "3.0" (6.13).
- 2020 uses the April snapshot, 8.6 months early (6.2).
