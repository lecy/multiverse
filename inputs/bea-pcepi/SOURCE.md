# PCE price index (deflator and active-filer floor)

| Field | Value |
|---|---|
| Publisher | U.S. Bureau of Economic Analysis (series PCEPI), as served by FRED, Federal Reserve Bank of St. Louis |
| File | `fred_pcepi.csv` |
| Original URL | `https://fred.stlouisfed.org/graph/fredgraph.csv?id=PCEPI` |
| Years used | 1999–2025, annual mean of the monthly index |
| Obtained | Downloaded by script, cached in `$MV_BUILD/raw/census-popest/` |
| Read by | `R/build/C_npo_panel/06_npo_assemble.R` (deflates dollars to 2022 $ and sets the $50,000 (2010 $) floor) |
| Licence | The series is BEA public domain. **FRED's own terms (fred.stlouisfed.org/legal) forbid incorporating FRED downloads into another database or archiving them without permission.** |
| Redistribute in release | **Not the FRED download.** Ship the same series from BEA instead |
| Cite as | Source: U.S. Bureau of Economic Analysis, Personal Consumption Expenditures: Chain-type Price Index (NIPA Table 2.8.4, line 1), monthly, retrieved [date]. |
| Frozen copy | `inputs/bea-pcepi/` (BEA copy, once made) |

**Action needed before release:**
- Download PCEPI from BEA: NIPA Table 2.8.4, or the BEA API `NIPA` dataset, table `T20804`, frequency M.
- Check that its annual means equal those of `fred_pcepi.csv` for 1999–2025. They should, because FRED republishes BEA's series. Any revision since the build would show up here.
- Freeze the BEA copy, and point 06 at it, or keep FRED for live builds and document the equivalence.

The floor and every real-dollar series depend on this file, so a revised PCEPI changes
the active-filer universe. Use the frozen copy for rebuilds.
