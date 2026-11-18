# PCE price index (deflator and active-filer floor)

| Field | Value |
|---|---|
| Publisher | U.S. Bureau of Economic Analysis |
| Series | PCE chain-type price index, NIPA Table 2.8.4 line 1, series code `DPCERG`, monthly (2017 = 100) |
| File | `bea_pcepi_monthly.csv` (the DPCERG rows only: `series, period, value, retrieved`) |
| Original URL | `https://apps.bea.gov/national/Release/TXT/NipaDataM.txt` (BEA's monthly NIPA flat file, about 37 MB; series names in `SeriesRegister.txt`) |
| Years used | 1999–2025, annual mean of the monthly index |
| Obtained | Downloaded by script in 06 when `$MV_BUILD/raw/bea-nipa/bea_pcepi_monthly.csv` is missing; only the DPCERG rows are kept and the flat file is deleted |
| Read by | `R/build/C_npo_panel/06_npo_assemble.R` (deflates dollars to 2022 $ and sets the $50,000 (2010 $) floor) |
| Licence | Public domain (U.S. government work) |
| Redistribute in release | Yes |
| Cite as | U.S. Bureau of Economic Analysis, Personal Consumption Expenditures: Chain-type Price Index (NIPA Table 2.8.4, line 1), monthly, retrieved [date]. |
| Frozen copy | `inputs/bea-pcepi/` (not yet made) |

**History.** Until 2026-11-17 the build read the same series from FRED
(`fredgraph.csv?id=PCEPI`, cached as `raw/census-popest/fred_pcepi.csv`). FRED's terms
forbid archiving its downloads, so 06 now reads BEA directly. The annual means for
1999–2025 from BEA (flat file retrieved 2026-11-17, data through 2026-09) are
identical to the FRED file of the reference build and to its `pce_deflator.csv`, so
the switch changes no output.

BEA revises the index with each release, and a revised index changes the active-filer
universe through the floor. Use the frozen copy for rebuilds.
