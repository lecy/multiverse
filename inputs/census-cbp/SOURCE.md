# Census County Business Patterns: NAICS 551114 managing offices

| Field | Value |
|---|---|
| Publisher | U.S. Census Bureau, County Business Patterns (CBP) |
| Files | `cbpYYco.zip`, the complete county file, one per year 2000–2023 (24 files, about 340 MB); `cbpYYus.zip`, the national file, 2008–2023 (16 files, about 60 MB) |
| Original URL | `https://www2.census.gov/programs-surveys/cbp/datasets/<YYYY>/cbp<YY>co.zip` and `.../cbp<YY>us.zip` |
| Years used | 2000–2023. CBP 2024 was not yet released when this was built (the 2024 folder returned 404 on 2026-11-18); 28 picks it up when the file appears |
| Obtained | Script (`R/build/D_cov/28_cov_corporate_hq.R`), cached in `$MV_BUILD/raw/census-cbp/`. No API key needed |
| Read by | `R/build/D_cov/28_cov_corporate_hq.R` (rows with `naics == "551114"` only) |
| Licence | U.S. government work, public domain |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Census Bureau, County Business Patterns, 2000–2023, NAICS 551114. |
| Frozen copy | `inputs/census-cbp/` |

**Why the bulk files and not the API:** the CBP API uses a different NAICS variable
in each vintage (`NAICS2002`, `NAICS2007`, `NAICS2012`, `NAICS2017`), returns no data
for some early years under the names tried, and its `EMPSZES` size-class rows are
incomplete for 2012–2016. The bulk county files have the same layout in every year,
with establishment counts by employment-size class.

**NAICS vintages.** 551114 ("Corporate, subsidiary, and regional managing offices")
has the same code and definition in NAICS 1997, 2002, 2007, 2012, 2017 and 2022.

**Breaks the build handles:**
- **2017 disclosure change.** From 2017 the CBP adds noise to employment instead of
  suppressing it, but it no longer publishes cells with fewer than 3 establishments,
  and it withholds some size-class counts (`"N"`). Absent counties are counted as 0
  (flag `cbp_hq_nosmall`); withheld size classes are allocated from the national
  file (see the script header).
- **Employment suppression before 2017.** Suppressed cells (flags A–M, with
  `emp = 0`) are imputed from the size classes, within the flag's range.
- **Connecticut.** The county files use the legacy counties in every year; 28 moves
  them to planning regions with `harmonize_counties()`.

The national files before 2008 are not used (their layout differs). The county
files are large because they hold every industry; only the 551114 rows are read.
