# County presidential election returns, 2000–2024

| Field | Value |
|---|---|
| Sources | 2000–2016: MIT Election Data and Science Lab (MEDSL), `countypres_2000-2016.csv`. 2020 and 2024: Tony McGovern, `US_County_Level_Election_Results_08-24` (`2020_US_County_Level_Presidential_Results.csv`, `2024_…`). Alaska: certified statewide totals from the Alaska Division of Elections, typed into 16 |
| Original URLs | `https://raw.githubusercontent.com/MEDSL/county-returns/master/countypres_2000-2016.csv`; `https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/{2020,2024}_US_County_Level_Presidential_Results.csv` |
| Years used | Presidential years 2000–2024 |
| Obtained | Downloaded by script, cached in `$MV_BUILD/raw/cov/elections/` as `countypres_2000-2016.csv`, `tonmcg_2020.csv`, `tonmcg_2024.csv` |
| Read by | `R/build/D_cov/16_cov_elections.R`. Checks in `R/build/F_checks/16b_verify_elections.R` use FEC 2020 and MEDSL 2024 official returns (`elections/fec/`, `elections/medsl_2024/`); they are verification inputs only |
| Licence | MEDSL: CC0 1.0 (Harvard Dataverse doi:10.7910/DVN/VOQCHQ). tonmcg: MIT License, "Copyright (c) 2025 Tony McGovern". Alaska and FEC figures: public records |
| Redistribute in release | Yes. Keep the MIT copyright and licence notice with the tonmcg files |
| Cite as | MIT Election Data and Science Lab, County Presidential Election Returns 2000–2016 (doi:10.7910/DVN/VOQCHQ); McGovern, T., US County Level Election Results 08–24 (GitHub). |
| Frozen copy | `inputs/elections/` (the three files 16 reads, plus `LICENSE-tonmcg.txt`; optionally the 16b verification files) |

**Drift risk:** both GitHub URLs point at `master`, unpinned. Record the commit hash
of each repository when freezing. The MEDSL GitHub repo is the older home of the data;
the maintained version is the Dataverse deposit, which now runs 2000–2024.

**Alaska:** the hand-entered totals are in the code (16, lines 106–109) with no
source document. Add the Division of Elections URL for each year in a comment.
