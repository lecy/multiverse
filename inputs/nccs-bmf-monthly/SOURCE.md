# NCCS monthly BMF (processed), December 2023 and December 2024

| Field | Value |
|---|---|
| Publisher | Urban Institute, National Center for Charitable Statistics |
| Files | `bmf_2023_12_processed.csv` (1.61 GB), `bmf_2024_12_processed.csv` (1.63 GB), with `bmf_<YYYY>_12_data_dictionary.csv` |
| Original URLs | `https://nccsdata.s3.us-east-1.amazonaws.com/processed/bmf/2023_12/bmf_2023_12_processed.csv` and `…/2024_12/bmf_2024_12_processed.csv` (remote last-modified 2026-09-17; the 2024 URL responds on 2026-11-17) |
| Years used | 2023, 2024 (registered stock); December 2024 rulings for entries 2022–24 |
| Obtained | **Manual download** to `$MV_BUILD/raw/bmf_monthly/`. The S3 prefix appears only in a comment in 02b; the script stops if a file is missing |
| Read by | `R/build/B_npo/02b_npo_bmf_monthly.R` (writes `c3_rulings_<year>.csv.gz` into the same folder, a build product, not an input) |
| Licence | ODC-BY 1.0 (NCCS terms §3.5) |
| Redistribute in release | Yes, with attribution |
| Cite as | Urban Institute, National Center for Charitable Statistics, processed monthly BMF, December 2023 and December 2024. |
| Frozen copy | `inputs/nccs-bmf-monthly/` |

**Notes:**
- There is no `OUTNCCS` screen in these files. They are restricted to the 50 states + DC in 02b.
- Geocodes come from the unified BMF (`../nccs-bmf-unified/`).
- **To do:** replace the manual step with `fetch()` on the two URLs. 02b would then download what it reads.
