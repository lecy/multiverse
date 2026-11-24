# Research frames

These are the two final research frames of the nonprofit-density project: one row per place and year, 2000–2024, with nonprofit outcomes and every covariate the multiverse uses. They are stored as one parquet file per year, so each file stays small enough for git. Each tagged release of this repository freezes one version of them.

| Frame | Unit | Rows × columns | Files | Size |
|---|---|---|---|---|
| `cbsa_year/` | 2023 CBSA (metro and micro, 925) × year | 23,125 × 433 | `cbsa_year_2000.parquet` … `_2024` | 50 MB |
| `county_year/` | county-equivalent, 2023 delineation (CT planning regions) × year | 78,600 × 365 | `county_year_2000.parquet` … `_2024` | 110 MB |

- **`variable_dictionary.csv`:** one row per variable, giving its group, the build step that makes it, which frame carries it, whether it is gap-filled, and a description. The gap-fill variants `_obs`, `_lin` and `_locf` share their base variable's row.
- **`MANIFEST.csv`:** each file's frame, year, rows, columns, bytes and sha256.
- **`FRAMES.csv`:** rows, columns, years, sort keys and a signature of the column names and types for each frame. It also records the sha256 of the single-file frame the slices were cut from.

## Read

R reads either frame as one table:

```r
library( arrow ); library( data.table )
cbsa <- rbindlist( lapply( sort( list.files( "data/cbsa_year", full.names = TRUE ) ), read_parquet ) )
setorder( cbsa, cbsa, year )
```

`arrow::open_dataset( "data/cbsa_year" )` gives the same data, lazily. Python can read the folder with `pandas.read_parquet( "data/cbsa_year" )`.

## Replicate the multiverse

Download [`replicate.R`](../replicate.R) on its own and run it (`Rscript replicate.R`). It:
1. Creates a local folder (`~/multiverse-replication`; set `MV_DIR` to change it).
2. Downloads this release's code and data from GitHub.
3. Installs the needed packages at their `renv.lock` versions in a private library.
4. Calls `R/multiverse/run_from_release.R`, which:
   - checks every slice against `MANIFEST.csv`, rebuilds both frames, and checks them against `FRAMES.csv`;
   - runs the 432,432 specifications of run v11 (`44_run_universe.R` with `MV_MODEL=lean`);
   - summarizes the run (45), refits grassroots (104), and draws the decision matrices (98).

It takes about 35 minutes on 22 workers. With `MV_RUN_V10=TRUE` it also runs v10, the paper's comparison run, and the v10/v11 tables (97). From a clone of the repository, `Rscript R/multiverse/run_from_release.R [workers] [out_dir]` runs step 4 alone.

## How these files were made

- `Rscript run.R database` built the frames from the frozen inputs (`inputs/`, `REPRODUCIBILITY-PLAN.md` Phases 4–5). The build ran in a clean room: a fresh package library, and inputs restored from the frozen copies with sha256 checks.
- `env/release_slices.R` cut them into these files.
- A rerun of v11 from the clean-room frames matches the stored v11. All 432,432 spec ids and sample sizes are identical, 94.5% of coefficients are bit-identical, and no significance verdict changes.

Code version: the release tag on this commit.

**Versions:** `v1.0-data` had 422 (CBSA) and 354 (county) columns. This version adds the 11 corporate-headquarters columns of step 28 to both frames: `hq_pub`, `hq_pub_large`, `cbp_hq_est`, `cbp_hq_est100`, `cbp_hq_emp`, `cbp_hq_emp_imp`, `cbp_hq_est100_imp` and `cbp_hq_nosmall`, plus the per-100,000 rates `hq_pub_per100k`, `hq_pub_large_per100k` and `cbp_hq_est_per100k`. Every other column is identical to `v1.0-data`. The frames come from a rebuild with the package code (npdensity, PACKAGE-PLAN.md Step 1b) from the frozen inputs, including the `sec-edgar` and `census-cbp` freeze. CBP covers 2000–2023, so the CBP columns are missing for 2024.

## Sources, licences and citation

The frames combine the sources documented in `inputs/` (one `SOURCE.md` per source: publisher, URL, years, licence and citation). In short:

- **Public domain (U.S. government):**
  - Census: PEP, SAIPE, ACS, Census of Governments, TIGER/OMB delineations, historical county estimates, County Business Patterns.
  - BEA regional accounts and the PCE price index.
  - BLS: QCEW and the nonprofit research data.
  - USDA ERS (LAUS), IRS SOI county income and exempt-organization extracts.
  - SEC EDGAR company filings (public-company headquarters, step 28).
  - Census API notice: *This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.*
- **ODC-BY 1.0:** NCCS (Urban Institute) BMF, CORE and e-file data. Attribute: Urban Institute, National Center for Charitable Statistics.
- **CC BY 4.0:** PatentsView (USPTO). Attribute: PatentsView, www.patentsview.org.
- **CC0 / MIT:** MEDSL election returns (CC0); tonmcg county results 2020/2024 (MIT, © 2025 Tony McGovern).
- **By permission:**
  - **Religion:** U.S. Religion Census / RCMS (© ASARB; data via ARDA). Credit ASARB and ARDA.
  - **Social capital:** Rupasingha, Goetz and Freshwater, NERCRD (Penn State).
  - The frames carry only county and CBSA measures derived from these files. Permission to bundle the source files with the release was received on 2026-11-17. The written permissions are still to be added as `inputs/religion-census/PERMISSION.md` and `inputs/social-capital/PERMISSION.md`.

Released under ODC-BY 1.0, with the attributions above. Cite the release (tag and DOI once deposited) and the sources you use.
