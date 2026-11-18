# Package plan: making replication a package install

Written 2026-11-18. It follows `REPRODUCIBILITY-PLAN.md`, Phases 0–6: the clean-room build passes and the frozen inputs are on S3.

**Goal:** a replicator installs one R package and chooses how far back to go:

| Level | What it reproduces | Needs | Time |
|---|---|---|---|
| 1. Results | every table and figure in the paper | the final frames and the multiverse runs | minutes |
| 2. Multiverse | the 432,432 v11 specifications | the final frames only | ~35 min on 22 cores |
| 3. Database | the frames themselves | frozen items plus retrievable sources | ~20 min plus downloads |

Today all three levels need this repository, `run.R`, the right `MV_*` folders, and S3 credentials.

---

## 1. Three kinds of data

Every file the project uses falls into one of three classes. Each class has its own home, and the package has one function for each.

### A. Frozen custom items: kept forever, never fetched again

These are files that can't be obtained again, or not as the same bytes: hand-placed files, files behind logins, files with no recorded origin, files bundled by permission, and our own harmonized intermediates. They are frozen once, with sha256 manifests, at `s3://lecy/nonprofit-density/inputs/` (done in Phase 4). A public copy goes on Zenodo.

| Folder | Why it is frozen | Size |
|---|---|---|
| `soi` | Harmonized IRS SOI exempt-org extracts. The sample frame has columns no saved script produces. | 10.0 GB |
| `nccs-bmf-legacy` | 23 legacy BMF snapshots, with no download URL recorded | 7.7 GB |
| `nccs-bmf-monthly` | Manual download; NCCS replaces these files in place | 3.4 GB |
| `nccs-bmf-unified` | A dated copy; the download URL is a moving "latest" path | 0.6 GB |
| `nccs-bmf-long-panel` | Has a methods memo but no producing script | 0.3 GB |
| `patents` | PatentsView files behind a USPTO login and CAPTCHA | 5.0 GB |
| `bls-qcew` | Pre-placed county files; origin unrecorded | small |
| `bls-nonprofits` | The BLS page blocks scripts; no update before 2029 | small |
| `census-hist` | 1970–99 county files, downloaded by hand | 6 MB |
| `religion-census` | RCMS/USRC, bundled **by permission** (`PERMISSION.md`) | 15 MB |
| `social-capital` | NERCRD, bundled **by permission** (`PERMISSION.md`) | 2 MB |
| `check-only` | Files for checks 10b, 10c and 16b; not used in the panels | 1.1 GB |

**Pinned copies of moving sources.** These sources can be downloaded, but their values change: BEA regional accounts, the BEA price index, LAUS, NCCS CORE and the election files.
- The frozen copy is what the build uses, and it defines the published numbers.
- Each keeps its downloader, so a user can rebuild on current data on purpose (`source = "live"`).
- They sit in class A with the flag `moving = TRUE` in the manifest.

### B. Final data frames: the release

These are the build's products that the analysis reads.

**Decided 2026-11-18:** the frames are in the repository itself, in `data/`, as one zstd parquet file per year (largest 4.7 MB), with `MANIFEST.csv` (sha256) and `FRAMES.csv` (rows, columns, sort keys, schema signature). Each version is frozen by a tag. The first tag is `v1.0-data` (commit `de899c2`).
- **Replication:** `replicate.R`, at the repository root, downloads a tag, installs the `renv.lock` versions of the needed packages into a private library, rebuilds the frames, and runs the multiverse.
- **Tested end to end** from an empty library: the v11 decision matrices came out byte-identical to the paper's.
- **Cost:** every new version adds about 160 MB to git history, so frames are committed only when a tag is cut.

The runs below are still too large for git (316 MB of coefficients), so they would go out as Release assets or on Zenodo.

| File | Rows × cols | Size (zstd) | Read by |
|---|---|---|---|
| `cbsa_year.parquet` | 23,125 × 422 | 43 MB | the multiverse engine (`load_panel`), Part I/II |
| `county_year.parquet` | 78,600 × 354 | 92 MB | D2 units, county checks |
| `variable_dictionary.csv` | 321 rows | <1 MB | everyone |
| `run_v11.parquet` (specs + coefs) | 432,432 specs, 5.6M coef rows | ~320 MB | Part II, decision matrices, cards |
| `run_v11_grass.parquet` | grassroots refit | 29 MB | 05-findings |
| `run_v10.parquet` | 605,352 specs | ~250 MB | 05-findings `@tbl-v10-v11` only |
| `spec_dictionary.csv` | one row per fork column and code | <1 MB | *to write* |
| optional: `npo/orgyear_*.parquet` | org-year panels (CORE, SOI, e-file…) | ~1.3 GB | users building their own measures |

The tables behind the paper (`outline/v11/`, `catchup/`, `variables/`…) are **not** released. They are regenerated from these frames by the paper scripts (level 1).

### C. Retrievable sources: fetched on demand

These are public sources with stable, versioned URLs or APIs. The package downloads them into a user cache and checks each file against the sha256 recorded at freeze time. Download, check, then:
- **Match:** use the downloaded file.
- **Mismatch:** stop, explain, and offer the frozen copy.

| Folder | Source | Access |
|---|---|---|
| `census-popest`, `census-pep-char` | Census PEP Vintage 2025 | file URLs |
| `census-saipe`, `census-acs` | Census APIs | `CENSUS_API_KEY` |
| `census-cog` | Census of Governments | file URLs |
| `census-geo` | TIGER 2023 via tigris, OMB 2023 crosswalk | package + URL |
| `irs-soi-county` | IRS SOI county income | file URLs |
| `nccs-efile` | `efile_v3_1` via panel990 (pinned commit) | package |
| `census-cbp` | County Business Patterns, NAICS 551114 | API (*new, in progress*) |
| `sec-edgar` | EDGAR index, filing headers, FSDS | needs a SEC User-Agent (*new, in progress*) |

**Rule for later sources:** a new source goes in class C only if its URL is versioned and its terms allow scripted access. Otherwise it goes in class A on the day it is added.

**One manifest for all three classes.** `inst/extdata/manifest.csv` (in the package) has one row per file:
`class, folder, key, bytes, sha256, moving, url_or_api, s3_key, zenodo_file, steps`.
It merges today's per-folder `MANIFEST.csv` files with the release manifest. The package reads only this file to decide where a file comes from.

---

## 2. Package design

**Name:** to decide (§6). Placeholder `npdensity`.

**Two parts in one repository:**
- `pkg/` is the R package: functions, the manifest, data documentation and tests. It installs with `remotes::install_github("lecy/multiverse", subdir = "pkg")`.
- The repository root stays the research compendium: `R/build/` steps, `run.R` with `pipeline.csv`, `R/paper/`, `paper/`, `site/` and `notes/`. Its scripts call `library(npdensity)` instead of `source("R/lib/...")`.

The pipeline isn't moved into package functions. Most of the 3,900 build lines are one-off transformations that read best as scripts. The package holds what is reused: data access, helpers, the multiverse engine, and fetchers.

### What goes into the package

| From | Into the package as | Notes |
|---|---|---|
| `R/lib/config.R`, `00_setup.R` (paths) | `mv_paths()`, `mv_set_paths()` | No globals set by `source()`. Paths come from options or env, with `tools::R_user_dir("npdensity", "cache")` as the default. |
| `00_setup.R` NTEE rule (`bad_ntee`, `resolve_ntee`, `ntee_best_from`) | `ntee_*()` | Exported and documented: it defines the sector. |
| `geo_helpers.R` | `geo_*()` | County recodes, CT planning regions, ZIP and lat/lon to county. |
| `13_16_cov_helpers.R`, `fetch()`, `fetch_ua()` | `mv_fetch()` | One fetcher with User-Agent, retries, cache and the sha256 check against the manifest. |
| `40_multiverse.R` | `mv_grid()`, `mv_run_spec()`, `mv_run_universe()`, `mv_blocks()`, `flog()`, `slog()`, `winsor()` | `MV_MODEL=lean` becomes the argument `model = "lean"`. The grid is a value, not a global. |
| `41_composites.R` | `mv_composites()` | |
| `44_run_universe.R` (batching, resume, combine) | `mv_run_universe(grid, panel, workers, dir)` | The script becomes a three-line wrapper. |
| `45`, `97`, `98` summaries | `mv_summarize()`, `mv_decision_importance()`, `mv_decision_matrix()` | These produce the published numbers, so they need tests. |
| *new* | `mv_get_frame("cbsa_year", release = "v1.0")` | Downloads the release asset, checks it, caches it, and returns a `data.table`. |
| *new* | `mv_get_run("v11")` | Same, for a run. |
| *new* | `mv_get_inputs(folders, dest)` | Class A from the public mirror or Zenodo; replaces `env/restore_inputs.R`. |
| *new* | `mv_fetch_source("census-saipe", dest)` | Class C downloaders, split out of the D_cov scripts. |
| `docs/variable_dictionary.csv` | `?cbsa_year`, `?county_year` (roxygen data docs) and `mv_dictionary()` | Generated from the CSV, so the CSV stays the source. |
| `inputs/*/SOURCE.md` | `vignette("sources")` and `mv_sources()` | Licence and citation for each source. |

### What stays as scripts

- `R/build/` steps 01–30, refactored only so that each step's download part calls `mv_fetch_source()` and the rest stays a transformation. That separates class C fetching from building, which the clean room showed is the fragile part.
- `run.R` and `pipeline.csv`. They work and passed the clean-room test, so there's no move to `{targets}` now (§6).
- `R/paper/` producers, `R/paper/make_figures.R` and `paper/figures.csv`.
- `R/notes/` exploratory scripts (33 files, 8,300 lines). They're not in the package or the release, and stay as a record.

---

## 3. Work plan

*Progress, 2026-11-18:*
- **Step 1a done.** `pkg/` is the package `npdensity` (name not final). It has:
  - the NTEE rule (`bad_ntee`, `ntee_best_from`, `read_ntee_best`, `resolve_ntee`);
  - `pad`, `log_msg`, `recode_county` and the constants;
  - `mv_fetch` (download to a temporary file, check sha256, then move into place), `file_sha256`;
  - `winsor`, `resid_logP`, `slog`, `flog`;
  - `mv_read_frame` (rebuilds a frame from the `data/` slices, with checks) and `frame_signature`.
  - Tests cover each function. `R CMD check`: 0 errors, 0 warnings, 0 notes.
- **Step 1b next:** switch `R/build`, `R/multiverse` and `R/paper` from `source("R/lib/...")` to the package. This waits until the corporate-HQ session has committed its edits to `pipeline.csv` and step 30. The geo helpers move in 1b, because they read build files at load time.

Each step ends with the clean-room check from Phase 5: rebuild in `D:/MV_CLEAN` and compare with `env/compare_builds.R`. A refactor step must reproduce the frames and v11 byte for byte, or cell for cell.

**Step 1: skeleton and helpers (1 day)**
- `usethis::create_package("pkg")`, DESCRIPTION with the imports from `renv.lock` that the package needs, MIT licence for the code.
- Move the helpers (NTEE, geo, `flog`/`slog`/`winsor`, `mv_fetch`) with roxygen docs and unit tests on small fixtures.
- Replace `source( "R/lib/00_setup.R" )` across `R/build`, `R/multiverse` and `R/paper` with `library( npdensity ); mv_paths()`. Done means the clean room reproduces.

**Step 2: multiverse engine (1–2 days)**
- Move `40`, `41` and the batching of `44` into `mv_*()` functions. Remove the global `BLOCKS`/`MV_MODEL`.
- **Regression test:** a fixed sample of 500 v11 spec_ids must give the stored coefficients to 1e-9, plus the full rerun in the clean room (35 min).
- Move the summaries behind the paper (45, 97, 98) and test them against the v11 tables in `outline/v11/mv*`.

**Step 3: the manifest and data access (1 day)**
- Build `inst/extdata/manifest.csv` from the S3 `MANIFEST.csv` files plus the release files.
- `mv_get_frame()`, `mv_get_run()`, `mv_get_inputs()`, with cache, sha256 check and a clear message on mismatch.
- Write `spec_dictionary.csv`: one row per fork column (`construct`, `hu`, `ident`, `op`, `dv_form`, `pred_form`, `rhs_logP`, `unit`, `est`, `cs_year`, `outlier`, `drop`, `op_block`) and code.

**Step 4: fetchers for class C (2 days)**
- Split the download parts of steps 01, 11–17, 19, 22, 27 and 28, and `panel990` for 05, into `mv_fetch_source()`. Each source's `SOURCE.md` names its URL.
- `source = "frozen" | "live"`: frozen is the default and gives the published numbers.
- Script the manual sources that can be scripted (`census-hist`, the monthly BMF; see `inputs/README.md`). Even then they stay in class A, because the files can change.

**Step 5: the release (½ day, after the paper is updated)**
- Tag `v1.0`. Attach the class B files and the manifest to the GitHub release. Zenodo picks the release up and issues a DOI.
- Make the class A mirror public: a read-only public S3 prefix, or a Zenodo record of the frozen folders (33 GB; Zenodo allows 50 GB per record). The RCMS and NERCRD permissions must cover the public copy.

**Step 6: documentation (1 day)**
- A pkgdown site in the NODC style (the `nodc-pkgdown` skill), with vignettes:
  - **Reproduce the paper (level 1):** `mv_get_frame()` plus `mv_get_run()`, then render `paper/`.
  - **Rerun the multiverse (level 2):** `mv_run_universe(mv_grid("principled", model = "lean"), mv_get_frame("cbsa_year"))`.
  - **Rebuild the database (level 3):** `mv_get_inputs()`, `mv_fetch_source()`, `run.R database`.
  - **Sources and licences:** generated from `inputs/*/SOURCE.md`.
- It can merge with the replication website the other session is building (`_quarto.yml`, `site/`), or link to it.

**Step 7: CI (½ day)**
- GitHub Actions: `R CMD check` on the package, and the 500-spec regression test against a small cached frame. No full builds in CI.

**Total: about 7–9 working days**, steps 1–3 first. Steps 1–3 already make levels 1 and 2 work from an install.

---

## 4. What a replicator would run

```r
remotes::install_github( "lecy/multiverse", subdir = "pkg" )
library( npdensity )

panel <- mv_get_frame( "cbsa_year", release = "v1.0" )   # 43 MB, checked, cached
v11   <- mv_get_run( "v11", release = "v1.0" )           # level 1: results as published

grid <- mv_grid( "principled", model = "lean" )          # level 2: rerun
res  <- mv_run_universe( grid, panel, workers = 8, dir = "run_v11" )

mv_get_inputs( dest = "inputs" )                          # level 3: class A, ~33 GB
mv_fetch_source( "all", dest = "inputs" )                 # class C, checked against the manifest
# then: Rscript run.R database   (from the compendium)
```

---

## 5. Risks

- **Byte-for-byte reproduction across the refactor.** Moving code can change argument defaults or the order of operations. The clean-room diff after each step catches this, as it caught 05's years and step 01's population file.
- **Class C drift.** A "stable" URL can still change. The sha256 check turns silent drift into a clear stop, and the frozen copy is always the fallback.
- **Permissions.** RCMS and NERCRD may allow bundling with the release but not a public mirror. Confirm the scope of each permission before Step 5.
- **The NCCS e-file licence** is still "ODC-BY, to confirm" (`inputs/README.md`).
- **Notes scripts** use the same helpers. After Step 1 they need `library(npdensity)` too, or they break. They aren't in the clean room, so check them by running a sample.

## 6. Decisions needed

1. **Package name.**
2. **Same repository (`pkg/` subfolder) or a separate package repository.** Recommendation: same repository until v1.0, then split if the package gets users of its own.
3. **Public home for class A:** a public S3 prefix, requester-pays S3, or Zenodo. Recommendation: Zenodo, for a DOI and no egress cost, with S3 kept as the working mirror.
4. **Whether to release the org-year panels** (1.3 GB) with v1.0 or later.
5. **`{targets}` instead of `run.R`:** not now. `run.R` passed the clean room. Revisit if the pipeline grows.
6. **Data licence for the release:** ODC-BY at least (NCCS), with the permissions attached for the bundled sources.
