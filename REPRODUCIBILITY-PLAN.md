# Reproducible build plan: nonprofit density database

Drafted 2026-11-17 from a review of `R/` (73 scripts, ~16,400 lines), `D:/MULTIVERSE`,
the Dropbox data hub, `paper/` and `s3://lecy/multiverse/`. Goal: anyone with the
code and the S3 release can rebuild the project database (and, optionally, the
multiverse run and paper exhibits) with one command, and get the same files.

---

## 1. Where things stand

### 1.1 Scripts are three projects in one numbered sequence

| Kind | Scripts | Notes |
|---|---|---|
| **Database build** | 01, 02, 02b, 03, 03b, 04, 05, 06, 07, 08, 09a, 09, 11–19, 20, 21, 22, 23, 30 | Writes `data/{geo,pop,npo,cov,analysis}` |
| **Multiverse engine and runs** | 40, 41, 42–46, 51, 53, 54, 55, 57, 82, 85, 92, 93 | Fork scripts both build covariates and run specs |
| **Research notes and paper exhibits** | 09b, 16b, 50, 52, 56, 58, 60, 70–81, 83, 84, 86–91, 94–99 | Many are exploratory; some feed the paper |

The README table lists scripts by number, not by run order. For example, 2b must run
after 5, 23 needs 06, 6 is listed after 9, and 60 is listed before 30. Other sessions
are still adding scripts (`99_interaction_screen_v11.R` appeared today at 12:43).

### 1.2 The dependency graph has cycles and hidden build steps

1. **Circular reads.** `07_npo_bls.R` (validation part) and `20_urban_economy.R`
   (`bench` part) read `data/analysis/cbsa_year.parquet`, which is the output of 30.
   But 30 also reads their outputs (`cov/cbsa/cbsa_bls.csv`, `cov/cov_qcew.csv`).
   This only works today because an earlier build already exists. A clean build
   would fail.
2. **Three panel covariates are built inside analysis scripts.** `load_panel()` in
   40 merges them at run time. 30 skips them because they are keyed by CBSA, not
   county:

   | File | Built by | Depends on |
   |---|---|---|
   | `cov_endowment.csv` | `55_endowment_fork.R build` | `npo/ein_location_bmf.csv.gz` |
   | `cov_lag_kernel.csv` | `82_lag_fork.R build` | `raw/census-hist`, `catchup/c9_metro_1970_2024.csv` (from `73_pre2000_growth.R`) |
   | `cov_gini_metro.csv` | `89_need_socap_diversity.R build` | ACS API |

   The "database" is therefore spread across `cbsa_year.parquet` plus three side
   files, two of which depend on research scripts.
3. **Optional inputs with silent fallbacks.** 06 uses `orgyear_grants.parquet`,
   `bmf_county_year_{monthly,recon}.csv` and `npo_entry_county_year.csv` only if
   they exist (`if (file.exists(...))`). If the order is wrong, the run still
   succeeds but builds a different database.

### 1.3 Portability blockers

- `C:/Users/jdlec/Dropbox (Personal)/...` is hard-coded in `setwd()` for parallel
  workers in 16 scripts: 42, 44, 51, 53, 55, 57, 82, 83, 85, 86, 87 (×2), 88, 89,
  90, 92, 93.
- `D:/MULTIVERSE` is hard-coded in `40_multiverse.R` (`load_panel`) and in 58.
  `00_setup.R` hard-codes `DROPBOX`, `DATA_HUB` and `BUILD`.
- **Census API key:** 17, 22 and 89 read it from `../CENSUS-DATA.R`, outside
  `multiverse/`. That file must never be uploaded. Switch to the standard
  `CENSUS_API_KEY` environment variable.
- **No package lockfile.** The project uses R 4.5.1 and about 20 packages
  (`data.table`, `arrow`, `fixest`, `sf`, `tigris`, `tidycensus`, `ggplot2`,
  `patchwork`, `lme4`, `psych`, `survival`, …). **`panel990` 0.0.0.9000 was installed
  with no recorded source.** It drives the 2020–24 e-file data, so its exact commit
  has to be pinned.
- `md2qmd.R` falls back to the Quarto that ships inside RStudio.
- The project is **not under version control**.

### 1.4 Inputs come from four places (about 65 GB)

| Source | Location | Size | Rebuildable by download? |
|---|---|---|---|
| NCCS BMF legacy snapshots 1989–2022 | `00 - DATA/00-NCCS/bmf` | 27 GB (only the years used are needed) | Partly (NCCS S3) |
| SOI extracts, unzipped and final | `00 - DATA/SOI/01_data/{01_unzipped,04_final}` | 3.8 + 8.9 GB | **Unknown:** `04_final` looks like the output of an earlier pipeline; document or include the script that made it |
| SOI panel sample frame | `00 - DATA/SOI/02_sample-framework/…AUGMENTED.CSV` | 1.6 GB | Same question |
| BMF long panel 1995–2022 | `00 - DATA/BMF/bmf-long-panel-1995-2022.rds` | 258 MB | Same question |
| CBSA crosswalk, old population file | `00 - DATA/population` | 1 MB | Yes |
| BLS QCEW nonprofit MSA files | `00 - URBAN/00-DATA/bls` | 62 MB | Yes (BLS research data) |
| Auto-download caches | `D:/MULTIVERSE/raw/{census-popest,nccs-core-hrmn,panel990,bmf_monthly,cov/*}` | about 9 GB | Mostly, but URLs drift (e.g. the 14_cov_laus URL carries `?v=78722`) |
| Manual downloads | `raw/patents` (USPTO login + CAPTCHA), `raw/census-hist`, `raw/cov/rcms/*.xlsx`, `raw/cov/socap/*.xlsx`, election verification files | 15 GB (patents) | **No**, or not without a login |

`raw/patents/223582-V1.zip` (7.4 GB, the unusable DataLumos mirror) and the
`*.tsv.zip` files duplicate the unzipped TSVs. They do not need to be shipped.

### 1.5 The paper is not linked to code

- The `paper/*.qmd` files have **no code chunks**. Numbers are typed in by hand, and
  the 47 PNGs in `paper/figures/` were copied in by hand, except the `d*_lean_*`,
  `decision_matrix_*` and `var_*` figures, which 94–98 write directly.
- By hash, two figures point to stale or ambiguous sources:
  - `f4_change_correlates.png` matches only `data/_pre_flog_20261116/catchup/fig_c7_moderators.png`, the **pre-flog backup**. The current `catchup/` version differs.
  - `f7_hazards.png` matches `data/outline/fig_fact7_hazards.png`, the unversioned outline root, not `outline/v10/`.
- Part I cites `outline/v10`, and Part II cites run v11. v10 predates the NTEE "UND"
  fix of 2026-11-17, so its 2020+ nohu spending specs are stale.

### 1.6 Multiverse runs

- **v11** (lean, 432,432 specs) is a single command:
  `MV_MODEL=lean` + `44_run_universe.R principled 2000 14 principled_v11`, taking
  87 minutes. That is easy to reproduce.
- **v10** was built as a chain: v6 → `82` → v7 → `85` → v8 → `92` → v9 → `93` → v10.
  Every add-on block now lives in `BLOCKS`, so one `44 principled` run should
  regenerate v10 directly. That needs a check: spec_ids and coefficients against
  the stored v10.
- `data/spec` holds 25 run folders (3.8 GB). Only v11, and v10 if the paper still
  cites it, belong in a release.

### 1.7 Storage and S3

- `s3://lecy/multiverse/` is a raw, private mirror of `D:/MULTIVERSE`, including
  `tmp/` (950 MB of scratch), `logs/` and loose `tmp_*.R` files. It works as a
  backup but is not a distribution.
- Random seeds: all simulation and CV scripts set seeds, except
  `09b_revexp_analysis.R`, which is a research note.

---

## 2. Target organisation

### 2.1 Repository layout (Dropbox `multiverse/`, under git)

```
multiverse/
  README.md                 what this is, requirements, how to run, data sources
  LICENSE, CITATION.cff
  renv.lock                 package versions (panel990 pinned to a commit)
  config.R                  paths from env vars: MV_BUILD, MV_INPUTS, CENSUS_API_KEY
  run.R                     single entry point: Rscript run.R [database|multiverse|paper|all]
  pipeline.csv              step table: id, script, args, stage, inputs, outputs
  R/
    lib/                    00_setup (no hard-coded paths), geo_helpers, cov_helpers,
                            multiverse engine (40), composites (41)
    build/                  database: one script per source, in run order
      A_geo/      01
      B_npo/      02, 02b, 03, 03b, 04, 05, 08, 09a, 09
      C_npo_panel/06, 23
      D_cov/      07 (BLS part), 11-19, 20 (QCEW part), 21, 22,
                  + endowment (from 55 build), pre-1970 population (from 73),
                  lag kernel (from 82 build), metro Gini (from 89 build)
      E_assemble/ 30 (merges the CBSA side covariates too) + codebook + manifest
      F_checks/   07 validation, 16b, 20 bench, splice/coverage diagnostics
    multiverse/             42-46, 44 run v11 (+ v10), 45, 97, 98
    paper/                  scripts that write every paper/figures/* file and
                            the numbers tables the text cites
    notes/                  research-note scripts kept for the record, not run by run.R
  paper/  notes/  references.bib
```

**Numbering:** keep the old numbers as prefixes inside the stage folders
(`build/D_cov/12_cov_bea.R`). Then the existing notes, README and memory still
resolve, and `pipeline.csv` holds the true order. Full renumbering would break
hundreds of cross-references for little gain.

### 2.2 Runner

The recommended runner is a plain `run.R` driven by `pipeline.csv`, not `targets`.
The scripts are file-in/file-out, so wrapping them needs no refactor. `run.R`:

- runs each step in a fresh `Rscript` process, with its log in `MV_BUILD/logs/`;
- skips a step whose outputs are newer than its inputs and script (make-style),
  or forces a rebuild with `--from <id>`;
- fails loudly when a declared input is missing. This replaces the silent
  `file.exists()` fallbacks in 06;
- at the end, writes `MANIFEST.csv` (path, bytes, sha256, step, timestamp).

`targets` can come later if the analysis layer needs finer caching.

### 2.3 What "the project database" is

The release is one folder, `database/`:

- `cbsa_year.parquet` and `county_year.parquet`: every covariate merged, including
  endowment, lag kernel and `gini_m`, so `load_panel()` reads one file;
- `npo_county_year.csv`, `npo_cbsa_year.csv`, `bmf_county_year_recon.csv` and
  `npo_entry_county_year.csv`;
- the org-year parquets (`orgyear_*.parquet`, about 1.3 GB). These are optional,
  but they are what make the splice auditable;
- `codebook.csv` (merged from the per-source codebooks, plus source, years and type
  for each variable), `diagnostics/` and `MANIFEST.csv`.

---

## 3. S3 distribution design

Keep the working mirror (`s3://lecy/multiverse/`) as it is, as a backup. Publish the
release to a separate, immutable, versioned prefix:

```
s3://lecy/nonprofit-density/v1.0/          (or a dedicated public bucket)
  README.md  MANIFEST.csv  LICENSE  CITATION.cff
  code/        multiverse-code-v1.0.zip    (git tag v1.0, no secrets)
  inputs/      frozen source files, one folder per source, each with SOURCE.md
               (URL, access date, license, sha256)
  database/    the folder in 2.3
  runs/        run_principled_v11/  (+ v10 if cited)
  paper/       figures + numbers tables
```

- **Access:** `s3://lecy` is private, and `lecy-data` can only touch that bucket.
  Public distribution needs a public-read bucket policy on the release prefix, a
  separate public bucket, or requester-pays. Egress is about $0.09/GB, and a full
  input download is about 50 GB. Recommendation: make `database/` and `code/`
  public, and put `inputs/` behind requester-pays or presigned links. Also deposit
  `code/` + `database/` (about 2 GB) on Zenodo for a DOI. **The IAM and bucket
  policy changes are yours to make.**
- **Redistribution licences (check before publishing `inputs/`):**
  - Federal sources are public domain: IRS, Census, BEA, BLS, USDA ERS and FEC.
  - PatentsView is CC BY 4.0.
  - Check the terms for NCCS (BMF, CORE, e-file), the US Religion Census 2020
    (ARDA terms), Rupasingha social capital (PSU), the tonmcg election compilation
    and MEDSL.
  - For anything not clearly redistributable, ship the download script, URL and
    sha256 instead of the file.
- **Never upload:** `CENSUS-DATA.R` or any API key, `tmp/`, `Rtmp*`, `_pre_*`
  backups, or superseded runs.

---

## 4. Work plan

Each phase ends with a check against the current outputs. Phases 1–3 change no
results.

**Phase 0: freeze and version (½ day)**
1. `git init` in `multiverse/` with a `.gitignore` for `*.html`, `.quarto/`,
   `*.pdf`/`*.docx` drafts, then commit as-is. Optionally push to a private GitHub
   repo.
2. Agree a code freeze with the other sessions. From then on, new scripts go in the
   right stage folder, and other sessions add rows to `pipeline.csv`.
3. Record `sessionInfo()` and find `panel990`'s source (GitHub commit or local
   tarball).

*Status 2026-11-17:*
- **Step 1 done.** Git repo on branch `main`, first commit `6e8c3a5` (253 files, 30 MB). `.gitignore` excludes secrets, rendered HTML, Quarto caches, Office/PDF drafts, data and logs. `.gitattributes` keeps LF. Remote: private GitHub repo `lecy/multiverse` (`origin`, first push 2026-11-17). Code, documents and figures only; data stays on D: and S3.
- **Step 3 done.** `env/session-info-2026-11-17.txt` and `env/packages-2026-11-17.csv`. `panel990` = GitHub commit `7045d97` (in its DESCRIPTION `RemoteSha`; still HEAD).
- **Step 2 done.** The code freeze was sent on 2026-11-17 to the 9 local sessions on this project. The rules:
  - Don't commit, branch or reset unless your user asks.
  - Leave `00_setup.R`, the helpers, scripts 01–30 and 40/41 alone without telling your user they will be refactored.
  - New scripts take a free number and a README row.
  - Keep data on D:.
  - Note the source of each paper figure.
- **Note:** the repo lives in Dropbox. Work on it from one machine at a time, because Dropbox can corrupt `.git` when two machines write to it.

**Phase 1: portability, with no change in behaviour (1 day)**
1. `config.R`: `MV_BUILD`, `MV_INPUTS` (replaces `DATA_HUB` and the `00 - URBAN`
   path) and `CENSUS_API_KEY` from env vars or a git-ignored `.Renviron`. The
   defaults keep today's D:/Dropbox paths.
2. Replace the 16 `setwd("C:/Users/...")` worker calls with a `PROJECT_ROOT` that is
   passed in. Remove `D:/MULTIVERSE` from 40 and 58.
3. `renv::init()` + snapshot, with panel990 pinned.
4. Check: rerun 30 and a 1,000-spec slice of v11. The outputs must be
   byte-identical (or identical cell by cell).

*Status 2026-11-17 (second machine, user `jlecy`, R 4.5.3):*
- **Step 1 done.** `R/config.R`, sourced first by `00_setup.R`, reads `MV_BUILD`, `MV_INPUTS` (data hub), `MV_BLS`, `MV_DROPBOX` and `CENSUS_API_KEY`. Defaults keep the original machine's layout: `D:/MULTIVERSE`, and Dropbox = four folders above the project. Set the variables in the user-level `~/.Renviron` (Documents/.Renviron), never in the project, because the project syncs. On this machine `MV_BUILD=C:/Users/jlecy/Documents/MULTIVERSE`. `census_key()` replaces the three copies in 13–16, 17 and 89; it falls back to `../CENSUS-DATA.R`.
- **Step 2 done.** 21 worker `setwd()` calls in 20 scripts (including 99–104) now use `Sys.getenv("MV_PROJECT_ROOT")`, which `config.R` sets. `D:/MULTIVERSE` is gone from 40 (`load_panel`), 58 and 60.
- **Step 3 done, without activating renv in the project.** An renv autoloader in the Dropbox folder would switch the other machine's sessions to an empty library. `env/build_renv_lock.R` builds the library in `$MV_BUILD/renv-build` and writes `renv.lock`. It installs every version in `env/packages-2026-11-17.csv` (all 22 match), with `panel990` at `7045d97c` and dependencies from the Posit Package Manager snapshot of 2026-11-16. Use it by setting `R_LIBS_USER` to that library, or with `renv::restore(lockfile = "renv.lock")`. The lockfile records R 4.5.3; the original build used 4.5.1.
- **Step 4 done.** Inputs and reference outputs came from `s3://lecy/multiverse/data` (needs the `lecy-data` profile).
  - **30:** `cbsa_year.parquet` (23,125 × 418) and `county_year.parquet` (78,600 × 354) are identical cell by cell (`env/compare_parquet.R`), with the same column order and types. The file hashes differ, which points to parquet metadata.
  - **v11 slice:** `env/check_v11_slice.R` uses 1,000 specs drawn with seed 20261117. The 432,432-spec grid is identical, and all 1,000 statuses and all 12,991 coefficient rows are reproduced. Every OLS, fixed-effects and Poisson spec is exact. 21 of the 190 `state_re` specs (lme4 random effects) differ, by at most 1e-6 relative. That fits the optimizer converging to within its tolerance, not a change in the code.

**Phase 2: fix the dependency graph (1–2 days)**
1. Split 07 into `07_npo_bls.R` (build) and `F_checks/07b_bls_validation.R`. Split
   20 into `qcew` (build) and `F_checks/20b_benchmarks.R`.
2. Move the builders for `cov_endowment`, `c9` pre-1970 population, `cov_lag_kernel`
   and `cov_gini_metro` into `build/D_cov/` as standalone scripts. Write them to
   `cov/cbsa/` so 30 merges them, and simplify `load_panel()`.
3. Replace the optional-input `file.exists()` branches in 06 with hard
   requirements.
4. Write `pipeline.csv` and `run.R`. Generate the dependency graph from
   `pipeline.csv` for the README.
5. Check: `cbsa_year.parquet` equals today's file plus the three merged columns.

*Status 2026-11-17 (second machine):*
- **Step 1 done.** `07_npo_bls.R` builds `cbsa_bls.csv` only, and `07b_bls_validation.R` holds the 990-vs-BLS validation. `20_urban_economy.R` builds `cov_qcew.csv` only (no `qcew|bench` argument), and `20b_benchmarks.R` holds the benchmarks and SAMI-on-SAMI. 07b reads `cbsa_bls.csv` back from disk, so its diagnostics can differ from before in the last printed digits. The scripts stay in `R/` until Phase 3.
- **Step 2 done, with two changes from the wording above.**
  - **New builders.** `25_cov_endowment.R`, `26_cov_lag_kernel.R` and `27_cov_acs_metro.R` replace the `build` modes of 55, 82 and 89, which now stop and point to them. They write the same files in the same places (`cov/cov_*.csv`, `need_socap/acs_need_cbsa.csv`), not `cov/cbsa/`, because 58, 72 and 89 read those paths.
  - **c9 needs no builder.** 82 read `c9_metro_1970_2024.csv` (73) only for a consistency printout, which was dropped. The real cycle was `load_panel()`, which 82 used to get CBSA population. 26 now sums county population from `npo_county_year.csv` exactly as 30 does.
  - **Merge moved to 30.** 30 merges `hist_pre1970`, `lag_g5`, `lag_g10` and `gini_m` last (after derive and fill), exactly as `load_panel()` attached them. `load_panel()` now only adds `logP` and `division`, and it stops if a panel lacks the four columns.
- **Step 3 done.** In 06, the grants, monthly BMF, reconstructed BMF and entry inputs are required (`need()`).
- **Step 4 done.** `pipeline.csv` has 36 steps (database, checks, multiverse). `run.R` provides the stage selection, `--dry-run`, `--from`, `--only`, make-style skipping, a missing-input stop, per-step logs and `MANIFEST.csv`. `run.R --graph` writes `env/pipeline_graph.md` (Mermaid). Inputs in `pipeline.csv` list build files only; raw sources are noted in the notes column and are left to the Phase 4 inventory. From Git Bash, export `MV_BUILD`, because R there reads `~/.Renviron` from `$HOME`, not Documents.
- **Step 5 done** (`env/check_phase2.R`, against the 11:02 S3 snapshot used for Phase 1).
  - **Side files and panels.** The rebuilt side files are identical to the reference: `cov_endowment`, `cov_lag_kernel` (+ coverage), `cov_gini_metro` and `acs_need_cbsa`. `cbsa_year.parquet` equals the reference plus exactly those four columns (23,125 × 422, cell by cell). `load_panel()` returns the same table as before (column order aside), and `county_year.parquet` is identical.
  - **06.** The committed 06 and the new 06 were run on the same inputs (current S3 copies). `npo_county_year`, `npo_cbsa_year`, `npo_county_year_by_source` and `splice_overlap` are byte-identical.
  - **v11 slice.** All 1,000 statuses match, and every OLS / fixed-effects / div_fe spec is byte-identical to the Phase 1 slice. About 60 of 190 `state_re` (lme4) specs differ in the 7th digit, but they differ by the same amount between two runs of identical code. The lme4 fits are not bit-reproducible run to run, and Phase 2 did not change them.

**Phase 3: reorganise folders (½ day)**
Move the scripts into `R/lib`, `R/build/*`, `R/multiverse`, `R/paper` and `R/notes`.
Update the `source()` paths, the README and the memory notes. Check:
`run.R database --dry-run` resolves every step.

**Phase 4: freeze the inputs (1 day, mostly upload time)**
1. Write an inventory script that lists every file the build reads. Instrument the
   read helpers, or use `pipeline.csv` inputs, and record size and sha256 for each.
   Copy only those files into `MV_INPUTS/<source>/`, so only the BMF snapshot years
   actually used are included, not 27 GB.
2. Document how the hub's derived inputs (`SOI/04_final`, the SOI frame and the BMF
   long panel) were made. Add their scripts if they are yours, or treat them as
   frozen inputs with provenance notes.
3. Write `SOURCE.md` (URL, date, licence) for each source.

**Phase 5: clean-room rebuild (1–2 days of compute)**
1. With `MV_BUILD=D:/MULTIVERSE_REPRO` and `MV_INPUTS=<frozen inputs>`, run
   `Rscript run.R database` on a fresh renv library, ideally on a second machine or
   an EC2 instance pulling from S3.
2. Diff against `D:/MULTIVERSE/data` by sha256, then cell by cell (reuse the
   `diff_files.csv`/`diff_cells.csv` approach from the flog check). Explain or fix
   every difference.
3. Record the wall time of each step in the README.

**Phase 6: paper exhibits (1–2 days)**
1. Decide whether Part I moves to v11 / post-NTEE-fix outputs, or v10 is rebuilt
   after the fix.
2. `R/paper/make_figures.R` writes every `paper/figures/*.png` from a named source,
   including fixing `f4_change_correlates` (stale) and `f7_hazards`.
   `R/paper/make_numbers.R` writes the tables the text cites. Optionally convert
   hand-typed numbers to inline `` `r` `` values.

**Phase 7: publish**
Tag `v1.0`, upload the release prefix with a manifest, set access, deposit on Zenodo,
and add the data availability statement to the paper.

---

## 5. Decisions needed

1. **Scope of the release:** the database only, or database + multiverse run(s) +
   paper exhibits? (Recommendation: all three, with the database as the
   centrepiece.)
2. **Raw inputs:** ship frozen copies where licences allow, or download-on-demand
   only? (Recommendation: frozen copies plus download scripts. Sources have already
   moved: PatentsView, NCCS marts.)
3. **Access:** public prefix vs requester-pays vs Zenodo.
4. **v10 in the paper:** retire it in favour of v11, or rerun it after the NTEE fix?
5. **Code freeze timing:** when the parallel sessions stop adding scripts.
