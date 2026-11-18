# Plan: replication site (Quarto website on GitHub Pages)

Status 2026-11-18: plan only, nothing built yet.

## Goal

A public website that explains how the nonprofit-density database is built, what the
replication files are, and how the multiverse analysis is designed and run. It has
eight parts, each ending in its own APA-style reference list.

## What already exists and gets reused

Most of the text already exists. The site mostly rearranges it rather than writing it
from scratch.

| Part | Existing source |
|---|---|
| 1 Dataset construction | `docs/workflow.qmd` (build flow, final tables), `Data Construction Plan.md`, `docs/build_flow.svg` |
| 2 Files, sources, dictionaries | `inputs/README.md` and the 25 `inputs/*/SOURCE.md` files, `docs/data_inventory.csv`, `docs/variable_dictionary.csv` (321 variables), the frozen `MANIFEST.csv` per S3 folder, `DATA-MANIFEST.md` |
| 3 Workflow code | `run.R`, `pipeline.csv` (30 database steps, 4 checks, 3 multiverse), `env/` (renv, freeze, restore, compare), `REPRODUCIBILITY-PLAN.md` Phases 4–5 |
| 4 Multiverse overview | `paper/03-multiverse-overview.qmd` |
| 5 Multiverse design | `paper/04-design-decisions.qmd` (cards D1–D12), `paper/02-variables.qmd`, `Multiverse Analysis Design Nonprofit Density.md` |
| 6 Stylized facts | `paper/01-stylized-facts.qmd` (Facts 1–7) |
| 7 Multiverse execution | `R/multiverse/` (16 scripts; steps 44v11, 97, 98 in `pipeline.csv`), `paper/05-findings.qmd` (decision matrix) |
| 8 Where the research goes | `paper/05-findings.qmd` "Scope and open gaps", `notes/modeling-framework-synthesis.md` "Still open", `REPRODUCIBILITY-PLAN.md` §5 |

The paper chapters contain no R code: their figures are pre-rendered PNGs in
`paper/figures/`. The site therefore renders without the 30 GB build or AWS access.

## Site structure

A Quarto `website` project at the repo root, with an explicit `render:` list so that
notes, the SAMI paper and the old `docs/workflow.qmd` are not swept in.

```
_quarto.yml                 website project; render list; output-dir: _site
index.qmd                   landing page: what the project is, the eight parts, how to cite
site/
  01-construction.qmd       Part 1
  02-files-sources.qmd      Part 2  (+ 02-dictionary.qmd if one page gets too long)
  03-workflow.qmd           Part 3
  04-multiverse.qmd         Part 4  -> includes paper/03-multiverse-overview body
  05-design.qmd             Part 5  -> includes paper/04 + 02 bodies
  06-stylized-facts.qmd     Part 6  -> includes paper/01 body
  07-multiverse-run.qmd     Part 7
  08-next.qmd               Part 8
  data/                     CSVs the pages read (manifests, dictionary), committed
  apa.csl                   APA 7th edition citation style
references.bib              existing 14 entries + data sources + software
```

Navigation: a sidebar in three groups. **Data** covers Parts 1–3, **Multiverse** covers
Parts 4–5 and 7, and **Findings** covers Parts 6 and 8. The parts keep their numbers in
the sidebar labels, so the order the user asked for is visible.

**Reusing the paper chapters without copying.** Move each chapter's body into a
partial (`paper/_01-stylized-facts-body.qmd`). The paper file and the site page then
both `{{< include >}}` it, so there's one copy of the text.

Figure paths in included text resolve relative to the including file, so the partials
should use paths relative to the project root (`/paper/figures/...`), which work in
both places. Check this on one chapter first (step 2 below), before converting the
rest.

## Page content

**Part 1, Dataset construction.**
- The question the database serves, and its units (3,144 counties and 925 CBSAs,
  2000–2024).
- The five build stages: geography, nonprofit counts, the org-year panel, covariates,
  assembly.
- The build-flow diagram, plus the order dependencies found earlier (02b depends on 05's
  cache, 08 on 02b, NTEE resolved once and applied everywhere).
- The two final tables (`county_year`, `cbsa_year`), with row and column counts and
  their gap-fill variants (`_obs`, `_lin`, `_locf`).
- Key measurement choices: the NTEE rule, the BEA price index, legacy BMF backfill.

**Part 2, Files, sources and data dictionaries.**
- A source table: 25 sources with publisher, years, how each is obtained, licence, and
  whether a frozen copy is on S3 or it is downloaded on demand (`data_inventory.csv`).
- One section per source, drawn from its `SOURCE.md`.
- File manifest: the 1,126 frozen files, as a searchable table (folder, path, size,
  sha256, the steps that read it). Built from the S3 manifests by a small script that
  writes `site/data/frozen_manifest.csv`, so the site never needs AWS at render time.
- Data dictionaries: one searchable table per final table, from
  `variable_dictionary.csv` (name, definition, source, unit, years, gap-fill variants).
- References: the data citations, all of them listed (`nocite`), plus software (R,
  data.table, arrow, renv, Quarto).

**Part 3, Workflow walk-through.**
- Setup: clone, `renv::restore()`, the four roots (`MV_BUILD`, `MV_INPUTS`, `MV_BLS`,
  `MV_DROPBOX`), the AWS profile.
- Restore inputs: `env/restore_inputs.R --check`.
- Run: `Rscript run.R database` and the flags `--graph`, `--dry-run`, `--only`,
  `--from`, `--raw`; how make-style skipping works; `MANIFEST.csv`.
- A step-by-step table generated from `pipeline.csv`: id, script, inputs, outputs,
  wall time.
- Verify: `env/compare_builds.R`, and the clean-room result (74 identical, 6 equal by
  cell, 17 explained).
- Every command in a copyable code block. The commands are shown, not executed when the
  site renders.

**Part 4, Multiverse overview.** `paper/03` as is: why a multiverse, the three kinds of
decisions, the grid, one metric across specifications, attributing spread to decisions.

**Part 5, Multiverse design.** Decision cards D1–D12 (`paper/04`), with variables and
constructs (`paper/02`) as a sub-page or a section.

**Part 6, Stylized facts.** Facts 1–7 (`paper/01`).

**Part 7, Multiverse execution.**
- How a universe is defined and run: `44_run_universe.R`, the fork scripts (`51`–`93`),
  the summaries (`45`, `97`, `98`).
- Commands: `Rscript run.R multiverse`, run time and cores.
- Outputs and where they are stored.
- The decision matrix and the importance results from `paper/05`.

**Part 8, Where the research goes from here.**
- The open gaps and scope limits from `paper/05`.
- The open questions in the modeling-framework synthesis.
- Planned extensions.
- Release and versioning (v1.0, Zenodo DOI).

## Citations (APA 7)

- **Style:** `csl: site/apa.csl` (the official APA 7th edition CSL from the Citation
  Style Language styles repository) and `bibliography: references.bib` in
  `_quarto.yml`.
- **Per-page reference lists:** each page ends with `# References {.unnumbered}` and a
  `::: {#refs} :::` block, so every page lists only what it cites.
- **New `references.bib` entries.** About 25 data sources, written from each
  `SOURCE.md` "Cite as" line, for example:
  - U.S. Census Bureau (2025), *County population totals*;
  - National Center for Charitable Statistics (2026), *Unified BMF*;
  - Internal Revenue Service, Statistics of Income (year), *SOI county data*;
  - U.S. Bureau of Labor Statistics, QCEW;
  - Bureau of Economic Analysis (2026), NIPA Table 2.8.4;
  - Association of Statisticians of American Religion Bodies (2023);
  - PatentsView;
  - MIT Election Data and Science Lab;
  - Economic Research Service.
- **Software entries:** R Core Team; data.table; arrow; renv; Quarto.
- **Corporate authors** use double braces in BibTeX (`author = {{U.S. Census Bureau}}`)
  so APA doesn't invert them. Datasets use `@misc` with `type = {Data set}` and a URL
  plus access date.
- **Check:** 23 of 25 `SOURCE.md` files have no full citation yet (only patents and
  one other carry a "Cite as" line). Writing those entries is the largest single task
  in the plan.

## Publishing

**The repository is private.**
- GitHub Pages from a private repo needs a paid plan, and the published site is
  public even then.
- Options:
  1. Make `lecy/multiverse` public at release (Phase 7). The site goes live then.
  2. Publish the site now from a separate public repo (e.g. `lecy/nonprofit-density`)
     that holds only the rendered `_site`.
  3. Keep it private until release and preview it locally (`quarto preview`).
- **Decided 2026-11-18: option 1.** `lecy/multiverse` will be made public, and the site
  is published from it. Before switching visibility, scan the full git history for
  secrets and private material (API keys, `.Renviron`, local paths, unpublished
  drafts).

**Mechanism:** render locally, then `quarto publish gh-pages`. That pushes `_site` to
a `gh-pages` branch. No GitHub Actions are needed, because nothing executes R at
render time. `_site/` and `.quarto/` go in `.gitignore`.

## Work order

1. **Scaffold:** add `_quarto.yml` (website), `index.qmd`, empty part pages, the APA
   CSL, and `.gitignore` entries. Check that `quarto render` builds an empty site
   without picking up other `.qmd` files.
2. **Pilot the include pattern on Part 6:** split `paper/01-stylized-facts.qmd` into a
   body partial plus a wrapper. Check that both the paper and the site page render
   with figures and citations. Then do Parts 4 and 5 the same way.
3. **Bibliography:** add the data-source and software entries to `references.bib`.
   Add a "Cite as" line to each `SOURCE.md` that lacks one, so the two stay in sync.
4. **Part 2:** a script (`docs/make_site_data.R`) that writes `site/data/*.csv` from
   the S3 manifests, `data_inventory.csv` and `variable_dictionary.csv`; then
   searchable tables (`DT` or `reactable`, added to `renv.lock`).
5. **Parts 1 and 3:** move the content from `docs/workflow.qmd`, and generate the step
   table from `pipeline.csv`. Retire `docs/workflow.qmd`, or make it point to the site.
6. **Parts 7 and 8:** new text, drawing on `paper/05` and the synthesis notes. This
   needs your input on Part 8 (what's next is a research judgement).
7. **Review:** check every page's reference list renders in APA, links work, the
   phone-width layout, and that no secrets or local paths appear (e.g. `D:/MULTIVERSE`
   in commands should become `$MV_BUILD`).
8. **Publish** when the repo goes public (see Publishing).

**Effort:** about 2 days. The bibliography (step 3) and Parts 7–8 (step 6) are most of
it; the rest is rearranging existing text.

## Decisions for you

1. **Hosting:** public `lecy/multiverse` at release (recommended), a separate public
   site repo now, or local preview only.
2. **Reuse the paper chapters by include (one copy of the text, recommended) or copy
   them into the site** (simpler, but they drift).
3. **Part 8 content:** which directions to name. I can draft from the open gaps, but
   you should set the priorities.
4. **Whether the frozen-file manifest (1,126 rows with sha256) is shown in full or
   summarized by folder** with a link to each S3 `MANIFEST.csv`.
