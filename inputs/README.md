# Source inputs

One folder per source. Each folder's `SOURCE.md` records the publisher, file names, the
original URL, the years used, how the file is obtained, which build scripts read it,
the licence, whether it may be redistributed, and how to cite it.

These files are the documentation for Phase 4 of `REPRODUCIBILITY-PLAN.md`. The data
themselves are not in git. When a source is frozen, its files and a `MANIFEST.csv`
(path, bytes, sha256) go to `s3://lecy/nonprofit-density/inputs/<folder>/`, and this
`SOURCE.md` goes with them. SOI and the unified BMF are frozen already. The others
are documented here but not yet frozen.

Written 2026-11-17 from the build scripts (`R/build/`), `DATA-MANIFEST.md`, the data
hub, the S3 mirror of the build caches, and the publishers' terms pages (checked
2026-11-17).

## Index

| Folder | Source | Obtained | Redistribute | Frozen |
|---|---|---|---|---|
| [census-popest](census-popest/SOURCE.md) | Census PEP county totals 2000–25, CT crosswalk | script | yes | no |
| [census-pep-char](census-pep-char/SOURCE.md) | Census PEP county characteristics 2000–24 | script | yes | no |
| [census-saipe](census-saipe/SOURCE.md) | Census SAIPE 2000–24 | API | yes | no |
| [census-acs](census-acs/SOURCE.md) | ACS 5-year 2009–24, Census 2000 SF3 | API | yes | no |
| [census-cog](census-cog/SOURCE.md) | Census of Governments finance 2002–22 | script | yes | no |
| [census-hist](census-hist/SOURCE.md) | Census county estimates 1970–99 | **manual** | yes | no |
| [census-geo](census-geo/SOURCE.md) | TIGER 2023 counties, OMB 2023 CBSA crosswalk | package / hub | yes | no |
| [bea-regional](bea-regional/SOURCE.md) | BEA regional accounts (CAINC, CAEMP, CAGDP) | script | yes | no |
| [bea-pcepi](bea-pcepi/SOURCE.md) | PCE price index, BEA NIPA 2.8.4 (FRED until 2026-11-17) | script | yes | no |
| [bls-qcew](bls-qcew/SOURCE.md) | BLS QCEW county files 2000–24 | **pre-placed, origin unrecorded** | yes | no |
| [bls-nonprofits](bls-nonprofits/SOURCE.md) | BLS nonprofit research data 2013–22 | **manual** | yes | no |
| [ers-laus](ers-laus/SOURCE.md) | LAUS via USDA ERS 2000–23 | script | yes | no |
| [irs-soi-county](irs-soi-county/SOURCE.md) | IRS SOI county income 2000–23 | script | yes | no |
| [soi](soi/SOURCE.md) | IRS SOI exempt-org extracts, harmonized | hub | yes | **yes** |
| [nccs-bmf-legacy](nccs-bmf-legacy/SOURCE.md) | NCCS legacy BMF snapshots (23 of 88 files) | hub | yes (ODC-BY) | no |
| [nccs-bmf-monthly](nccs-bmf-monthly/SOURCE.md) | NCCS monthly BMF Dec 2023, Dec 2024 | **manual** | yes (ODC-BY) | no |
| [nccs-bmf-unified](nccs-bmf-unified/SOURCE.md) | NCCS unified BMF, geocoded | panel990 | yes (ODC-BY) | **yes** |
| [nccs-bmf-long-panel](nccs-bmf-long-panel/SOURCE.md) | Long NCCS BMF panel 1995–2022 | hub | yes (ODC-BY) | no |
| [nccs-core](nccs-core/SOURCE.md) | NCCS harmonized CORE TY2000–19 | script | yes (ODC-BY) | no |
| [nccs-efile](nccs-efile/SOURCE.md) | NCCS e-file `efile_v3_1` via panel990 | package | yes (ODC-BY, to confirm) | no |
| [elections](elections/SOURCE.md) | MEDSL 2000–16, tonmcg 2020/24, Alaska | script | yes (CC0 / MIT) | no |
| [religion-census](religion-census/SOURCE.md) | RCMS 2000, 2010 (ARDA), USRC 2020 | **manual** | yes, **by permission** | no |
| [social-capital](social-capital/SOURCE.md) | Rupasingha et al. (NERCRD) | **manual** | yes, **by permission** | no |
| [patents](patents/SOURCE.md) | PatentsView (USPTO) 2026-04 release | **manual, login** | yes (CC BY 4.0) | no |
| [sec-edgar](sec-edgar/SOURCE.md) | SEC EDGAR index, filing headers, FSDS `sub.txt` 2000–24; Census ZCTA files | script (SEC User-Agent) | yes | no |
| [census-cbp](census-cbp/SOURCE.md) | Census County Business Patterns 2000–23 (NAICS 551114) | script | yes | no |
| [check-only](check-only/SOURCE.md) | 1023-EZ, NTEE-resolved, e-file 2009–19, election checks | various | yes | no |

## Open items found while documenting

**Origin unrecorded:**
- **QCEW county files:** no script downloads them, and the URL and date are unknown. Add a downloader to 20.
- **Legacy BMF snapshots:** no download URL is recorded.
- **CBSA crosswalk:** not matched to a specific OMB file.

**Manual steps that could be scripted:**
- census-hist (URLs found)
- the monthly BMF (URLs known)
- the BLS nonprofit files (the page blocks scripts)

**No producing script:**
- **SOI sample frame:** the saved scripts do not produce its columns (see `soi/SOURCE.md`).
- **Long BMF panel:** the Stata code was not found.

**Licences to settle before publishing:**
- **ASARB (RCMS) and NERCRD (social capital):** permission to bundle was received (recorded 2026-11-17). File each written permission as `PERMISSION.md` in its folder, and ship it with the data.
- **FRED:** done 2026-11-17. 06 reads the same series from BEA (`bea-pcepi/SOURCE.md`); outputs unchanged.
- **NCCS e-file:** confirm that its catalog licence is ODC-BY.

**Inputs outside the hub and the build folder:** the RCMS .SAV files sit in
`38 - resource environments/replication/`.

## Getting the sources again

- **Frozen copies (the published numbers):** `Rscript env/restore_inputs.R`, or `npdensity::mv_get_inputs()`. Every file is checked against the package manifest (`npdensity::mv_manifest()`).
- **From the publishers:** `Rscript env/fetch_sources.R`, or `npdensity::mv_fetch_source( mode = "live" )`. It downloads every file that has a stable public URL again and compares it with the frozen hash. The URLs are in the source registry, `npdensity::mv_sources()` (`env/build_sources.R`). It now covers the sources that were placed by hand before, `census-hist` and the monthly BMF.
- **Live check, 2026-11-24** (`live_check.csv`): 153 of the 1,520 frozen files have a stable public URL. 152 of them are byte-identical to the freeze. One was refused by the Census firewall: Delaware's 2000–2010 county characteristics (see `census-pep-char/SOURCE.md`).
- The other 1,367 files have only the frozen copy, and the registry's `note` says why: API responses (SAIPE, ACS, QCEW), files extracted from a download, SEC files re-compressed by step 28, hand-placed files, and our own intermediates (CORE, panel990 tables).
