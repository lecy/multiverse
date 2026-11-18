# Long NCCS BMF panel, 1995–2022

| Field | Value |
|---|---|
| Producer | Built in 2022 by a collaborator (methods memo initialled "LF") from the NCCS legacy BMF files (the last month available each year), Pub 78 and IRS automatic revocations |
| File | `bmf-long-panel-1995-2022.rds` (270 MB, dated 2022-11-17), presumably a `haven` read of the Stata original `bmf-long-panel-1995-2022.dta` (1.9 GB, 2022-11-11). That original is also inside `Births and Deaths Long BMF Panel 1995-2022.zip` as `Long NCCS BMF Panel 1995-2022 merged trend with pub78 and autorev clean_updated_9-2022.dta` |
| Documentation | `<MV_INPUTS>/BMF/Long BMF Panel 1995-2022 Methods Description_LF 9-2022.docx`; tables in `Long BMF 1995-2022 tables and graphs_updated.docx` |
| Method (from the memo) | Non-501(c)(3) and NCCS "out of scope" records are dropped. EINs whose first ruling date falls after a revocation date are excluded. The result is 1,481,254 organizations; 2022 runs through August 2022 |
| Producing script | **Not found.** No Stata `.do` for the panel and no R script for the `.dta` → `.rds` step were found in `00 - DATA` or `00 - URBAN` |
| Hub location | `<MV_INPUTS>/BMF/` |
| Read by | `R/build/B_npo/08_npo_entry.R`. Columns: `ein, LongBMF19952022PanelOrg, ruledate1, ruleyear, fips_ruleyear, zip5_ruleyear, city_ruleyear, state_ruleyear, subseccd_ruleyear, fndncd_ruleyear, nteefinal, outnccs`. Rows: `LongBMF19952022PanelOrg == 1`, `outnccs != "OUT"`, `subseccd_ruleyear == 3`; ruling years 2000–2021 |
| Licence | Derived from NCCS data (ODC-BY 1.0) and public IRS lists. The panel itself carries no licence; it is the project team's own product |
| Redistribute in release | Yes, with NCCS attribution and a credit to its builder |
| Cite as | Long NCCS BMF Panel 1995–2022 (methods memo, September 2022), built from Urban Institute NCCS legacy BMF files. |
| Frozen copy | `inputs/nccs-bmf/long-panel/`: the `.rds` and the two `.docx` files. Optionally also a column subset (the 12 columns above) as parquet |

**Provenance verdict:** documented by the memo, but no script exists. The frozen
file and its sha256 are the record. Ask the builder whether the Stata code survives.
If it does, add it to the frozen folder.
