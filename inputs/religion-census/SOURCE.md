# Religious Congregations and Membership Study (RCMS), 2000, 2010, 2020

| Field | Value |
|---|---|
| Publisher | Association of Statisticians of American Religious Bodies (ASARB). 2000 and 2010 county files distributed by the Association of Religion Data Archives (ARDA); 2020 by the U.S. Religion Census |
| Files | 2000: `Religious Congregations and Membership Study, 2000 (Counties File).SAV`. 2010: `U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).SAV`. 2020: `2020_USRC_Group_Detail.xlsx` (sheet "2020 Group by County"), `2020_USRC_Summaries.xlsx` (sheet "2020 County Summary") |
| Original URLs | 2000 and 2010: ARDA dataset download pages, https://www.thearda.com (the code records no URL). 2020: https://www.usreligioncensus.org (data download page; record the exact page when freezing) |
| Years used | 2000, 2010, 2020 (interpolated, and carried forward after 2020, in 30) |
| Obtained | **Manual download.** The .SAV files are in `38 - resource environments/replication/np_density_refactor/Data/`, outside the project and the data hub. The xlsx files are in `$MV_BUILD/raw/cov/rcms/` |
| Read by | `R/build/D_cov/18_cov_rcms.R` |
| Licence | ASARB holds the copyright. The USRC FAQ (usreligioncensus.org/faq) says permission to use the data "does not grant permission to redistribute or repost the data". ARDA download agreements require credit to ARDA and the original collectors, and grant no redistribution right (ARDA's terms page could not be read on 2026-11-17) |
| Redistribute in release | **Yes, by permission.** Without it, the FAQ forbids reposting. The project received permission to bundle these files with the release (recorded 2026-11-17). Keep the written permission in this folder as `PERMISSION.md` (or the e-mail as PDF), and ship it with the files |
| Cite as | Grammich, C., et al. 2023. *2020 U.S. Religion Census: Religious Congregations & Membership Study.* ASARB. For 2000 and 2010, the ARDA citation shown on each dataset's page. |
| Frozen copy | `inputs/religion-census/`: the two .SAV files, the two 2020 xlsx files, `PERMISSION.md`, and `MANIFEST.csv` |

**Permission scope:** check that the permission covers the public release prefix and
Zenodo, as well as the S3 bundle. Check also whether it covers the derived county variables
in `database/` (`rel_adh`, `rel_cng`, `rel_hhi`, `pct_adherent`, `pct_catholic`), and
whether ASARB or ARDA asked for specific citation or notice wording. Copy any required
wording into "Cite as" above.

**To do:** move the two .SAV files into the data hub (`<MV_INPUTS>/religion/`) and
point 18 at `DATA_HUB`. The current `replication/` path is the only input outside
both the hub and the build folder.
