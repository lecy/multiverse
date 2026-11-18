# PatentsView granted-patent tables (USPTO)

| Field | Value |
|---|---|
| Publisher | U.S. Patent and Trademark Office, PatentsView, via the USPTO Open Data Portal (dataset PVGPATDIS, 2026-04 release, grants through 2025) |
| Files | `g_patent.tsv`, `g_application.tsv`, `g_inventor_disambiguated.tsv`, `g_location_disambiguated.tsv`, `g_assignee_disambiguated.tsv` (and their `.tsv.zip` originals) |
| Original URL | https://data.uspto.gov (PatentsView moved there in March 2026; the old page https://patentsview.org/download/data-download-tables redirects to the transition guide) |
| Years used | Application years 2000–2024; 2021+ truncated by grant lag (`patents_truncated`) |
| Obtained | **Manual download, USPTO login required** (with a CAPTCHA), to `$MV_BUILD/raw/patents/`. If the files are missing, 21 prints a message and skips |
| Read by | `R/build/D_cov/21_patents.R` |
| Licence | CC BY 4.0 (stated on the PatentsView bulk download pages). The underlying USPTO records are public domain |
| Redistribute in release | Yes, with attribution |
| Cite as | Source: PatentsView (www.patentsview.org), U.S. Patent and Trademark Office, granted patent disambiguated data, 2026-04 release. |
| Frozen copy | `inputs/patents/`: **the five `.tsv.zip` files only (about 1.3 GB)** |

**Leave out of the release:**
- The unzipped `.tsv` files (4.7 GB), which duplicate the zips.
- `223582-V1.zip` (7.5 GB), the unusable DataLumos mirror.
- `tables/` (1.0 GB). It holds only `g_location_not_disambiguated.tsv.zip`, which 21 does not use.

21 reads a `.tsv.zip` directly (`unzip -p`) when no `.tsv` is present, so the five
zips are a complete input. Because the live source needs a login, the frozen copy is the only way to rebuild
this input without a USPTO account.
