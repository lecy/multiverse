# BLS research data on the nonprofit sector (MSA files)

| Field | Value |
|---|---|
| Publisher | U.S. Bureau of Labor Statistics, Business Employment Dynamics / QCEW research data on nonprofits (QCEW establishments matched to IRS 501(c)(3) status) |
| Files | `qcew-nonprofits-<year>.csv`, 2013–2022 |
| Original URL | BLS nonprofit research data page, `https://www.bls.gov/bdm/nonprofits/nonprofits.htm` (the page blocks scripted requests; download in a browser). The code records only the local path |
| Years used | 2013–2022. BLS has announced no update before 2029 |
| Obtained | **Manual download**, kept outside the project in `$MV_BLS` (default `<Dropbox>/00 - URBAN/00-DATA/bls`); the script stops if a file is missing |
| Read by | `R/build/D_cov/07_npo_bls.R`; `R/build/F_checks/07b_bls_validation.R` |
| Licence | Public domain (bls.gov/opub/copyright-information.htm) |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Bureau of Labor Statistics, Research Data on the Nonprofit Sector, 2013–2022. |
| Frozen copy | `inputs/bls-nonprofits/` |

MSA codes are on the 2013 and 2018 delineations and are matched to 2023 CBSAs in 07
(`bls_delin_flag`). The folder holds 62 MB in total; freeze only the
`qcew-nonprofits-<year>.csv` files that 07 reads.
