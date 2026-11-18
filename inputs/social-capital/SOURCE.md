# County social capital index (Rupasingha, Goetz and Freshwater)

| Field | Value |
|---|---|
| Publisher | Northeast Regional Center for Rural Development (NERCRD), Penn State |
| Files | `socap_97_05_09.xlsx` (1997, 2005, 2009), `socap_2014.xlsx` (2014) |
| Original URL | https://nercrd.psu.edu/data-resources/county-level-measure-of-social-capital/ (the code records no URL) |
| Years used | 1997, 2005, 2009, 2014 (2000 interpolated; carried forward after 2014) |
| Obtained | **Manual download** to `$MV_BUILD/raw/cov/socap/` (2.2 MB) |
| Read by | `R/build/D_cov/19_cov_socap.R` (association establishments, census mail response; index re-estimated without NCCS density and turnout, `socap_sk2`) |
| Licence | None published. NERCRD offers the files without warranty and asks users to email nercrd@psu.edu about their use of the data |
| Redistribute in release | **Unclear.** Do not ship the xlsx files without permission |
| Cite as | Rupasingha, A., Goetz, S. J., and Freshwater, D. 2006. "The production of social capital in US counties." *Journal of Socio-Economics* 35: 83–101 (with 2014 update). |
| Frozen copy | **Not shipped** unless NERCRD agrees. Ship `SOURCE.md`, the sha256 of each file, and download instructions |

**Recommendation:** email nercrd@psu.edu to ask permission. The files are small and the
2014 update may not stay online. The derived variables in the database are lower risk
than the original files.
