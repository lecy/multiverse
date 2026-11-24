# SEC EDGAR: public-company headquarters addresses

| Field | Value |
|---|---|
| Publisher | U.S. Securities and Exchange Commission: EDGAR full index, EDGAR filing headers, and the Financial Statement Data Sets (Division of Economic and Risk Analysis) |
| Files | `full-index/form_<YYYY>Q<q>.idx.gz`: the quarterly EDGAR form index, 2000Q1–2024Q4 (100 files). `fsds/sub_<yyyy>q<q>.txt.gz`: `sub.txt` from each Financial Statement Data Set, 2009Q2–2024Q4 (63 files). `hdr/hdr_<YYYY>Q<q>.txt.gz`: the SEC header (first 25 kB of the `.txt` submission, up to `</SEC-HEADER>`) of every annual report not in FSDS, one record per filing after a `<<<ACC accession>>>` line. `zcta/zcta_county_rel_10.txt` and `zcta/tab20_zcta520_county20_natl.txt`: Census ZCTA-to-county relationship files, 2010 and 2020 |
| Original URL | `https://www.sec.gov/Archives/edgar/full-index/<YYYY>/QTR<q>/form.idx`; `https://www.sec.gov/files/dera/data/financial-statement-data-sets/<yyyy>q<q>.zip` (only `sub.txt` is kept); `https://www.sec.gov/Archives/<path from the index>`; `https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt`; `https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt` |
| Years used | Annual reports filed 2000–2024. Large-filer status 2011–2024 |
| Obtained | Script (`R/build/D_cov/28_cov_corporate_hq.R`), cached in `$MV_BUILD/raw/sec-edgar/`. SEC requests need `SEC_USER_AGENT` (a name and contact address, set in `~/.Renviron`, never written to the repo), and the script stays under the SEC's limit of 10 requests per second. Fetching the headers takes several hours the first time; an interrupted run resumes from `hdr_<qtr>.part` |
| Read by | `R/build/D_cov/28_cov_corporate_hq.R` |
| Licence | EDGAR filings and the Financial Statement Data Sets are public information released by a U.S. government agency (no copyright claimed). The Census relationship files are public domain |
| Redistribute in release | Yes |
| Cite as | Source: U.S. Securities and Exchange Commission, EDGAR full index and filing headers, 2000–2024, and Financial Statement Data Sets, 2009–2024; U.S. Census Bureau, ZCTA to County Relationship Files, 2010 and 2020. |
| Frozen copy | `inputs/sec-edgar/` |

**What is cached.** The index files are the original `form.idx` gzipped. `sub.txt`
is copied from each FSDS zip and the zip deleted: `num.txt` and `pre.txt` (most of
each zip, about 5 GB in all) are not used. The header files are verbatim header text,
cut at `</SEC-HEADER>`. The full 10-K documents are never downloaded.

**Forms.** 10-K, 10-K405, 10-KSB, 10-KSB40, 10-KT, 10-KT405, 10-KSBT, 10-K405T,
20-F and 40-F, without amendments. 10-KSB (small businesses) ran until 2009; its
filers then moved to the 10-K, so counting both keeps the series continuous.

**Address source.** FSDS covers XBRL filers only, phased in from 2009 (large
accelerated filers above $5B float), 2010 (other large accelerated filers) and 2011
(everyone else). Filings outside FSDS (all filings before 2009Q2, asset-backed
issuers, and some small filers after) take the address from the header. Both report
the business address on file with EDGAR at the time of filing.

**Excluded filers.** Non-US business addresses, and SIC 6189 (asset-backed
securities), 6221 (commodity pools), 6770 (blank checks / SPACs), 8888 (foreign
governments) and 9995 (non-operating shell companies): these file annual reports but
are not operating headquarters.

**Known limits.**
- The address is the one on the EDGAR filer record. A firm whose registered
  business address is a law firm or registered agent is counted there.
- Large-filer status (`afs = "1-LAF"`) exists only for filings in FSDS, so
  `hq_pub_large` starts in 2011.
- ZIP to county: the 2010 ZCTA file (largest population part) first, then the 2020
  file (largest land part) for newer ZCTAs, then the BMF-learned modal county for
  ZIPs that are not ZCTAs. A ZIP that crosses a county line goes to one county.
