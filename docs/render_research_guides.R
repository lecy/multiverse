## Render the datagoodr research guides for the two research frames and copy
## them into the website.
##
##   Rscript docs/render_research_guides.R [frame]
##
## Run after docs/make_research_guides.R (builds the DGFs) and
## docs/check_research_guide_types.R (corrects and confirms the data types).
## Writes docs/research-guide/<frame>/research-guide.html (self-contained) and
## copies it to site/guides/<frame>.html, which _quarto.yml publishes as a
## resource and Part 3 links to.

suppressMessages( library( datagoodr ) )

args   <- commandArgs( trailingOnly = TRUE )
frames <- if ( length( args ) >= 1 && args[1] != "both" ) args[1] else c( "cbsa_year", "county_year" )

quarto <- Sys.getenv( "QUARTO", "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe" )
titles <- c( cbsa_year   = "Research guide: CBSA-year frame",
             county_year = "Research guide: county-year frame" )
subs   <- c( cbsa_year   = "925 CBSAs (2023 delineation) x 25 years, 2000-2024: 23,125 rows, 422 variables",
             county_year = "3,144 county-equivalents (2023 delineation) x 25 years, 2000-2024: 78,600 rows, 354 variables" )

## A one-line project file keeps quarto from rendering these guides as pages of
## the website project above them.
writeLines( c( "project:", "  type: default" ), file.path( "docs", "research-guide", "_quarto.yml" ) )
dir.create( file.path( "site", "guides" ), showWarnings = FALSE )

for ( frame in frames ) {
  out <- file.path( "docs", "research-guide", frame )
  qmd <- file.path( out, "research-guide.qmd" )
  create_rg( dgf = file.path( out, "DGF.xlsx" ), dir = out, file = "research-guide.qmd",
             style = "compact", overwrite = TRUE, render = FALSE )
  doc <- readLines( qmd )
  doc <- sub( '^title: "Research Guide"', sprintf( 'title: "%s"\nsubtitle: "%s"', titles[ frame ], subs[ frame ] ), doc )
  doc <- sub( '^  dgf_file: .*$', '  dgf_file: "DGF.xlsx"', doc )
  writeLines( doc, qmd )
  st <- system2( quarto, c( "render", shQuote( normalizePath( qmd ) ) ) )
  if ( st != 0 ) stop( "quarto render failed for ", frame )
  file.copy( file.path( out, "research-guide.html" ), file.path( "site", "guides", paste0( frame, ".html" ) ), overwrite = TRUE )
  cat( frame, ": rendered", round( file.size( file.path( out, "research-guide.html" ) ) / 1e6, 1 ), "MB\n" )
}
