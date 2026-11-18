## md2qmd.R
## Convert a GitHub-flavored results note (notes/*.md) to a Quarto document (.qmd) next to
## it, and optionally render that to self-contained HTML.
##   Rscript R/lib/md2qmd.R notes/catchup-lag-results.md --render --subtitle="Catch-up results note"
## Arguments: the .md path; --render (also run quarto render); --subtitle="..." (optional).
## What it does:
##   - moves the first-line "# Title" into the YAML header (date: last-modified)
##   - drops a trailing "Updated Mon D, YYYY." sentence near the top (the YAML date replaces it)
##   - turns a blockquote that opens with a bold-only line ("> **How to read ...**") into a
##     Quarto callout-note titled with that line; other blockquotes are left alone
##   - inserts the blank lines Pandoc markdown needs and GitHub markdown does not: before a
##     top-level list that follows a paragraph, after a list followed directly by a paragraph,
##     and before and after pipe tables (without them, lists merge into the paragraph above)
## The .md stays the source: the .qmd is overwritten on every run, so edit the .md and re-run.
## Rendering needs quarto (it ships with RStudio).

args <- commandArgs( TRUE )
md <- args[ !grepl( "^--", args ) ][ 1 ]
if ( is.na( md ) || !file.exists( md ) )
  stop( "usage: Rscript R/lib/md2qmd.R notes/<note>.md [--render] [--subtitle=\"...\"]" )
render <- "--render" %in% args
subtitle <- sub( "^--subtitle=", "", grep( "^--subtitle=", args, value = TRUE ) )
qmd <- sub( "\\.md$", ".qmd", md )

x <- readLines( md, encoding = "UTF-8", warn = FALSE )
title <- tools::file_path_sans_ext( basename( md ) )
if ( length( x ) && grepl( "^# ", x[ 1 ] ) ) { title <- sub( "^# +", "", x[ 1 ] ); x <- x[ -1 ] }
top <- seq_len( min( 40, length( x ) ) )
x[ top ] <- sub( " Updated [A-Z][a-z]{2} [0-9]{1,2}, [0-9]{4}\\.$", "", x[ top ] )

is_item <- function( s ) grepl( "^[ \t]*([-*+]|[0-9]+\\.)[ \t]", s )     # any list item
is_top  <- function( s ) grepl( "^([-*+]|[0-9]+\\.)[ \t]", s )          # top-level list item
is_ind  <- function( s ) grepl( "^[ \t]+[^ \t]", s )                    # indented continuation
is_tab  <- function( s ) grepl( "^\\|", s )                             # pipe-table row

out <- character( 0 ); prev <- ""
emit <- function( s ) {
  blank <- FALSE
  if ( is_tab( s ) && prev != "" && !is_tab( prev ) ) {
    blank <- TRUE                                                       # a table starts a block
  } else if ( s != "" && is_tab( prev ) && !is_tab( s ) ) {
    blank <- TRUE                                                       # and ends one
  } else if ( s != "" && prev != "" && !grepl( "^(\\||:::|#)", prev ) && !grepl( "^(\\||:::)", s ) ) {
    if ( is_top( s ) && !is_item( prev ) && !is_ind( prev ) ) {
      blank <- TRUE                                                     # list after a paragraph
    } else if ( !is_item( s ) && !is_ind( s ) && ( is_item( prev ) || is_ind( prev ) ) && !grepl( "^#", s ) ) {
      blank <- TRUE                                                     # paragraph after a list
    }
  }
  if ( blank ) out <<- c( out, "" )
  out <<- c( out, s ); prev <<- s
}

incall <- FALSE
for ( i in seq_along( x ) ) {
  s <- x[ i ]
  if ( !incall && grepl( "^> \\*\\*.*\\*\\*$", s ) && i < length( x ) && grepl( "^>", x[ i + 1 ] ) ) {
    t <- sub( "\\.$", "", sub( "\\*\\*$", "", sub( "^> \\*\\*", "", s ) ) )
    if ( prev != "" ) out <- c( out, "" )
    out <- c( out, "::: {.callout-note}", paste( "##", t ), "" ); prev <- ""
    incall <- TRUE
  } else if ( incall && grepl( "^>", s ) ) {
    emit( sub( "^> ?", "", s ) )
  } else {
    if ( incall ) { out <- c( out, ":::" ); prev <- ":::"; incall <- FALSE }
    emit( s )
  }
}
if ( incall ) out <- c( out, ":::" )

q <- function( s ) paste0( "\"", gsub( "\"", "\\\\\"", s ), "\"" )
yaml <- c( "---", paste( "title:", q( title ) ),
  if ( length( subtitle ) ) paste( "subtitle:", q( subtitle ) ),
  "date: last-modified", "date-format: \"MMM D, YYYY\"",
  "format:", "  html:",
  "    toc: true", "    toc-location: left", "    toc-depth: 3", "    toc-expand: 1",
  "    number-sections: false", "    embed-resources: true", "    theme: cosmo",
  "    fontsize: 1rem", "    linestretch: 1.5", "    fig-align: center",
  "    fig-cap-location: bottom", "    tbl-cap-location: top", "    page-layout: full",
  "    grid:", "      sidebar-width: 260px", "      body-width: 1000px",
  "    lightbox: true", "---", "" )
con <- file( qmd, open = "wb" )                                         # "wb": LF line endings, like the .md
writeLines( enc2utf8( c( yaml, out ) ), con, useBytes = TRUE ); close( con )
cat( "wrote", qmd, "(", length( out ), "body lines )\n" )

if ( render ) {
  quarto <- Sys.which( "quarto" )
  if ( !nzchar( quarto ) ) quarto <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"
  if ( !file.exists( quarto ) && !nzchar( Sys.which( "quarto" ) ) ) stop( "quarto not found; render with: quarto render ", qmd )
  status <- system2( quarto, c( "render", shQuote( qmd ), "--to", "html" ) )
  if ( status != 0 ) stop( "quarto render failed (status ", status, ")" )
  cat( "rendered", sub( "\\.qmd$", ".html", qmd ), "\n" )
}
