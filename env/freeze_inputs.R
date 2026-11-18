## env/freeze_inputs.R   (Phase 4 Step 1 of REPRODUCIBILITY-PLAN.md)
## Freezes every raw input the build reads to s3://lecy/nonprofit-density/inputs/<folder>/.
##
##   Rscript env/freeze_inputs.R                    plan only: list files, sizes, origins
##   Rscript env/freeze_inputs.R --run [--folders a,b]   copy, hash, write manifests
##   Rscript env/freeze_inputs.R --verify           compare each frozen folder with its MANIFEST
##
## What is frozen: the raw column of pipeline.csv (<inputs folder>=<path>), plus EXTRA
## below (check-only files of steps outside pipeline.csv, cache files that let 05 and
## 09a skip their downloads, and the monthly-BMF data dictionaries).
##
## Layout of each folder:
##   <root>/<path>   root = build (MV_BUILD-relative path), hub (MV_INPUTS), bls (MV_BLS),
##                   dropbox (MV_DROPBOX); a file goes back to the same place on restore
##   MANIFEST.csv    key, bytes, sha256, source, build_path, steps, frozen_at
##   SOURCE.md, PERMISSION.md   copied from inputs/<folder>/
##
## Origins: a file under MV_BUILD is copied from the S3 mirror of the reference build
## (s3://lecy/multiverse/<path>) when the mirror has it, because those are the bytes the
## reference outputs were built from; otherwise from this machine. Data-hub, BLS and
## Dropbox files come from this machine. Mirror files are copied inside S3 and hashed
## from a temporary download; local files are hashed and uploaded.
##
## Folders frozen before this script (soi, nccs-bmf-unified) are not copied again;
## --verify checks that their manifests cover the files the build reads.
## Resumable: a file already listed in the folder's MANIFEST with the same size is skipped.
## Needs the AWS CLI with profile lecy-data (AWS_PROFILE overrides).

source( "R/lib/config.R" )
suppressPackageStartupMessages( library( data.table ) )
args <- commandArgs( TRUE )
RUN <- "--run" %in% args; VERIFY <- "--verify" %in% args
FOLDERS <- { i <- match( "--folders", args ); if ( is.na( i ) ) character( 0 ) else strsplit( args[ i + 1 ], "," )[[ 1 ]] }
PROFILE <- Sys.getenv( "AWS_PROFILE", "lecy-data" )
DEST <- "s3://lecy/nonprofit-density/inputs"
MIRROR <- "s3://lecy/multiverse"
DONE_ELSEWHERE <- c( soi = "s3://lecy/nonprofit-density/inputs/soi/",
                     "nccs-bmf-unified" = "s3://lecy/nonprofit-density/inputs/nccs-bmf/unified-bmf-geocoded/2026-09-21/" )
EXTRA <- c( "check-only=raw/irs_1023ez/baseline/",                       # 10b
            "check-only=raw/ntee_resolved/ntee_resolved_crosswalk.parquet",  # 10c
            "nccs-efile=raw/panel990/efile_grants_*.parquet",             # lets 09a skip its download
            "nccs-bmf-monthly=raw/bmf_monthly/bmf_20[0-9][0-9]_12_data_dictionary.csv" )
TMPD <- file.path( BUILD, "tmp", "freeze" ); dir.create( TMPD, recursive = TRUE, showWarnings = FALSE )

aws <- function( ..., ok = 0L ) {
  a <- c( ..., "--profile", PROFILE, if ( identical( ..2, "cp" ) ) "--only-show-errors" )
  out <- suppressWarnings( system2( "aws", shQuote( a ), stdout = TRUE, stderr = TRUE ) )
  st <- attr( out, "status" ); if ( !is.null( st ) && !st %in% ok ) stop( "aws ", paste( a[ 1:2 ], collapse = " " ), " failed: ", paste( out, collapse = " " ) )
  out
}
s3_ls <- function( prefix ) {
  x <- aws( "s3", "ls", paste0( prefix, "/" ), "--recursive", ok = 0:1 )     # 1 = nothing under the prefix
  x <- x[ grepl( "^\\d{4}-", x ) ]
  d <- data.table( bytes = as.numeric( sub( "^\\S+\\s+\\S+\\s+(\\d+)\\s.*$", "\\1", x ) ),
                   k = sub( "^\\S+\\s+\\S+\\s+\\d+\\s", "", x ) )
  setnames( d, "k", "key" )[]
}
sha256 <- function( f ) digest::digest( f, algo = "sha256", file = TRUE )
glob_re <- function( g ) paste0( "^", gsub( "\\*", "[^/]*", gsub( "\\?", "[^/]", gsub( "\\.", "\\\\.", g ) ) ),
                                 if ( endsWith( g, "/" ) ) ".+" else "", "$" )

## ---- file list ------------------------------------------------------------------------
p <- fread( "pipeline.csv", colClasses = "character", na.strings = character( 0 ) )
e <- p[ , .( e = trimws( strsplit( raw, ";", fixed = TRUE )[[ 1 ]] ) ), by = id ][ nzchar( e ) ]
e <- rbind( e, data.table( id = "extra", e = EXTRA ) )
e[ , `:=`( folder = sub( "=.*", "", e ), path = sub( "^[^=]*=", "", e ) ) ]
e[ , root := fcase( startsWith( path, "$INPUTS/" ), "hub", startsWith( path, "$BLS/" ), "bls",
                    startsWith( path, "$DROPBOX/" ), "dropbox", default = "build" ) ]
e[ , rel := sub( "^\\$(INPUTS|BLS|DROPBOX)/", "", path ) ]
base <- c( hub = DATA_HUB, bls = BLS_DIR, dropbox = DROPBOX, build = BUILD )
mir <- rbind( s3_ls( paste0( MIRROR, "/raw" ) ), s3_ls( paste0( MIRROR, "/data/geo" ) ) )[ , rel := sub( "^multiverse/", "", key ) ]

files <- rbindlist( lapply( seq_len( nrow( e ) ), function( i ) {
  r <- e[ i ]
  if ( r$root == "build" ) {
    m <- mir[ grepl( glob_re( r$rel ), rel ) ]
    if ( nrow( m ) ) return( m[ , .( folder = r$folder, root = "build", rel, bytes, source = paste0( MIRROR, "/", rel ), step = r$id ) ] )
  }
  f <- file.path( base[[ r$root ]], r$rel )
  f <- if ( endsWith( r$rel, "/" ) ) list.files( f, recursive = TRUE, full.names = TRUE )
       else if ( grepl( "[*?[]", f ) ) Sys.glob( f ) else f[ file.exists( f ) ]
  if ( !length( f ) ) return( data.table( folder = r$folder, root = r$root, rel = r$rel, bytes = NA_real_, source = NA_character_, step = r$id ) )
  data.table( folder = r$folder, root = r$root, rel = substring( normalizePath( f, "/" ), nchar( normalizePath( base[[ r$root ]], "/" ) ) + 2 ),
              bytes = file.size( f ), source = normalizePath( f, "/" ), step = r$id )
} ) )
files <- files[ , .( bytes = bytes[ 1 ], source = source[ 1 ], steps = paste( unique( step ), collapse = "," ) ), by = .( folder, root, rel ) ]
files[ , `:=`( key = paste0( root, "/", rel ),
               build_path = fifelse( root == "build", rel, paste0( "$", toupper( c( hub = "inputs", bls = "bls", dropbox = "dropbox" )[ root ] ), "/", rel ) ) ) ]
missing <- files[ is.na( source ) ]
if ( nrow( missing ) ) { print( missing[ , .( folder, root, rel, steps ) ] ); stop( nrow( missing ), " declared raw inputs resolve to no file" ) }
todo <- files[ !folder %in% names( DONE_ELSEWHERE ) ]
if ( length( FOLDERS ) ) { stopifnot( all( FOLDERS %in% files$folder ) ); todo <- todo[ folder %in% FOLDERS ] }

cat( sprintf( "%d files in %d folders, %.2f GB (%.2f GB from the mirror, %.2f GB from this machine); %d files in folders frozen earlier\n",
              nrow( todo ), uniqueN( todo$folder ), sum( todo$bytes ) / 1e9, sum( todo[ startsWith( source, "s3://" ) ]$bytes ) / 1e9,
              sum( todo[ !startsWith( source, "s3://" ) ]$bytes ) / 1e9, nrow( files[ folder %in% names( DONE_ELSEWHERE ) ] ) ) )
print( todo[ , .( files = .N, MB = round( sum( bytes ) / 1e6, 1 ), from = paste( unique( fifelse( startsWith( source, "s3://" ), "mirror", "local" ) ), collapse = "+" ) ), keyby = folder ] )
fwrite( files, file.path( TMPD, "freeze_plan.csv" ) )

## ---- run ------------------------------------------------------------------------------
if ( RUN ) for ( fo in unique( todo$folder ) ) {
  x <- todo[ folder == fo ]
  dst <- paste0( DEST, "/", fo )
  have <- s3_ls( dst )
  manf <- file.path( TMPD, paste0( fo, "_MANIFEST.csv" ) )
  man <- if ( paste0( "nonprofit-density/inputs/", fo, "/MANIFEST.csv" ) %in% have$key ) {
    aws( "s3", "cp", paste0( dst, "/MANIFEST.csv" ), manf ); fread( manf, colClasses = list( character = "sha256" ) )
  } else setnames( data.table( k = character( 0 ), bytes = numeric( 0 ), sha256 = character( 0 ), source = character( 0 ),
                               build_path = character( 0 ), steps = character( 0 ), frozen_at = character( 0 ) ), "k", "key" )
  t_up <- Sys.time()
  for ( i in seq_len( nrow( x ) ) ) {
    r <- x[ i ]
    done <- man[ key == r$key & bytes == r$bytes ]
    if ( nrow( done ) && paste0( "nonprofit-density/inputs/", fo, "/", r$key ) %in% have$key ) next
    cat( format( Sys.time(), "%H:%M:%S" ), fo, r$key, round( r$bytes / 1e6, 1 ), "MB\n" )
    if ( startsWith( r$source, "s3://" ) ) {
      tf <- file.path( TMPD, "blob" )
      aws( "s3", "cp", r$source, tf ); h <- sha256( tf )
      stopifnot( file.size( tf ) == r$bytes ); unlink( tf )
      ## server-side copy; --copy-props none: lecy-data may not read object tags, which
      ## a multipart copy (files over 8 MB) otherwise asks for
      aws( "s3", "cp", r$source, paste0( dst, "/", r$key ), "--copy-props", "none" )
    } else {
      h <- sha256( r$source )
      aws( "s3", "cp", r$source, paste0( dst, "/", r$key ) )
    }
    new <- setnames( data.table( k = r$key, bytes = r$bytes, sha256 = h, source = r$source, build_path = r$build_path,
                                 steps = r$steps, frozen_at = format( Sys.time(), "%Y-%m-%d %H:%M:%S" ) ), "k", "key" )
    man <- rbind( man[ key != r$key ], new )
    fwrite( man, manf )
    if ( difftime( Sys.time(), t_up, units = "secs" ) > 60 ) {                # resumable: at most a minute lost
      aws( "s3", "cp", manf, paste0( dst, "/MANIFEST.csv" ) ); t_up <- Sys.time() }
  }
  if ( nrow( man ) ) { fwrite( man, manf ); aws( "s3", "cp", manf, paste0( dst, "/MANIFEST.csv" ) ) }
  for ( d in c( "SOURCE.md", "PERMISSION.md" ) ) {
    f <- file.path( "inputs", fo, d ); if ( file.exists( f ) ) aws( "s3", "cp", f, paste0( dst, "/", d ) )
  }
  cat( "== ", fo, ": ", nrow( man ), " files in MANIFEST\n", sep = "" )
}

## ---- verify ---------------------------------------------------------------------------
if ( VERIFY ) {
  res <- rbindlist( lapply( unique( files$folder ), function( fo ) {
    x <- files[ folder == fo ]
    if ( fo %in% names( DONE_ELSEWHERE ) ) {
      pre <- DONE_ELSEWHERE[[ fo ]]; ls <- s3_ls( sub( "/$", "", pre ) )
      key0 <- sub( "^s3://lecy/", "", pre )
      got <- basename( ls$key )
      return( data.table( folder = fo, files = nrow( x ), in_manifest = sum( basename( x$rel ) %in% got ),
                          size_ok = NA_integer_, uploaded = NA_integer_, note = paste( "frozen earlier at", pre ) ) )
    }
    dst <- paste0( DEST, "/", fo ); ls <- s3_ls( dst )
    manf <- file.path( TMPD, paste0( fo, "_MANIFEST.csv" ) )
    if ( !paste0( "nonprofit-density/inputs/", fo, "/MANIFEST.csv" ) %in% ls$key )
      return( data.table( folder = fo, files = nrow( x ), in_manifest = 0L, size_ok = 0L, uploaded = 0L, note = "not frozen" ) )
    aws( "s3", "cp", paste0( dst, "/MANIFEST.csv" ), manf ); man <- fread( manf, colClasses = list( character = "sha256" ) )
    up <- ls[ , .( key = sub( paste0( "^nonprofit-density/inputs/", fo, "/" ), "", key ), up_bytes = bytes ) ]
    m <- merge( merge( x[ , .( key, bytes ) ], man[ , .( key, mbytes = bytes, sha256 ) ], by = "key", all.x = TRUE ), up, by = "key", all.x = TRUE )
    data.table( folder = fo, files = nrow( x ), in_manifest = sum( !is.na( m$sha256 ) ),
                size_ok = sum( m$bytes == m$mbytes & m$bytes == m$up_bytes, na.rm = TRUE ), uploaded = sum( !is.na( m$up_bytes ) ),
                note = paste( intersect( c( "SOURCE.md", "PERMISSION.md" ), up$key ), collapse = " " ) )
  } ) )
  print( res, nrows = 100 )
  ok <- res[ is.na( size_ok ), all( in_manifest == files ) ] && res[ !is.na( size_ok ), all( size_ok == files & in_manifest == files ) ]
  cat( if ( ok ) "\nVERIFY PASS\n" else "\nVERIFY FAIL\n" )
}
