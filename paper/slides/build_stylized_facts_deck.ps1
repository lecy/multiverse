# build_stylized_facts_deck.ps1
# Draft deck for paper/01-stylized-facts.qmd, built by driving PowerPoint (COM).
# Usage (Windows PowerShell, from multiverse/paper/slides):  powershell -File build_stylized_facts_deck.ps1 [-Export <png folder>]
# Output: stylized-facts.pptx next to this script. Figures come from ../figures.
param( [string]$Export = "" )
$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$fig  = Join-Path (Split-Path -Parent $here) "figures"
$out  = Join-Path $here "stylized-facts.pptx"

# ---- palette (matches the paper's figures: blue 2A78D6, orange EB6834) ----------------------
$DARK = "1D2939"; $INK = "1F2933"; $MUTED = "5F6B7A"; $FAINT = "8A94A3"
$ORANGE = "EB6834"; $BLUE = "2A78D6"; $TINT = "F1F4F8"; $WARM = "FDF0EA"; $LIGHT = "C9D3DF"
$HEAD = "Cambria"; $BODY = "Calibri"

function C( [string]$h ) { [Convert]::ToInt32( $h.Substring( 0, 2 ), 16 ) + 256 * [Convert]::ToInt32( $h.Substring( 2, 2 ), 16 ) + 65536 * [Convert]::ToInt32( $h.Substring( 4, 2 ), 16 ) }
function TF( $b ) { if ( $b ) { -1 } else { 0 } }

function T( $s, [string]$text, $l, $t, $w, $h, $size, [string]$color = $INK, $bold = $false, [string]$font = $BODY, [int]$align = 1, $italic = $false, [string]$name = "", [int]$anchor = 1 ) {
  $sh = $s.Shapes.AddTextbox( 1, $l, $t, $w, $h )
  $tf = $sh.TextFrame; $tf.WordWrap = -1; $tf.AutoSize = 0
  $tf.MarginLeft = 0; $tf.MarginRight = 0; $tf.MarginTop = 0; $tf.MarginBottom = 0
  $tr = $tf.TextRange; $tr.Text = $text
  $sh.TextFrame2.AutoSize = 0; $sh.Height = $h; $sh.TextFrame2.VerticalAnchor = $anchor
  $tr.Font.Name = $font; $tr.Font.Size = $size; $tr.Font.Bold = ( TF $bold ); $tr.Font.Italic = ( TF $italic )
  $tr.Font.Color.RGB = ( C $color ); $tr.ParagraphFormat.Alignment = $align
  if ( $name ) { $sh.Name = $name }
  $sh
}
function Bul( $s, [string[]]$items, $l, $t, $w, $h, $size = 16, [string]$color = $INK, [string]$name = "" ) {
  $sh = T $s ( $items -join "`r" ) $l $t $w $h $size $color $false $BODY 1 $false $name
  $pf = $sh.TextFrame.TextRange.ParagraphFormat
  $pf.Bullet.Visible = -1; $pf.Bullet.Character = 8226; $pf.Bullet.Font.Color.RGB = ( C $ORANGE ); $pf.SpaceAfter = 8
  $sh.TextFrame.Ruler.Levels.Item( 1 ).FirstMargin = 0; $sh.TextFrame.Ruler.Levels.Item( 1 ).LeftMargin = 16
  $sh
}
function Box( $s, $l, $t, $w, $h, [string]$fill, [int]$type = 5, [string]$name = "" ) {
  $b = $s.Shapes.AddShape( $type, $l, $t, $w, $h ); $b.Fill.ForeColor.RGB = ( C $fill ); $b.Line.Visible = 0
  if ( $type -eq 5 ) { $b.Adjustments.Item( 1 ) = 0.06 }
  if ( $name ) { $b.Name = $name }
  $b
}
function Badge( $s, [string]$n, $l = 40, $t = 30, $d = 40, [string]$fill = $ORANGE ) {
  $o = $s.Shapes.AddShape( 9, $l, $t, $d, $d ); $o.Fill.ForeColor.RGB = ( C $fill ); $o.Line.Visible = 0; $o.Name = "Fact badge $n"
  $tf = $o.TextFrame; $tf.MarginLeft = 0; $tf.MarginRight = 0; $tf.MarginTop = 0; $tf.MarginBottom = 0; $tf.VerticalAnchor = 3
  $tr = $tf.TextRange; $tr.Text = $n; $tr.Font.Name = $HEAD; $tr.Font.Size = [int]( $d * 0.5 ); $tr.Font.Bold = -1
  $tr.Font.Color.RGB = ( C "FFFFFF" ); $tr.ParagraphFormat.Alignment = 2
  $o
}
function Head( $s, [string]$n, [string]$title ) {
  Badge $s $n | Out-Null
  T $s $title 94 28 826 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
}
function Num( $s, $i = 0 ) { T $s "$( $s.SlideIndex )" 880 512 40 14 10 $FAINT $false $BODY 3 $false "Slide number" | Out-Null }
function Divider( $pres, [string]$part, [string]$title, [string]$sub ) {
  $s = NewSlide $pres $DARK
  Badge $s $part 60 140 64 | Out-Null
  T $s $title 60 228 840 64 40 "FFFFFF" $true $HEAD 1 $false "Title" 3 | Out-Null
  T $s $sub 60 300 840 60 19 $LIGHT $false $HEAD 1 $true "Subtitle" | Out-Null
  $s
}
function Arrow( $s, $x1, $y1, $x2, $y2, [string]$color = $FAINT ) {
  $a = $s.Shapes.AddLine( $x1, $y1, $x2, $y2 ); $a.Line.ForeColor.RGB = ( C $color ); $a.Line.Weight = 2; $a.Line.EndArrowheadStyle = 2; $a.Name = "Arrow"
}
function Pic( $s, [string]$file, $l, $t, $maxW, $maxH, [string]$alt ) {
  $p = $s.Shapes.AddPicture( ( Join-Path $fig $file ), 0, -1, $l, $t, -1, -1 )
  $p.LockAspectRatio = -1; $p.Width = $maxW
  if ( $p.Height -gt $maxH ) { $p.Height = $maxH }
  $p.Left = $l + ( $maxW - $p.Width ) / 2; $p.Top = $t + ( $maxH - $p.Height ) / 2
  $p.AlternativeText = $alt; $p.Name = "Figure"
  $p
}
function Stat( $s, $l, $t, $w, $h, [string]$num, [string]$label, [string]$fill = $TINT ) {
  Box $s $l $t $w $h $fill 5 "Stat card" | Out-Null
  T $s $num ( $l + 18 ) ( $t + 16 ) ( $w - 36 ) 50 38 $BLUE $true $HEAD 1 $false "Stat number" | Out-Null
  T $s $label ( $l + 18 ) ( $t + 72 ) ( $w - 36 ) ( $h - 84 ) 14 $INK $false $BODY 1 $false "Stat label" | Out-Null
}
function Design( $s, [string[]]$items, $l = 40, $t = 352, $w = 880, $h = 146 ) {
  Box $s $l $t $w $h $WARM 5 "Design box" | Out-Null
  T $s "What it means for design" ( $l + 20 ) ( $t + 14 ) ( $w - 40 ) 20 15 "B4471A" $true $BODY 1 $false "Design header" | Out-Null
  Bul $s $items ( $l + 20 ) ( $t + 42 ) ( $w - 40 ) ( $h - 52 ) 15 $INK "Design bullets" | Out-Null
}
function Claim( $s, [string]$text ) { T $s $text 40 92 880 60 19 $INK $false $HEAD 1 $true "Claim" | Out-Null }
function Tbl( $s, $data, $l, $t, [double[]]$colW, $size = 13, $hilite = @() ) {
  $rows = $data.Count; $cols = $data[0].Count
  $sh = $s.Shapes.AddTable( $rows, $cols, $l, $t, ( $colW | Measure-Object -Sum ).Sum, $rows * 22 ); $sh.Name = "Table"
  $tb = $sh.Table; $tb.ApplyStyle( "{2D5ABB26-0587-4C30-8999-92F81FD0307C}", $false )
  for ( $j = 1; $j -le $cols; $j++ ) { $tb.Columns.Item( $j ).Width = $colW[ $j - 1 ] }
  for ( $i = 1; $i -le $rows; $i++ ) {
    for ( $j = 1; $j -le $cols; $j++ ) {
      $cell = $tb.Cell( $i, $j ); $cs = $cell.Shape; $tf = $cs.TextFrame
      $tf.MarginLeft = 8; $tf.MarginRight = 8; $tf.MarginTop = 4; $tf.MarginBottom = 4; $tf.VerticalAnchor = 3
      $tr = $tf.TextRange; $tr.Text = [string]$data[ $i - 1 ][ $j - 1 ]; $tr.Font.Name = $BODY; $tr.Font.Size = $size
      $tr.ParagraphFormat.Alignment = $( if ( $j -eq 1 ) { 1 } else { 2 } )
      if ( $i -eq 1 ) { $cs.Fill.Visible = -1; $cs.Fill.Solid(); $cs.Fill.ForeColor.RGB = ( C $DARK ); $tr.Font.Color.RGB = ( C "FFFFFF" ); $tr.Font.Bold = -1 }
      else {
        $cs.Fill.Visible = -1; $cs.Fill.Solid(); $cs.Fill.ForeColor.RGB = ( C $( if ( $i % 2 -eq 0 ) { "FFFFFF" } else { $TINT } ) )
        $tr.Font.Color.RGB = ( C $INK ); $tr.Font.Bold = ( TF ( $j -eq 1 ) )
      }
    }
    $tb.Rows.Item( $i ).Height = 22
  }
  foreach ( $hc in $hilite ) { $r = $tb.Cell( $hc[ 0 ], $hc[ 1 ] ).Shape.TextFrame.TextRange; $r.Font.Color.RGB = ( C "C2410C" ); $r.Font.Bold = -1 }
  $sh
}
function Notes( $s, [string]$txt ) { $s.NotesPage.Shapes.Placeholders.Item( 2 ).TextFrame.TextRange.Text = $txt }
function NewSlide( $pres, [string]$bg = "FFFFFF" ) {
  $s = $pres.Slides.Add( $pres.Slides.Count + 1, 12 )
  $s.FollowMasterBackground = 0; $s.Background.Fill.Solid(); $s.Background.Fill.ForeColor.RGB = ( C $bg )
  $s
}

# ---- presentation -------------------------------------------------------------------------------
$app = New-Object -ComObject PowerPoint.Application
$pres = $app.Presentations.Add( 0 )
$pres.PageSetup.SlideWidth = 960; $pres.PageSetup.SlideHeight = 540
$pres.SlideMaster.Theme.ThemeFontScheme.MajorFont.Item( 1 ).Name = $HEAD
$pres.SlideMaster.Theme.ThemeFontScheme.MinorFont.Item( 1 ).Name = $BODY

# 1. title ------------------------------------------------------------------------------------------
$s = NewSlide $pres $DARK
T $s "Nonprofit density: data, stylized facts and a multiverse" 60 100 840 110 40 "FFFFFF" $true $HEAD 1 $false "Title" 4 | Out-Null
T $s "Seven regularities any density design has to confront, and the design decisions that change the answer" 60 232 840 34 20 $LIGHT $false $HEAD 1 $true "Subtitle" | Out-Null
T $s "Jesse Lecy and Jeremy P. Thornton" 60 302 840 24 16 "FFFFFF" $false $BODY 1 $false "Authors" | Out-Null
T $s "501(c)(3) public charities in 387 US metros, 2000–2024" 60 328 840 22 14 $LIGHT $false $BODY 1 $false "Data" | Out-Null
for ( $k = 1; $k -le 7; $k++ ) { Badge $s "$k" ( 60 + ( $k - 1 ) * 52 ) 430 36 | Out-Null }
Notes $s "Seven regularities hold in metro-level 501(c)(3) density data before any covariate enters a model. Each is descriptive, and together they motivate the design decisions D1–D12. Data cover 387 US metros, 2000–2024, unless noted."

# ---- part 1: data --------------------------------------------------------------------------------------
$s = Divider $pres "1" "The data" "Four nonprofit sources spliced into one county-by-year panel, 2000–2024, with covariates from ten public sources"
$secData = $s.SlideIndex
Notes $s "Every source, version, acquisition and slice is documented in DATA-MANIFEST.md in the project folder."

# data: the splice timeline
$s = NewSlide $pres
T $s "Four nonprofit sources, spliced into one 2000–2024 panel" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$X0 = 230; $YW = 27.2                                       # x of 2000 and points per year (25 years to 2024)
function YX( $yr ) { $X0 + ( $yr - 2000 ) * $YW }
foreach ( $yr in @( 2000, 2005, 2010, 2015, 2020, 2024 ) ) { T $s "$yr" ( ( YX $yr ) - 2 ) 92 50 16 12 $MUTED $false $BODY 1 $false "Year tick" | Out-Null }
$rows = @( @( "990 / 990-EZ financials", 116 ), @( "990-PF foundations", 176 ), @( "Registered stock (IRS BMF)", 236 ), @( "BLS nonprofit employment", 316 ) )
foreach ( $r in $rows ) { T $s $r[ 0 ] 40 ( $r[ 1 ] + 6 ) 180 34 14 $INK $true $BODY 1 $false "Row label" | Out-Null }
function Seg( $s, $y, $a, $b, [string]$fill, [string]$label, [string]$tc = "FFFFFF" ) {
  $l = YX $a; $w = ( YX ( $b + 1 ) ) - $l - 2
  $r = Box $s $l $y $w 44 $fill 1 "Source bar"
  $tf = $r.TextFrame; $tf.MarginLeft = 4; $tf.MarginRight = 4; $tf.MarginTop = 0; $tf.MarginBottom = 0; $tf.WordWrap = -1
  $tr = $tf.TextRange; $tr.Text = $label; $tr.Font.Name = $BODY; $tr.Font.Size = 12; $tr.Font.Color.RGB = ( C $tc ); $tr.ParagraphFormat.Alignment = 2
}
Seg $s 116 2000 2010 $BLUE "NCCS CORE (harmonized)"
Seg $s 116 2011 2019 "1BAF7A" "IRS SOI annual extracts"
Seg $s 116 2020 2024 $ORANGE "NCCS e-file (990 XML)"
Seg $s 176 2000 2015 $BLUE "NCCS CORE private foundations"
Seg $s 176 2016 2018 $LIGHT "interpolated" $INK
Seg $s 176 2019 2020 "1BAF7A" "SOI PF"
Seg $s 176 2021 2024 $ORANGE "e-file"
Seg $s 236 2000 2022 $DARK "NCCS legacy BMF snapshots → reconstructed stock (first and last appearance, ruling year)"
Seg $s 236 2023 2024 "5F6B7A" "monthly BMF"
Seg $s 316 2013 2022 "8A94A3" "QCEW nonprofit research data"
foreach ( $yr in @( 2011, 2020 ) ) {
  $ln = $s.Shapes.AddLine( ( YX $yr ) - 1, 108, ( YX $yr ) - 1, 284 ); $ln.Line.ForeColor.RGB = ( C $DARK ); $ln.Line.DashStyle = 4; $ln.Line.Weight = 1.25; $ln.Name = "Splice line"
}
T $s "splice" ( ( YX 2011 ) - 28 ) 288 56 16 11 $MUTED $false $BODY 2 $false "Splice label" | Out-Null
T $s "splice" ( ( YX 2020 ) - 28 ) 288 56 16 11 $MUTED $false $BODY 2 $false "Splice label" | Out-Null
Box $s 40 384 880 116 $TINT 5 "Note card" | Out-Null
Bul $s @( "Every organization is placed in a 2023 county (Connecticut planning regions) and summed to 387 metros and 925 metro and micro areas (OMB 2023).",
          "Population from Census estimates 2000–2025; dollars in 2022 dollars (PCE).",
          "Filers jump +13% at the 2011 splice and fall about 9% in 2020, the first e-file year; count rankings barely move (Fact 7)." ) 58 396 846 96 14 $INK "Notes" | Out-Null
Num $s
Notes $s "990 and 990-EZ: NCCS CORE 2000–2010, IRS SOI extracts 2011–2019, NCCS e-file 2020–2024 (2020 switches to e-file because SOI's 2020 EZ extract is short). 990-PF: CORE to 2015, interpolated 2016–2018 because the IRS published no 990-PF extract for filing years 2017–2019, SOI PF 2019–2020, e-file from 2021. The registered stock is reconstructed: an EIN counts from its ruling year to the last year it appears, because the 2016–18 BMF snapshots omit most newly ruled organizations. BLS nonprofit research data cover 2013–2022 only."

# data: the four frames
$s = NewSlide $pres
T $s "Four frames, four questions" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$fw = 205
$frames = @(
  @( "1.42M", "Registered", "501(c)(3) public charities in the IRS BMF stock. Includes 990-N postcard filers, churches that register and group subordinates." ),
  @( "323,000", "Filers", "990 or 990-EZ with revenue or expenses of at least `$50,000 in 2010 dollars. 23% of the registered stock. The formal, fiscally active sector." ),
  @( "1.09M", "Grassroots", "Registered minus filers: postcard and small filers, churches, subordinates. Small and informal organizations." ),
  @( "`$2.9T", "Expenditures", "Total expenses of filers, 2022 dollars. Hospitals and universities are 51%, so the default excludes them (`$1.45T)." ) )
for ( $k = 0; $k -lt 4; $k++ ) {
  $l = 40 + $k * ( $fw + 20 ); $f = $frames[ $k ]
  Box $s $l 96 $fw 250 $TINT 5 "Frame card" | Out-Null
  T $s $f[ 0 ] ( $l + 16 ) 112 ( $fw - 32 ) 46 36 $BLUE $true $HEAD 1 $false "Frame number" | Out-Null
  T $s $f[ 1 ] ( $l + 16 ) 164 ( $fw - 32 ) 24 18 $DARK $true $HEAD 1 $false "Frame name" | Out-Null
  T $s $f[ 2 ] ( $l + 16 ) 196 ( $fw - 32 ) 140 14 $INK $false $BODY 1 $false "Frame definition" | Out-Null
}
T $s "National totals, 2022. In the 387 metros: 1.19 million registered and 288,000 filers in 2024." 40 358 880 20 13 $MUTED $false $BODY 1 $true "Footnote" | Out-Null
Box $s 40 392 880 76 $WARM 5 "Design box" | Out-Null
T $s "Filers answer questions about the formal sector, grassroots about small and informal organizations, expenditures about resources. They rank metros differently and respond to different covariates (Fact 4, D1)." 58 402 846 56 16 $INK $false $BODY 1 $false "Takeaway" 3 | Out-Null
Num $s
Notes $s "The filer floor is `$40,780 nominal in 2000, `$50,000 in 2010, `$57,818 in 2020 and `$68,311 in 2024. 990-PF filers are excluded from the filer count. Grassroots can be zero or negative in a few small metros. Hospitals (NTEE E20–E24) and higher education (B40–B43, B50) account for 51–56% of active 501(c)(3) expenses in every year."

# data: covariate sources
$s = NewSlide $pres
T $s "Thirteen covariates from ten public sources" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$dc = @( @( "Construct", "Covariate", "Source", "Observed" ),
         @( "Need", "Poverty rate", "Census SAIPE", "Annual" ),
         @( "Need", "% 65+", "Census population estimates", "Annual" ),
         @( "Heterogeneity", "Gini (metro)", "ACS 5-year income brackets, summed to metros", "2010–2024" ),
         @( "Heterogeneity", "% nonwhite", "Census population estimates", "Annual" ),
         @( "Heterogeneity", "Religious fractionalization", "U.S. Religion Census (RCMS)", "2000, 2010, 2020" ),
         @( "Heterogeneity", "Age SD", "Census population estimates, age bins", "Annual" ),
         @( "Wealth", "Foundation assets", "990-PF (CORE, SOI, e-file)", "Annual; 2016–18 interpolated" ),
         @( "Education", "% BA", "Census 2000, ACS 5-year", "2000, 2009–2024" ),
         @( "Social capital", "Turnout", "County presidential returns / adults", "Election years" ),
         @( "Growth", "Growth kernel (10-year)", "Census county population, 1970–2024", "Annual" ),
         @( "Government, direct", "S&L government earnings", "BEA regional accounts (CAINC5)", "Annual" ),
         @( "Government, indirect", "Medicaid", "BEA transfer receipts (CAINC35)", "Annual" ),
         @( "History", "Pre-1970 endowment", "IRS BMF: share of the 2000 stock ruled before 1970", "Fixed" ) )
Tbl $s $dc 40 84 @( 160, 200, 330, 190 ) 13 | Out-Null
T $s "Totals (foundation assets, earnings, Medicaid, the endowment) enter in SAMI form: logged, with a per-capita floor for zeros, and residualized on log population. Full versions and slices: DATA-MANIFEST.md." 40 460 880 40 13 $MUTED $false $BODY 1 $true "Footnote" | Out-Null
Num $s
Notes $s "The 13 covariates are the lean set of the variables chapter: one item per construct, with diversity and government split into the separate constructs they contain. Turnout is observed only in presidential election years and interpolated between them; the Gini, % BA and religion are interpolated between census or ACS years. Fact 7 reports how much of each covariate is observed rather than filled in."

# ---- part 2: stylized facts --------------------------------------------------------------------------
$s = Divider $pres "2" "Seven stylized facts" "Regularities in metro nonprofit density that hold before any covariate enters a model"
$secFacts = $s.SlideIndex
Notes $s "Each fact is descriptive; together they motivate the design decisions."

# 2. overview ---------------------------------------------------------------------------------------
$s = NewSlide $pres
T $s "Seven regularities in metro nonprofit density" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$cards = @(
  @( "1", "Counts scale with population; spending scales faster", "Counts β ≈ 1.00 at every unit; spending 1.23–1.46" ),
  @( "2", "Density is nearly frozen", "Filer density correlates .94 twenty years apart" ),
  @( "3", "Density runs on two clocks", "Catch-up takes ~5 years within a metro, decades between metros" ),
  @( "4", "Registered charities are two populations", "Filers follow the economy; grassroots follow demography and history" ),
  @( "5", "Market share sets the level, not the pace", "Nonprofit employment share explains a third of filer density" ),
  @( "6", "Size is the main source of collinearity", "Size-scaled covariates are near-copies of population" ),
  @( "7", "Data hazards are frequent and patterned", "Source splices, a tax law and registration practices" ) )
$cw = 208; $ch = 196
for ( $k = 0; $k -lt 8; $k++ ) {
  $col = $k % 4; $row = [math]::Floor( $k / 4 ); $l = 40 + $col * ( $cw + 16 ); $t = 96 + $row * ( $ch + 16 )
  if ( $k -lt 7 ) {
    $c = $cards[ $k ]
    Box $s $l $t $cw $ch $TINT 5 "Fact card $( $c[0] )" | Out-Null
    Badge $s $c[ 0 ] ( $l + 16 ) ( $t + 16 ) 34 | Out-Null
    T $s $c[ 1 ] ( $l + 16 ) ( $t + 62 ) ( $cw - 32 ) 62 16 $DARK $true $HEAD 1 $false "Card title" | Out-Null
    T $s $c[ 2 ] ( $l + 16 ) ( $t + 128 ) ( $cw - 32 ) 58 13 $MUTED $false $BODY 1 $false "Card text" | Out-Null
  } else {
    Box $s $l $t $cw $ch $DARK 5 "Data card" | Out-Null
    T $s "The data" ( $l + 16 ) ( $t + 18 ) ( $cw - 32 ) 24 16 "FFFFFF" $true $HEAD 1 $false "Card title" | Out-Null
    T $s ( "Filers: 990/990-EZ filers above a `$50k real floor`rGrassroots: registered public charities that do not file`rSpending: expenses excl. hospitals and universities" ) ( $l + 16 ) ( $t + 50 ) ( $cw - 32 ) 136 12 $LIGHT $false $BODY 1 $false "Card text" | Out-Null
  }
}
Num $s 2
Notes $s "The seven facts. Facts 1, 2 and 3 are about the outcome itself: how it scales, how fixed it is, and how it adjusts. Fact 4 says the registered stock mixes two populations. Fact 5 is about the institutional level of the sector. Facts 6 and 7 are about the covariates and the data."

# 3. fact 1 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "1" "Counts scale with population; spending does not"
Claim $s "Organization counts grow in proportion to population. Spending grows faster, and how much faster depends on the geographic unit. This one fact decides whether per-capita measures are safe."
$w3 = 280
Stat $s 40 176 $w3 156 "0.99" "Scaling exponent for filer counts, 387 metros (Poisson 1.00–1.02 at every unit)"
Stat $s ( 40 + $w3 + 20 ) 176 $w3 156 "1.29" "Exponent for spending excl. hospitals and universities: +95% per resident per tenfold population"
Stat $s ( 40 + 2 * ( $w3 + 20 ) ) 176 $w3 156 "1.46 → 1.23" "Spending exponent from county to CSA (OLS); counts stay at 0.98–0.99"
Design $s @( "Counts per capita are acceptable at the metro level; spending per capita is not. Use log-log or SAMI forms (D4).",
             "Report β with its unit and estimator, and say whether it is bivariate or conditional on covariates (0.99 vs. 0.88).",
             "Use Poisson when zeros are common: hospitals at the county level give 0.36 by OLS and 0.81 by Poisson." )
Num $s 3
Notes $s "Filers scale at 0.99, so filers per capita are size-neutral: they change by −3% per tenfold population. Spending scales at 1.29, so spending per resident rises 95% per tenfold population and per capita ranks large metros above small ones for size alone. The spending exponent depends on the unit (county 1.46, metro 1.27, CSA 1.23) because big-budget organizations sit in core counties. Counts look sublinear (0.88) only after conditioning on covariates."

# 4. fact 1 figure ------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "1" "Per capita is size-neutral for filers, not for spending"
Pic $s "f1_scaling.png" 40 86 560 430 "Four panels: filers and spending against population on log axes (exponents 0.99 and 1.29) and the per-capita versions" | Out-Null
Bul $s @( "Filers per capita fall only 3% per tenfold increase in population; size explains 0.1% of their variance.",
          "Spending per resident rises 95% per tenfold increase; size explains 16% of its variance.",
          "The SAMI and log filers per capita correlate .999; for spending only .92." ) 628 120 292 300 16 $INK "Takeaways" | Out-Null
T $s "387 metros, 2022" 628 470 292 18 11 $FAINT $false $BODY 1 $false "Source" | Out-Null
Num $s 4
Notes $s "Panels A and B: the fitted exponents against the dashed proportional line. Panels C and D: the same data per capita. A per-capita measure assumes β = 1. Where β > 1, per-capita values rank large metros above small ones for size alone."

# 5. fact 2 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "2" "Density is nearly frozen"
Claim $s "A metro's filer density in 2000 predicts its density in 2024 almost perfectly, and the spread between metros has not narrowed. Grassroots and spending move more, but still slowly."
Stat $s 40 176 $w3 156 ".94" "Correlation of filer density (SAMI) twenty years apart; more persistent than metro GDP (.78)"
Stat $s ( 40 + $w3 + 20 ) 176 $w3 156 "97.5%" "Share of filer-density variance between metros; within a metro the SD is 0.06 log points"
Stat $s ( 40 + 2 * ( $w3 + 20 ) ) 176 $w3 156 "~240 yrs" "Half-life of convergence for filers; the cross-metro SD was .40 in both 2000 and 2024"
Design $s @( "Within-metro designs work with about 2.5% of the outcome's variance and mostly interpolated covariate change: weakly identified (D3).",
             "Pooling years adds little to one cross-section. Cluster by metro and count degrees of freedom in metros.",
             "Persistence makes history a confounder in levels models (Fact 3, D8)." )
Num $s 5
Notes $s "Persistence by outcome: filers .99 at one year, .94 at twenty; grassroots .78 and spending .74 at twenty, about as persistent as GDP. 97.5% of the filer SAMI variance is between metros. No convergence: λ = −.064 over 24 years, a half-life of about 240 years; instrumenting the 2000 level with the registered stock gives −.060, so it is not measurement error. Grassroots narrowed slightly (half-life about 27 years); spending widened."

# 6. fact 2 figure ------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "2" "The few large moves are real but rare"
Pic $s "f2_rank_paths.png" 40 84 400 440 "Percentile rank paths of the filer SAMI, 2000 to 2024: steady metros, the 12 largest climbers and the 12 largest decliners" | Out-Null
Bul $s @( "The top and bottom 10% of metros moved about ±25% relative to expected size in 24 years.",
          "Climbers: New Orleans (+33 percentile points), Chico (+31), Cleveland TN (+28). Decliners: Decatur IL and Hagerstown (−33), Tyler (−29).",
          "Risers start low (26th percentile) and have thin historical endowments.",
          "Growth alone does not predict the change (R² = .002); conditional on the starting level, the West and South drifted up." ) 470 110 450 360 16 $INK "Takeaways" | Out-Null
Num $s 6
Notes $s "Who moved: risers average +.22 in SAMI, start at the 26th percentile, 44% in the West, with a pre-1970 endowment SAMI of −.19. Fallers start near the middle and grew faster. With region and census traits fixed, the change falls with the starting SAMI (−.62 SD) and with population growth (−.44 SD) and rises with growth in % BA (+.32 SD). The 24-year change correlates .62 with the 2011–2024 change: real but noisy."

# 7. fact 3 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "3" "Density runs on two clocks"
Claim $s "Within a metro, nonprofits catch up with population growth in about five years. Between metros, density gaps left by decades of growth persist for a generation or more."
Stat $s 40 176 $w3 156 "~5 yrs" "Within-metro half-life of the gap after growth for filers (about 9 years for grassroots)"
Stat $s ( 40 + $w3 + 20 ) 176 $w3 156 "−25%" "Fastest-growing fifth of metros: below size-expected filer density in every year, 2000–2024"
Stat $s ( 40 + 2 * ( $w3 + 20 ) ) 176 $w3 156 "~35 yrs" "Half-life between metros once growth slows; still-fast growers closed none of their gap"
Design $s @( "Annual within-metro models estimate a short-run adjustment. Do not read them as long-run effects.",
             "For change questions, use differences of 10+ years or a geometric kernel of past population change (half-life 5–10 years).",
             "Stock-flow accounting says the Sunbelt gap is a lag, not a trait: control for past growth or the pre-1970 endowment." )
Num $s 7
Notes $s "Within metros, local projections give a half-life of about 5 years for filers and 9 for grassroots. Between metros, the fastest-growing fifth sits about 25% below size-expected density in every year, though new growth after 2000 was matched almost one-for-one. The 2000 gap tracks 1970–2000 growth. Stock-flow test (2012–19, one data source): still-growing fast growers add more entrants per resident than other metros, not fewer; growth alone accounts for the whole gap. Decline is asymmetric: organizations stay at first, then grassroots shed the excess with a half-life of about 4 years."

# 8. fact 3 figure ------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "3" "After growth, filers close the gap in about eight years"
Pic $s "f3_gap_by_direction.png" 40 86 880 340 "Density gap in the years after a metro grows 10%, holds steady or shrinks 10% in three years, for filers, grassroots and expenditures" | Out-Null
$tw = 280
Box $s 40 438 $tw 70 $TINT 5 "Note card" | Out-Null
T $s "Growth: filers close the gap by about year 8, grassroots by year 12" 56 448 ( $tw - 32 ) 52 14 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Box $s ( 40 + $tw + 20 ) 438 $tw 70 $TINT 5 "Note card" | Out-Null
T $s "Stable population: no lag at all" ( 56 + $tw + 20 ) 448 ( $tw - 32 ) 52 14 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Box $s ( 40 + 2 * ( $tw + 20 ) ) 438 $tw 70 $TINT 5 "Note card" | Out-Null
T $s "Decline: organizations stay at first; grassroots shed the excess by year 7" ( 56 + 2 * ( $tw + 20 ) ) 448 ( $tw - 32 ) 52 14 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Num $s 8
Notes $s "Within metro (metro and year fixed effects). Below 0 means fewer organizations than the new population implies. Panel B shows metro-years whose population barely moved, against the same model's no-change baseline. Relative income losses lower filer density by about 1.5–2% at once; gains do nothing."

# 9. fact 4 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "4" "Registered charities are two populations"
Claim $s "About a quarter file a 990 above the `$50k floor and behave like an economic sector. The other three quarters are grassroots organizations whose density follows demography and institutional legacy."
Stat $s 40 176 250 140 "72–80%" "Grassroots share of the registered stock; both groups scale at about 1.0"
Stat $s 40 332 250 140 ".65–.73" "Correlation of filer and grassroots density across metros"
$d4 = @( @( "", "Filers", "Grassroots" ),
         @( "Persistence, r at 20 years", ".94", ".78" ),
         @( "Within-metro half-life after growth", "~5 years", "~9 years" ),
         @( "Fastest-growing fifth, SAMI in 2024", "−.26", "−.16" ),
         @( "Deficit convergence per decade", "−.01", "−.23" ),
         @( "10% relative income loss, immediate", "about −1.5%", "about 0" ),
         @( "Nonprofit market share, within R²", ".35", ".14" ) )
Tbl $s $d4 310 176 @( 330, 140, 140 ) 14 | Out-Null
T $s "Filers follow the local economy (income, wages, market share); grassroots follow endowment and demography, and converge. Analyze them separately: pooling cancels opposite responses (D1)." 310 380 610 92 15 $INK $false $BODY 1 $false "Takeaway" | Out-Null
Num $s 9
Notes $s "In the 387 metros, the registered stock grew from 563,000 in 2000 to 1.19 million in 2024; filers grew from 158,000 to 288,000. For 2011–2023 change, filer gains follow income growth (partial r .35), private wage growth, % BA and turnout. Grassroots fall where wages and government earnings grow and rise where poverty rises. In the pooled registered stock the income effect is .03: the two groups respond in opposite directions and cancel. Organizations move between groups: after growth the filing share rises for 6–10 years."

# 10. fact 4 figure -----------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "4" "Grassroots drive the growth in registrations"
Pic $s "f4_composition.png" 40 86 880 368 "Registered public charities in 387 metros split into filers and grassroots, 2000 to 2024, and the share filing above the floor" | Out-Null
T $s "The registered stock doubled (563,000 → 1.19 million) while filers grew 82% (158,000 → 288,000). The share filing above the floor drifted from 28% to 24%, with dips at the source changes and a low of 20% in 2020." 40 462 880 48 15 $INK $false $BODY 1 $false "Takeaway" | Out-Null
Num $s 10
Notes $s "Treat filing status as a stage, not a type of organization: the floor moves organizations between the groups as local economies change. Use the registered stock as a splice-free robustness check. Check registration practices that distort grassroots counts: LDS congregations in Utah, group exemptions in Memphis and Mankato, military towns."

# 11. fact 5 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "5" "Market share sets the level, not the pace"
Claim $s "The 501(c)(3) share of private employment explains about a third of cross-metro filer density. It barely changes within metros and does not predict which metros catch up."
Stat $s 40 176 $w3 156 ".35" "R² of filer density on nonprofit employment share (spending .38, grassroots .14); 363 metros, 2013–2022"
Stat $s ( 40 + $w3 + 20 ) 176 $w3 156 "5.3 vs 0.4" "SD of share across metros vs. within a metro, in percentage points"
Stat $s ( 40 + 2 * ( $w3 + 20 ) ) 176 $w3 156 "61 vs 25" "Nonprofit jobs per 1,000 residents, Northeast vs. South; for-profit jobs are similar (308 vs. 294)"
Design $s @( "Share is partly the outcome (it includes hospitals and universities). Use it as a benchmark or bound, not an ordinary control (D8).",
             "Read regional effects in levels as institutional history: who owns hospitals and colleges, and how old the filer sector is.",
             "In change models, treat share as a fixed metro trait." )
Num $s 11
Notes $s "Share absorbs growth history: with share fixed, 1970–2000 growth has no effect on filer density. Share is nearly fixed (2013 and 2022 correlate .95) and share change does not predict catch-up. The regional gap is in the numerator: the South and West have well under half the Northeast's nonprofit jobs per resident with similar for-profit employment. Older sectors hold larger shares: Northeast and Midwest filers are about 3.5 years older, and Northeast filers last longest."

# 12. fact 5 figure -----------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "5" "Low share, low density: the Sunbelt sits low on both"
Pic $s "f5_share_level.png" 40 86 880 370 "Filer, grassroots and spending density percentile against nonprofit employment share percentile, 2013 to 2022, colored by region" | Out-Null
T $s "The relation is loose: at a 10% share, metros run from the 5th to the 95th percentile of filer density. Holding share fixed, a higher nonprofit/for-profit wage ratio goes with fewer, larger organizations." 40 462 880 48 15 $INK $false $BODY 1 $false "Takeaway" | Out-Null
Num $s 12
Notes $s "BLS QCEW nonprofit data, 363 metros, 2013–2022 pooled with year fixed effects. Residuals point to institutions: filers fall furthest below in military towns and border metros; grassroots fall furthest below in Utah; college and amenity towns sit above."

# 13. fact 6 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "6" "Size is the main source of collinearity"
Claim $s "Metro size is one latent factor behind most density covariates. The collinearity in density models is mostly every size-scaled covariate moving with population, not overlap between theory blocks."
Stat $s 40 176 $w3 156 ".89–.97" "Correlation of logged size-scaled covariates (government earnings, contributions, foundations) with log population"
Stat $s ( 40 + $w3 + 20 ) 176 $w3 156 "12%" "Share of the government-earnings effect a SAMI outcome keeps when the covariate is not size-adjusted (= 1 − R²)"
Stat $s ( 40 + 2 * ( $w3 + 20 ) ) 176 $w3 156 "13.0 vs 2.2" "VIF of government earnings in the log-log model vs. the largest VIF in the identical all-SAMI model"
Design $s @( "Handle size once: residualize size-scaled covariates on log population, or keep log population on the right. The two are equivalent.",
             "Never pair a size-adjusted outcome with raw or logged size-scaled covariates (D6).",
             "Diagnose collinearity in SAMI form; high VIFs in the log-log model reflect size, not instability." )
Num $s 13
Notes $s "After residualizing on log population, most cross-block pairs fall below |r| = .1; government employment vs. congregations goes from .89 to −.01. A SAMI outcome with an unadjusted covariate shrinks the covariate's effect by exactly 1 − R² of that covariate on log P. The two correct specifications give identical coefficients (Frisch–Waugh–Lovell)."

# 14. fact 6 table ------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "6" "Mishandling size can flip a sign"
$d6 = @( @( "Covariate (partial r, 2022)", "log N on log P + covariates (= all-SAMI)", "SAMI on logged covariates, no size control", "log N on logged covariates, no log P" ),
         @( "S&L govt earnings", ".33", "−.20", ".83" ),
         @( "Foundation assets", ".41", ".26", ".52" ),
         @( "% BA", ".24", ".53", "−.06" ),
         @( "Poverty rate", "−.10", ".02", "−.20" ),
         @( "% nonwhite", "−.26", "−.23", "−.23" ),
         @( "Turnout", ".09", ".06", ".10" ) )
Tbl $s $d6 40 100 @( 240, 213, 213, 214 ) 15 @( @( 2, 3 ), @( 2, 4 ), @( 4, 4 ) ) | Out-Null
Box $s 40 360 430 84 $TINT 5 "Note card" | Out-Null
T $s "Without log population, government earnings becomes a proxy for size (.83) and % BA disappears (−.06)." 60 370 390 64 16 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Box $s 490 360 430 84 $TINT 5 "Note card" | Out-Null
T $s "With a SAMI outcome but unadjusted covariates, government earnings turns negative (−.20)." 510 370 390 64 16 $INK $false $BODY 1 $false "Note" 3 | Out-Null
T $s "Correct specifications agree exactly (Frisch–Waugh–Lovell): the problem is not collinearity among theory blocks but size left in the covariates." 40 462 880 40 15 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s 14
Notes $s "The default six-covariate model of the earlier outline, 387 metros, 2022. The first column is the correct specification; the other two leave size in the covariates or out of the model. Real collinearity appears only when several measures of one construct enter together: all 30 candidate items give a VIF of 78."

# 15. fact 7 ------------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "7" "Data hazards are frequent and patterned"
Claim $s "Three source changes, one tax law and two registration practices leave marks large enough to pass for findings. They are predictable: each hits specific years, measures or metros."
$d7 = @( @( "Year-over-year change, 387-metro totals", "2011 (CORE → SOI)", "2018 (tax law)", "2020 (SOI → e-file)", "2021" ),
         @( "Filers, `$50k real floor", "+13.3%", "+1.5%", "−8.8%", "+12.9%" ),
         @( "Filers, no floor", "+25.9%", "+1.3%", "−7.7%", "+10.3%" ),
         @( "Registered stock (BMF)", "−6.0%", "+3.3%", "+3.2%", "+0.7%" ),
         @( "Spending excl. hospitals, universities", "+6.4%", "+4.7%", "+13.1%", "+6.0%" ),
         @( "Itemized contributions", "+2.2%", "−21.7%", "+5.0%", "+30.4%" ) )
Tbl $s $d7 40 172 @( 300, 145, 145, 145, 145 ) 14 @( @( 2, 2 ), @( 3, 2 ), @( 2, 4 ), @( 6, 3 ) ) | Out-Null
Bul $s @( "Splices move spending rankings (year-to-year r .87 in 2011), not count rankings (.985 or above every year).",
          "Turnout is observed in 28% of metro-years, % BA and the Gini in 64%: panel covariates are mostly interpolated.",
          "Group exemptions (Memphis, Mankato), military towns and unregistered LDS congregations distort the registered stock." ) 40 352 880 150 15 $INK "Takeaways" | Out-Null
Num $s 15
Notes $s "The 2011 filer jump is the switch from NCCS CORE to the IRS SOI extracts; the registered stock fell the same year because of the automatic revocations. Tax year 2020 counts are about 9% low in both SOI and e-file. Itemized contributions fall 22% at the 2018 tax law: the share of returns itemizing fell from 31% to 11.5% nationally. The $50k real floor excludes about a quarter of filers but barely changes which metros have more filers (SAMI r = .987)."

# 16. fact 7 figure -----------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "7" "Most hazards hit specific years, measures or metros"
Pic $s "f7_hazards.png" 40 84 500 440 "Data hazards: metro totals indexed to 2010 with source changes, year-to-year SAMI correlations, observed share of each covariate, registered-stock SAMI against filer SAMI" | Out-Null
Bul $s @( "Use year fixed effects in every panel; check any change across 2011 or 2020 against counts.",
          "Treat tax year 2020 counts as low; drop or flag them in change and lag models.",
          "Do not let contribution models span 2018: the law adds about .05 to contribution terms in count cross-sections.",
          "Exclude covariates built from nonprofit counts (Rupasingha index, association counts).",
          "Floor size-scaled totals before logging: Eagle Pass, TX sat 11 SD below the mean under log(1 + x)." ) 568 104 352 400 15 $INK "Takeaways" | Out-Null
Num $s 16
Notes $s "Panels: (A) 387-metro totals indexed to 2010 with source changes marked; (B) year-to-year SAMI correlations; (C) share of metro-years observed for each covariate; (D) registered-stock SAMI against filer SAMI in 2022, with outliers labeled."

# ---- part 3: the multiverse ----------------------------------------------------------------------------
$s = Divider $pres "3" "The multiverse" "Fit every defensible combination of design choices, and ask which choices move the answer"
$secMv = $s.SlideIndex
Notes $s "Most disagreement in nonprofit density research comes from a few design decisions, not from the data."

# mv: why
$s = NewSlide $pres
T $s "One published estimate is one path through many" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
T $s "Every result rests on a chain of choices: which organizations to count, how to scale by population, which year, which estimator, which measure of each construct. Each is defensible, and the chain branches at every step." 40 92 520 110 18 $INK $false $HEAD 1 $true "Claim" | Out-Null
Bul $s @( "Garden of forking paths: readers cannot tell whether a neighboring path gives the same answer (Gelman and Loken).",
          "Report how estimates move across reasonable specifications (Leamer); fit them all (multiverse, Steegen et al.; specification curves, Simonsohn et al.).",
          "Attribute the spread to the choices that produce it (Young and Holsteen)." ) 40 216 520 230 15 $INK "Literature" | Out-Null
Box $s 590 92 330 160 $TINT 5 "Disagreement card" | Out-Null
T $s "Published signs disagree" 610 108 290 22 17 $DARK $true $HEAD 1 $false "Card title" | Out-Null
T $s "Racial diversity is positive in some density studies and negative in others. Poverty and income flip sign between pooled and fixed-effects models of the same metros." 610 136 290 106 14 $INK $false $BODY 1 $false "Card text" | Out-Null
Box $s 590 268 330 180 $DARK 5 "Question card" | Out-Null
T $s "Are the disagreements about the world, or about the paths taken?" 610 284 290 148 22 "FFFFFF" $true $HEAD 1 $true "Question" 3 | Out-Null
Num $s
Notes $s "The covariates in density research are standard (education, poverty, diversity, philanthropy, government, civic culture), but published signs disagree. A multiverse makes the choices visible: it fits every defensible combination and asks which choices move the answer and by how much."

# mv: three kinds of decisions
$s = NewSlide $pres
T $s "Three kinds of decisions, three treatments" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$kinds = @(
  @( "1", "Estimand choice", "The options answer different questions.", "Separate strata, never pooled; argued from theory.", "Outcome population, identification, unit of analysis" ),
  @( "2", "Analyst discretion", "Defensible alternatives for the same question.", "Crossed as forks; report robustness across them.", "Estimator, year, winsorizing, measure of a construct" ),
  @( "3", "Avoidable error", "One option is wrong for a known reason.", "Kept to show its cost, then excluded from recommendations.", "Per capita when β ≠ 1, raw-dollar predictors, OLS with many zeros" ) )
$kw = 280
for ( $k = 0; $k -lt 3; $k++ ) {
  $l = 40 + $k * ( $kw + 20 ); $q = $kinds[ $k ]
  Box $s $l 92 $kw 400 $TINT 5 "Kind card" | Out-Null
  Badge $s $q[ 0 ] ( $l + 18 ) 110 38 | Out-Null
  T $s $q[ 1 ] ( $l + 18 ) 160 ( $kw - 36 ) 28 20 $DARK $true $HEAD 1 $false "Kind" | Out-Null
  T $s $q[ 2 ] ( $l + 18 ) 196 ( $kw - 36 ) 48 15 $INK $false $BODY 1 $true "Meaning" | Out-Null
  T $s "How it is handled" ( $l + 18 ) 258 ( $kw - 36 ) 18 13 "B4471A" $true $BODY 1 $false "Label" | Out-Null
  T $s $q[ 3 ] ( $l + 18 ) 280 ( $kw - 36 ) 66 15 $INK $false $BODY 1 $false "Handling" | Out-Null
  T $s "Examples" ( $l + 18 ) 362 ( $kw - 36 ) 18 13 "B4471A" $true $BODY 1 $false "Label" | Out-Null
  T $s $q[ 4 ] ( $l + 18 ) 384 ( $kw - 36 ) 90 15 $INK $false $BODY 1 $false "Examples" | Out-Null
}
Num $s
Notes $s "Not every choice belongs in one distribution (Del Giudice and Gangestad). Pooling choices that change the estimand with choices that are interchangeable produces a spread with no interpretation. Every decision is tagged with one of three types, because the right response differs."

# mv: the grid
$s = NewSlide $pres
T $s "432,432 specifications from four layers of choices" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$steps = @( @( "Data", "387 metros and 925 CBSAs, 2000–2024" ), @( "12 strata", "counts or spending × hospitals and universities in or out × identification" ),
            @( "8 forks", "crossed inside each stratum" ), @( "24 sets", "of covariates: default · drop 1 of 9 blocks · swap 1 of 14 measures" ),
            @( "432,432", "specifications after removing impossible pairs and duplicates" ) )
$bw = 160; $gap = 20
for ( $k = 0; $k -lt 5; $k++ ) {
  $l = 40 + $k * ( $bw + $gap ); $fill = $( if ( $k -eq 4 ) { $DARK } else { $TINT } )
  Box $s $l 88 $bw 116 $fill 5 "Grid step" | Out-Null
  T $s $steps[ $k ][ 0 ] ( $l + 12 ) 98 ( $bw - 24 ) 30 $( if ( $k -eq 4 ) { 24 } else { 18 } ) $( if ( $k -eq 4 ) { "FFFFFF" } else { $BLUE } ) $true $HEAD 1 $false "Step title" | Out-Null
  T $s $steps[ $k ][ 1 ] ( $l + 12 ) 134 ( $bw - 24 ) 64 13 $( if ( $k -eq 4 ) { $LIGHT } else { $INK } ) $false $BODY 1 $false "Step text" | Out-Null
  if ( $k -lt 4 ) { Arrow $s ( $l + $bw + 3 ) 146 ( $l + $bw + $gap - 3 ) 146 }
}
$dg = @( @( "Fork (crossed within each stratum)", "Options" ),
         @( "Outcome measure", "Counts: filers, registered stock. Spending: expenses or revenue, gross or net of grants" ),
         @( "DV form", "Per capita · log per capita · log-log · SAMI · Poisson with offset · Poisson, log P free" ),
         @( "Predictor form (size-scaled totals)", "Raw · per capita · logged · SAMI form" ),
         @( "log P on the right", "No · yes" ),
         @( "Unit", "Metro · metro + micro" ),
         @( "Estimator", "OLS · state FE · Census division FE · state random effects" ),
         @( "Year (cross-section only)", "2022 · 2019" ),
         @( "Winsorizing", "None · top and bottom 1%" ) )
Tbl $s $dg 40 222 @( 270, 610 ) 13 | Out-Null
T $s "Grassroots enter as a refit of all 39,312 registered-stock specifications with registered non-filers as the outcome." 40 476 880 20 13 $MUTED $false $BODY 1 $true "Footnote" | Out-Null
Num $s
Notes $s "Identification: a single-year cross-section, a pooled panel 2015–2022 with year fixed effects, or a within-metro panel 2010–2022 with metro and year fixed effects. Covariate sets change one thing at a time from the 13-covariate default, so every specification differs from a default twin in a single, named way. 509,344 combinations remain after impossible pairs are removed; algebraic duplicates (log(Y/P) with log P on the right equals log Y with log P on the right; residualized predictors alongside log P equal logged predictors) are fitted only as a check, leaving 432,432 (run v11)."

# mv: partial r
$s = NewSlide $pres
T $s "One metric across every specification: partial r" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
Box $s 40 92 300 92 $TINT 5 "Formula card" | Out-Null
T $s "r = t / √(t² + df)" 56 104 268 68 30 $DARK $true $HEAD 2 $false "Formula" 3 | Out-Null
Bul $s @( "Scale-free across per capita, log and Poisson models.",
          "Identical across algebraically equivalent specifications.",
          "t from robust or clustered SEs; df counted in metros.",
          "|r| ≈ .1 is small, ≈ .3 moderate." ) 40 202 300 260 15 $INK "Properties" | Out-Null
Pic $s "mv_partial_r_explainer.png" 360 92 560 300 "What partial r measures: filer-count SAMI against % BA, bivariate (r = .59) and net of the other covariates (partial r = .26)" | Out-Null
T $s "% BA and filer density: r = .59 on its own, partial r = .26 once the other covariates and log population are held fixed. More than half of the variance in % BA is shared with the other covariates." 360 404 560 70 14 $INK $false $BODY 1 $false "Takeaway" | Out-Null
Num $s
Notes $s "Coefficients cannot be compared across the grid: a per capita model, a log model and a Poisson model put the same covariate on different scales. Partial r is a direct transformation of the t-statistic. Panel B removes from both % BA and the SAMI what the other covariates and log population predict; the slope of the residuals equals the full-model coefficient (Frisch–Waugh–Lovell)."

# mv: specification curve
$s = NewSlide $pres
T $s "Reading one covariate across the grid" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
Pic $s "mv_spec_curve_nonwhite.png" 40 82 470 440 "Specification curve for % nonwhite across 55,912 specifications in the counts, cross-section stratum, with strips showing where each fork's options fall" | Out-Null
Bul $s @( "% nonwhite runs from about −.5 to +.25 across 55,912 specifications of one stratum.",
          "Median −.02; 61% share its sign; 31% negative and significant, 8% positive and significant.",
          "The strips show where each option falls: filers at the negative end, registrations at the positive end.",
          "Every covariate is summarized by its median, sign agreement and share significant (robust and state-clustered SEs)." ) 540 100 380 360 16 $INK "Takeaways" | Out-Null
T $s "Illustration from run v10 (six-covariate default; filers and registrations together)." 540 472 380 32 12 $FAINT $false $BODY 1 $true "Source" | Out-Null
Num $s
Notes $s "Read each strip left to right: dark means that option makes up most of the specifications in that part of the curve. Outcome and unit pull hardest; OLS leans negative and state random effects positive; DV form, predictor form, year and winsorizing spread evenly along the curve. This pull from the outcome is why the findings report filers and grassroots separately."

# mv: eta squared
$s = NewSlide $pres
T $s "Which decision moves a covariate: η²" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
T $s "If you knew only which option of one decision a specification used, how much of the covariate's spread could you predict?" 40 84 880 44 18 $INK $false $HEAD 1 $true "Question" | Out-Null
Pic $s "mv_eta2_explainer.png" 40 134 880 300 "Partial r of % nonwhite in 964 default-model specifications grouped by estimator (eta squared .15) and by winsorizing (eta squared .01)" | Out-Null
$hw = 430
Box $s 40 444 $hw 60 $TINT 5 "Note card" | Out-Null
T $s "By estimator: OLS −.15, state FE −.06. The option means explain 15% of the spread (η² = .15)." 56 450 ( $hw - 32 ) 48 14 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Box $s ( 40 + $hw + 20 ) 444 $hw 60 $TINT 5 "Note card" | Out-Null
T $s "By winsorizing: both means sit on the overall mean (η² = .01). η² ≥ .06 counts as a decision that matters." ( 56 + $hw + 20 ) 450 ( $hw - 32 ) 48 14 $INK $false $BODY 1 $false "Note" 3 | Out-Null
Num $s
Notes $s "η² is the spread between option means over the total spread, computed within each stratum and outcome, then averaged. With 10^4–10^5 specifications every F test is significant, so η² rather than a p-value is the screen. Because some options only occur together, cells overlap and do not add up. It is a marginal measure in the spirit of Young and Holsteen's model influence: how much a decision matters, not why."

# mv: which decisions change verdicts (bars drawn as shapes)
$s = NewSlide $pres
T $s "Four decisions change conclusions; the usual robustness checks do not" 40 28 880 44 26 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$bars = @( @( "Measure (own swap)", .44, .44, .27 ), @( "Outcome population", .41, .41, .37 ), @( "Identification", .38, .42, .29 ),
           @( "Predictor form", .32, .22, .26 ), @( "Unit", .26, .20, .22 ), @( "DV form", .23, .16, .27 ), @( "Estimator", .19, .14, .20 ),
           @( "log P on the right", .17, .13, .14 ), @( "Endowment in or out", .13, .11, .10 ), @( "Winsorizing", .13, .12, .14 ),
           @( "Growth kernel in or out", .10, .11, .10 ), @( "Year (2022 vs. 2019)", .09, .13, .14 ), @( "Hospitals and universities", .02, -1, .23 ) )
$BX = 230; $SC = 900; $Y0 = 104; $RH = 30
$cols3 = @( $BLUE, "1BAF7A", "8A94A3" )
for ( $k = 0; $k -lt $bars.Count; $k++ ) {
  $b = $bars[ $k ]; $y = $Y0 + $k * $RH
  T $s $b[ 0 ] 40 ( $y + 4 ) 180 20 13 $INK $( $k -lt 4 ) $BODY 3 $false "Decision label" 3 | Out-Null
  for ( $j = 1; $j -le 3; $j++ ) {
    if ( $b[ $j ] -lt 0 ) { continue }
    $w = [math]::Max( 2, $b[ $j ] * $SC ); $by = $y + 2 + ( $j - 1 ) * 9
    Box $s $BX $by $w 7 $cols3[ $j - 1 ] 1 "Bar" | Out-Null
    T $s ( "{0:0}%" -f ( 100 * $b[ $j ] ) ) ( $BX + $w + 4 ) ( $by - 3 ) 34 12 10 $MUTED $false $BODY 1 $false "Bar value" | Out-Null
  }
}
$lg = @( @( "Filers", $BLUE ), @( "Grassroots", "1BAF7A" ), @( "Expenditures", "8A94A3" ) )
for ( $j = 0; $j -lt 3; $j++ ) { Box $s ( 690 + $j * 80 ) 84 12 12 $lg[ $j ][ 1 ] 1 "Legend key" | Out-Null; T $s $lg[ $j ][ 0 ] ( 706 + $j * 80 ) 82 70 16 12 $INK $false $BODY 1 $false "Legend" | Out-Null }
Box $s 690 300 230 180 $WARM 5 "Design box" | Out-Null
T $s "Share of matched specification pairs whose verdict changes (significantly positive, not significant, significantly negative) when only that decision changes, averaged over the 13 covariates." 706 312 198 160 13 $INK $false $BODY 1 $false "Reading" | Out-Null
Num $s
Notes $s "Run v11 with grassroots refit. Measurement, outcome population and identification each change about four in ten verdicts for filers and grassroots; the checks papers usually report (year, winsorizing, log P on the right) change about one in ten. The hospital and university exclusion matters only for spending. The estimand choices should be argued from theory, never averaged."

# mv: decision cards
$s = NewSlide $pres
T $s "Every decision gets a card, read on one yardstick" 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
$parts = @( @( "Decision", "The choice an analyst faces, in one line" ), @( "Options", "The alternatives in published work, plus ours" ),
            @( "Mechanism", "Why the choice changes results: the algebra or the data" ), @( "Evidence", "How the covariates move under each option" ),
            @( "Recommendation", "What to do, and what to report otherwise" ) )
$pw = 160
for ( $k = 0; $k -lt 5; $k++ ) {
  $l = 40 + $k * ( $pw + 20 )
  Box $s $l 96 $pw 150 $TINT 5 "Card part" | Out-Null
  Badge $s "$( $k + 1 )" ( $l + 14 ) 110 30 | Out-Null
  T $s $parts[ $k ][ 0 ] ( $l + 14 ) 150 ( $pw - 28 ) 24 16 $DARK $true $HEAD 1 $false "Part" | Out-Null
  T $s $parts[ $k ][ 1 ] ( $l + 14 ) 178 ( $pw - 28 ) 62 13 $INK $false $BODY 1 $false "Part text" | Out-Null
  if ( $k -lt 4 ) { Arrow $s ( $l + $pw + 3 ) 171 ( $l + $pw + 17 ) 171 }
}
Box $s 40 268 430 230 $DARK 5 "Yardstick card" | Out-Null
T $s "The yardstick" 60 284 390 24 18 "FFFFFF" $true $HEAD 1 $false "Card title" | Out-Null
T $s "Five benchmark covariates fixed before any card was written: % BA, foundation assets, % nonwhite, turnout and poverty. Each stands for a theory the field tests and a different way results go wrong (size scaling, measurement, estimator, collinearity)." 60 316 390 170 15 $LIGHT $false $BODY 1 $false "Card text" | Out-Null
Box $s 490 268 430 230 $TINT 5 "Report card" | Out-Null
T $s "What each card reports" 510 284 390 24 18 $DARK $true $HEAD 1 $false "Card title" | Out-Null
Bul $s @( "Median partial r under each option",
          "Sign agreement and the share significant (robust and state-clustered SEs)",
          "The decision's η², for filers, grassroots and expenditures",
          "Sensitivity notes for checks that are not forks (tax law, city size, sample exclusions)" ) 510 318 390 170 15 $INK "Card bullets" | Out-Null
Num $s
Notes $s "The design decisions D1–D12 are written in this format so the paper can be scanned as a reference. Reading every decision on the same covariates lets readers compare the cost of decisions directly."

# 17. closing -----------------------------------------------------------------------------------------
$s = NewSlide $pres $DARK
$secClose = $s.SlideIndex
T $s "What any density design has to do" 60 40 840 50 34 "FFFFFF" $true $HEAD 1 $false "Title" 3 | Out-Null
$imp = @(
  @( "1", "Handle size once", "SAMI-form covariates, or log P on the right (Facts 1, 6)" ),
  @( "2", "Separate the populations", "Filers, grassroots and spending answer different questions (Facts 1, 4)" ),
  @( "3", "Read within-metro estimates as short-run", "Little variance, mostly interpolated, half-closed in ~5 years (Facts 2, 3)" ),
  @( "4", "Control for history in levels", "Growth kernel and pre-1970 endowment (Fact 3)" ),
  @( "5", "Treat market share as a bound", "It is partly the outcome (Fact 5)" ),
  @( "6", "Flag the data hazards", "Splice years, the 2018 tax law, interpolated covariates (Fact 7)" ) )
for ( $k = 0; $k -lt 6; $k++ ) {
  $col = $k % 2; $row = [math]::Floor( $k / 2 ); $l = 60 + $col * 430; $t = 124 + $row * 112
  Badge $s $imp[ $k ][ 0 ] $l $t 38 | Out-Null
  T $s $imp[ $k ][ 1 ] ( $l + 54 ) ( $t + 2 ) 350 24 18 "FFFFFF" $true $HEAD 1 $false "Point" | Out-Null
  T $s $imp[ $k ][ 2 ] ( $l + 54 ) ( $t + 30 ) 350 46 14 $LIGHT $false $BODY 1 $false "Point text" | Out-Null
}
T $s "The design decisions D1–D12 take each of these up in turn." 60 470 840 24 15 $LIGHT $false $BODY 1 $true "Footer" | Out-Null
Notes $s "These six rules follow directly from the facts and set up the decision cards. The multiverse then shows how much each decision moves the covariate results."

# ---- sections, save and export -------------------------------------------------------------------
$sp = $pres.SectionProperties
$sp.AddBeforeSlide( 1, "Title" ) | Out-Null
$sp.AddBeforeSlide( $secData, "Data" ) | Out-Null
$sp.AddBeforeSlide( $secFacts, "Stylized facts" ) | Out-Null
$sp.AddBeforeSlide( $secMv, "Multiverse" ) | Out-Null
$sp.AddBeforeSlide( $secClose, "Implications" ) | Out-Null
if ( Test-Path $out ) { Remove-Item $out -Force }
$pres.SaveAs( $out, 24 )
if ( $Export ) {
  New-Item -ItemType Directory -Force $Export | Out-Null
  for ( $i = 1; $i -le $pres.Slides.Count; $i++ ) { $pres.Slides.Item( $i ).Export( ( Join-Path $Export ( "slide-{0:D2}.png" -f $i ) ), "PNG", 1600, 900 ) }
}
$pres.Close(); $app.Quit()
[System.Runtime.InteropServices.Marshal]::ReleaseComObject( $app ) | Out-Null
"Saved $out ($( (Get-Item $out).Length ) bytes)"
