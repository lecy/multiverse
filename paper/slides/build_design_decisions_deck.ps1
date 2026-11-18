# build_design_decisions_deck.ps1
# Draft deck for paper/04-design-decisions.qmd, built by driving PowerPoint (COM).
# Same design system as build_stylized_facts_deck.ps1 (palette, badges, claim, stat cards, design box, tables).
# Usage (Windows PowerShell, from multiverse/paper/slides):  powershell -File build_design_decisions_deck.ps1 [-Export <png folder>]
# Output: design-decisions.pptx next to this script. Charts come from ./figs (make_deck_figures.R).
param( [string]$Export = "" )
$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$fig  = Join-Path $here "figs"
$out  = Join-Path $here "design-decisions.pptx"

# ---- palette (shared with the stylized-facts deck) -------------------------------------------
$DARK = "1D2939"; $INK = "1F2933"; $MUTED = "5F6B7A"; $FAINT = "8A94A3"
$ORANGE = "EB6834"; $BLUE = "2A78D6"; $TINT = "F1F4F8"; $WARM = "FDF0EA"; $LIGHT = "C9D3DF"; $RUST = "C2410C"
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
# Badge: as in the stylized-facts deck; the label shrinks for card numbers (D1 to D12).
function Badge( $s, [string]$n, $l = 40, $t = 30, $d = 40, [string]$fill = $ORANGE ) {
  $o = $s.Shapes.AddShape( 9, $l, $t, $d, $d ); $o.Fill.ForeColor.RGB = ( C $fill ); $o.Line.Visible = 0; $o.Name = "Card badge $n"
  $tf = $o.TextFrame; $tf.MarginLeft = 0; $tf.MarginRight = 0; $tf.MarginTop = 0; $tf.MarginBottom = 0; $tf.VerticalAnchor = 3; $tf.WordWrap = 0
  $k = $( if ( $n.Length -ge 3 ) { 0.34 } elseif ( $n.Length -eq 2 ) { 0.42 } else { 0.5 } )
  $tr = $tf.TextRange; $tr.Text = $n; $tr.Font.Name = $HEAD; $tr.Font.Size = [int]( $d * $k ); $tr.Font.Bold = -1
  $tr.Font.Color.RGB = ( C "FFFFFF" ); $tr.ParagraphFormat.Alignment = 2
  $o
}
function Head( $s, [string]$n, [string]$title ) {
  Badge $s $n | Out-Null
  T $s $title 94 28 826 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null
}
function Title( $s, [string]$title ) { T $s $title 40 28 880 44 28 $DARK $true $HEAD 1 $false "Title" 3 | Out-Null }
function Num( $s, $i = 0 ) { T $s "$( $s.SlideIndex )" 880 512 40 14 10 $FAINT $false $BODY 3 $false "Slide number" | Out-Null }
function Divider( $pres, [string]$part, [string]$title, [string]$sub ) {
  $s = NewSlide $pres $DARK
  Badge $s $part 60 140 64 | Out-Null
  T $s $title 60 228 840 64 40 "FFFFFF" $true $HEAD 1 $false "Title" 3 | Out-Null
  T $s $sub 60 300 840 60 19 $LIGHT $false $HEAD 1 $true "Subtitle" | Out-Null
  $s
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
  $sz = $( if ( $num.Length -gt 10 ) { 30 } elseif ( $num.Length -gt 8 ) { 34 } else { 38 } )
  T $s $num ( $l + 18 ) ( $t + 16 ) ( $w - 36 ) 50 $sz $BLUE $true $HEAD 1 $false "Stat number" 3 | Out-Null
  T $s $label ( $l + 18 ) ( $t + 72 ) ( $w - 36 ) ( $h - 84 ) 14 $INK $false $BODY 1 $false "Stat label" | Out-Null
}
function Stats3( $s, $a, $b, $c, $t = 176 ) {
  $w3 = 280
  Stat $s 40 $t $w3 156 $a[ 0 ] $a[ 1 ]
  Stat $s ( 40 + $w3 + 20 ) $t $w3 156 $b[ 0 ] $b[ 1 ]
  Stat $s ( 40 + 2 * ( $w3 + 20 ) ) $t $w3 156 $c[ 0 ] $c[ 1 ]
}
function Design( $s, [string[]]$items, $l = 40, $t = 352, $w = 880, $h = 146, [string]$header = "Recommendation" ) {
  Box $s $l $t $w $h $WARM 5 "Design box" | Out-Null
  T $s $header ( $l + 20 ) ( $t + 14 ) ( $w - 40 ) 20 15 "B4471A" $true $BODY 1 $false "Design header" | Out-Null
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
  foreach ( $hc in $hilite ) { $r = $tb.Cell( $hc[ 0 ], $hc[ 1 ] ).Shape.TextFrame.TextRange; $r.Font.Color.RGB = ( C $RUST ); $r.Font.Bold = -1 }
  $sh
}
function NoteCards( $s, [string]$a, [string]$b, $t = 360, $h = 96 ) {
  Box $s 40 $t 430 $h $TINT 5 "Note card" | Out-Null
  T $s $a 60 ( $t + 12 ) 390 ( $h - 24 ) 15 $INK $false $BODY 1 $false "Note" 3 | Out-Null
  Box $s 490 $t 430 $h $TINT 5 "Note card" | Out-Null
  T $s $b 510 ( $t + 12 ) 390 ( $h - 24 ) 15 $INK $false $BODY 1 $false "Note" 3 | Out-Null
}
function Source( $s, [string]$text, $l = 628, $t = 486, $w = 292 ) { T $s $text $l $t $w 18 11 $FAINT $false $BODY 1 $false "Source" | Out-Null }
function Notes( $s, [string]$txt ) { $s.NotesPage.Shapes.Placeholders.Item( 2 ).TextFrame.TextRange.Text = $txt }
function NewSlide( $pres, [string]$bg = "FFFFFF" ) {
  $s = $pres.Slides.Add( $pres.Slides.Count + 1, 12 )
  $s.FollowMasterBackground = 0; $s.Background.Fill.Solid(); $s.Background.Fill.ForeColor.RGB = ( C $bg )
  $s
}
# Figure slide: chart on the left, takeaways on the right (as in the stylized-facts figure slides).
function FigSlide( $pres, [string]$n, [string]$title, [string]$file, [string]$alt, [string[]]$items, [string]$src, $i ) {
  $s = NewSlide $pres
  Head $s $n $title
  Pic $s $file 40 84 540 430 $alt | Out-Null
  Bul $s $items 612 110 308 360 15 $INK "Takeaways" | Out-Null
  if ( $src ) { Source $s $src 612 486 308 }
  Num $s $i
  $s
}

# ---- presentation -------------------------------------------------------------------------------
$app = New-Object -ComObject PowerPoint.Application
$pres = $app.Presentations.Add( 0 )
$pres.PageSetup.SlideWidth = 960; $pres.PageSetup.SlideHeight = 540
$pres.SlideMaster.Theme.ThemeFontScheme.MajorFont.Item( 1 ).Name = $HEAD
$pres.SlideMaster.Theme.ThemeFontScheme.MinorFont.Item( 1 ).Name = $BODY
$i = 0

# 1. title ------------------------------------------------------------------------------------------
$s = NewSlide $pres $DARK; $i++
T $s "Design decisions in nonprofit density research" 60 70 840 140 44 "FFFFFF" $true $HEAD 1 $false "Title" 4 | Out-Null
T $s "Twelve decision cards, read on the 13-covariate model" 60 232 840 34 22 $LIGHT $false $HEAD 1 $true "Subtitle" | Out-Null
T $s "Jesse Lecy and Jeremy P. Thornton" 60 302 840 24 16 "FFFFFF" $false $BODY 1 $false "Authors" | Out-Null
T $s "Multiverse run v11: 432,432 specifications, 387 US metros, 2010–2022" 60 328 840 22 14 $LIGHT $false $BODY 1 $false "Data" | Out-Null
for ( $k = 1; $k -le 12; $k++ ) { Badge $s "D$k" ( 60 + ( $k - 1 ) * 48 ) 430 38 | Out-Null }
Notes $s "Twelve design decisions, ordered from the choices that define the question to the choices that only refine it. Every card is read on the 13-covariate model: poverty and % 65+ (need); the metro Gini, % nonwhite, religious fractionalization and age SD (heterogeneity); foundation assets, % BA and turnout (supply); the 10-year growth kernel, state and local government earnings, Medicaid and the pre-1970 endowment (market niche). Single models refit each option on 385 metros; run v11 of the multiverse supplies the distributions."

# 2. overview ---------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Title $s "Twelve decisions, from defining the question to refining it"
$EST = "Estimand choice"; $ERR = "Avoidable error"; $DIS = "Analyst discretion"
$cards = @(
  @( "D1", "Outcome population", $EST, "Filers and grassroots give different signs" ),
  @( "D2", "Unit of analysis", $EST, "Poverty reverses only within metros" ),
  @( "D3", "Identification", $EST, "Within-metro estimates shrink or flip" ),
  @( "D4", "DV form", $ERR, "Per capita forces the size coefficient to 1" ),
  @( "D5", "Estimator with zeros", $ERR, "OLS bias tracks the share of zeros" ),
  @( "D6", "Predictor form", $ERR, "Enter size-scaled totals in SAMI form" ),
  @( "D7", "One item or an index", $DIS, "Indexes cancel opposite-signed items" ),
  @( "D8", "Historical controls", "Estimand; bad-control risk", "Endowment and growth kernel are one fact" ),
  @( "D9", "Geography FE and SEs", "Discretion that flips signs", "State-patterned terms move most" ),
  @( "D10", "Money measure", $DIS, "Exclude hospitals and universities" ),
  @( "D11", "Subsector scope", $EST, "The full sector averages different rules" ),
  @( "D12", "Sector age", "Bad-control risk", "Partly the outcome in change models" ) )
$cw = 208; $ch = 128
for ( $k = 0; $k -lt 12; $k++ ) {
  $c = $cards[ $k ]; $col = $k % 4; $row = [math]::Floor( $k / 4 ); $l = 40 + $col * ( $cw + 16 ); $t = 90 + $row * ( $ch + 12 )
  $tc = $( if ( $c[ 2 ] -like "Estimand*" ) { $BLUE } elseif ( $c[ 2 ] -like "*error" -or $c[ 2 ] -like "Bad*" ) { $RUST } else { $MUTED } )
  Box $s $l $t $cw $ch $TINT 5 "Card $( $c[0] )" | Out-Null
  Badge $s $c[ 0 ] ( $l + 14 ) ( $t + 14 ) 34 | Out-Null
  T $s $c[ 1 ] ( $l + 58 ) ( $t + 12 ) ( $cw - 70 ) 40 15 $DARK $true $HEAD 1 $false "Card title" 3 | Out-Null
  T $s $c[ 2 ] ( $l + 14 ) ( $t + 60 ) ( $cw - 28 ) 16 11 $tc $true $BODY 1 $false "Card type" | Out-Null
  T $s $c[ 3 ] ( $l + 14 ) ( $t + 80 ) ( $cw - 28 ) 40 13 $MUTED $false $BODY 1 $false "Card text" | Out-Null
}
Num $s $i
Notes $s "Each card gives the decision, its type, the options, the mechanism, the evidence, a recommendation and what to report. Types follow Del Giudice and Gangestad (2021). Estimand choices (blue) answer different questions and are never pooled. Avoidable errors (rust) have one option that is wrong for a known reason. Analyst discretion (gray) is a set of defensible ways to answer the same question. D8 and D12 also carry a bad-control risk: the control is partly the outcome's own past."

# 3. three kinds -----------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Title $s "Three kinds of decisions call for different responses"
$kinds = @(
  @( "Estimand choice", $BLUE, "The options answer different questions.", "Separate strata, never pooled; argued from theory.", "Outcome population, identification, unit, subsector scope" ),
  @( "Analyst discretion", $MUTED, "Defensible ways to answer the same question.", "Crossed as forks; report robustness across them.", "Estimator, year, winsorizing, measure of a construct" ),
  @( "Avoidable error", $RUST, "One option is wrong for a known reason.", "Kept in the grid to show its cost, then excluded.", "Per capita outcomes, raw-dollar predictors, OLS with many zeros" ) )
for ( $k = 0; $k -lt 3; $k++ ) {
  $kd = $kinds[ $k ]; $l = 40 + $k * 300; $t = 96; $w = 280
  Box $s $l $t $w 316 $TINT 5 "Kind card" | Out-Null
  T $s $kd[ 0 ] ( $l + 20 ) ( $t + 18 ) ( $w - 40 ) 28 22 $kd[ 1 ] $true $HEAD 1 $false "Kind title" | Out-Null
  $y = $t + 64
  foreach ( $pair in @( @( "What it means", $kd[ 2 ] ), @( "How it is handled", $kd[ 3 ] ), @( "Examples", $kd[ 4 ] ) ) ) {
    T $s $pair[ 0 ] ( $l + 20 ) $y ( $w - 40 ) 16 12 $FAINT $true $BODY 1 $false "Kind label" | Out-Null
    T $s $pair[ 1 ] ( $l + 20 ) ( $y + 18 ) ( $w - 40 ) 54 15 $INK $false $BODY 1 $false "Kind text" | Out-Null
    $y += 80
  }
}
T $s "Pooling estimand choices with interchangeable ones produces a spread with no interpretation. Two cards (D8, D12) also carry a bad-control risk." 40 432 880 48 15 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s $i
Notes $s "Following Del Giudice and Gangestad (2021). Estimand choices split the multiverse into strata; analyst choices are crossed inside each stratum; avoidable errors stay in the grid only to show what they cost."

# 4. impact ------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Title $s "Four decisions change conclusions; the usual checks do not"
Pic $s "impact.png" 40 84 560 430 "Dot plot of the share of covariate verdicts that change when only one decision changes, for filers, grassroots and spending" | Out-Null
Bul $s @( "Shaded: population, identification, measurement and predictor form change 22–44% of verdicts.",
          "Population is the largest decision for filers (41%); identification for grassroots (42%).",
          "Year, winsorizing, log P on the right and the money measure change 5–17%.",
          "The hospital and university exclusion matters only for spending (23% vs. 2%)." ) 628 110 292 360 15 $INK "Takeaways" | Out-Null
Source $s "Run v11; matched pairs differing in one decision"
Num $s $i
Notes $s "A verdict is significantly positive, not significant or significantly negative. Verdicts changed is the share of matched specification pairs, differing only in the one decision, in which a covariate's verdict differs. Mean |delta r| for the top decisions is .07–.11; for year, winsorizing, log P and the money measure it is .04 or less. With 10^4–10^5 specifications every F test is significant, so eta-squared (.06 threshold) is the screen. Source: R/98_decision_matrix.R, R/97_v11_decision_summary.R."

# part 1 -----------------------------------------------------------------------------------------------
$s = Divider $pres "1" "What is being estimated" "Population, unit and identification are estimand choices: each option answers a different question (D1–D3)"; $i++
Notes $s "Part 1 covers the estimand choices that define the question. They are argued from theory and never pooled across options."

# D1 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D1" "Filers and grassroots answer different questions"
Claim $s "Which organizations density counts is an estimand choice. The same 13-covariate model gives filers, grassroots and spending different answers, and the choice changes signs."
Stats3 $s @( ".67", "Correlation of filer and grassroots SAMIs; the registered stock correlates .97 with grassroots" ) @( "41%", "Covariate verdicts that change with the population alone: the largest decision for filers" ) @( "−.20 vs +.13", "% nonwhite for filers vs. grassroots (partial r, 385 metros, 2022)" )
Design $s @( "Match the population to the theory: service capacity calls for filers, civic life for grassroots, resources for spending.",
             "Report filers and grassroots separately. The pooled registered stock is mostly grassroots and hides their disagreements.",
             "When counts and spending disagree, the finding is about the number vs. the size of organizations." )
Num $s $i
Notes $s "Filers: 501(c)(3) public charities filing a 990 or 990-EZ with revenue or expenses of at least 50k (2010 dollars). Grassroots: registered charities that do not file at that level (72–80% of the registered stock). All three counts scale at about 1.0; spending at 1.29. Filers follow the local economy; grassroots follow demography and institutional legacy. Unregistered congregations are outside every count. Check registered and grassroots counts for group-exemption artifacts (Memphis, Mankato)."

FigSlide $pres "D1" "The same model, three populations" "d1_populations.png" "Partial r of the 13 covariates for filers, grassroots and spending" @(
  "% nonwhite is negative for filers (−.20) and positive for grassroots (+.13).",
  "Religious fractionalization (.51) and turnout (.24) carry grassroots; Medicaid (.24) carries filers.",
  "Poverty is negative for every population and strongest for filers (−.29); with six covariates it changed sign.",
  "% BA, foundation assets, the endowment and S&L earnings agree in sign everywhere." ) "13-covariate log-log model, 385 metros, 2022" ( $i + 1 ) | ForEach-Object { Notes $_ "Rows are sorted by the gap between filers and grassroots. Spending excludes hospitals and universities. In the multiverse, filers vs. grassroots explains eta-squared .69 of the spread for % nonwhite, .58 for turnout and .57 for religious fractionalization." }
$i++

# D2 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D2" "Density is a market question: use metros"
Claim $s "Counties split labor and service markets and create zeros. Counties compared within a metro measure where organizations locate inside a market, not how many it supports."
Stats3 $s @( "0.97–0.99", "Filer count exponent at every unit; spending runs 1.25–1.42 (1.60 within metros)" ) @( "−.29 vs +.12", "Poverty for filers across metros vs. across counties within metros" ) @( "26%", "Filer verdicts that change with the unit (metro vs. metro plus micro)" )
Design $s @( "Use metro CBSAs or CSAs for density: they match markets, have no zeros and agree with each other.",
             "Use counties only for location within a market, with metro fixed effects, and say so.",
             "Treat a population threshold like the unit: report large-city claims for metros of 250k+ as well." )
Num $s $i
Notes $s "Every term was rebuilt from county data for each unit (the rebuilt metro values correlate 1.000 with the panel). The endowment and Medicaid are strongest at the market level. Do not pool micropolitan areas with metros without testing whether they behave alike. Compare spending exponents only across studies that use the same unit."

$s = NewSlide $pres; $i++
Head $s "D2" "Poverty reverses only inside metros"
$d2 = @( @( "Partial r, filer counts, 2022", "Poverty", "% BA", "Endowment", "Medicaid" ),
         @( "County", "−.06", ".30", ".16", ".10" ),
         @( "Metro", "−.29", ".37", ".32", ".24" ),
         @( "Metro + micro", "−.21", ".29", ".23", ".17" ),
         @( "Micro only", "−.19", ".28", ".21", ".13" ),
         @( "CSA or metro", "−.34", ".36", ".34", ".25" ),
         @( "Counties within metros (metro FE)", "+.12", ".41", ".30", ".04" ) )
Tbl $s $d2 40 92 @( 300, 145, 145, 145, 145 ) 14 @( ,@( 7, 2 ) ) | Out-Null
NoteCards $s "Across markets, poorer places have fewer filers. Within a metro, organizations sit in the core county, where need and headquarters concentrate." "With 13 covariates, poverty is negative at every metro size. The old reversal in large metros was an omitted-variable pattern." 290 96
T $s "Case exclusions (DC, Alaska and Hawaii, group-exemption metros, size extremes) move results less than winsorizing does: one robustness line, not a fork." 40 410 880 48 15 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s $i
Notes $s "Poverty's effect for filers is concave in metro size (joint p .018) but negative at every size: median t −4.7 at 100k, −2.7 at 250k, −0.8 at 1M across 1,212 default-model specifications. In the six-covariate model it was −.11 to −.14 in smaller metros and +.08 to +.13 in larger ones. Two size differences remain: foundation assets for spending peak at mid-size, and turnout for filers is negative in smaller metros and positive in larger ones."

# D3 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D3" "Within-metro estimates shrink or flip"
Claim $s "Identification is an estimand choice. Between-metro and within-metro estimators answer different questions; they are not stronger and weaker versions of one answer."
Stats3 $s @( "1.2%", "Share of filer SAMI variance within metros (grassroots 4.9%, spending 7.2%), 2010–2022" ) @( ".31 → .08", "% BA for filers, cross-section to within metros (poverty −.17 → −.02)" ) @( "42%", "Grassroots verdicts that change with identification: their largest decision" )
Design $s @( "For levels, use a cross-section or the between part of a hybrid model, with the endowment and growth kernel.",
             "For change, use long differences or growth curves over 10+ years, holding the starting level fixed.",
             "Treat metro-FE estimates as a check. In pooled panels, cluster by metro and count degrees of freedom in metros." )
Num $s $i
Notes $s "Density and its covariates are nearly fixed (Fact 2). A within estimator discards the 95–99% of variance between metros and fits interpolated ACS change, source splices and short-run adjustment. % BA and % nonwhite have under 1% of their variance within metros. Hybrid models: between effects .13–.42 SD per SD for filers and spending; within effects .03 SD or less. Counting metro-years as independent overstates the sample 8 to 13 times."

FigSlide $pres "D3" "Filers lose the between-metro story; grassroots flip" "d3_within.png" "Arrows from cross-section to within-metro median partial r for filers and grassroots" @(
  "Pooling years changes little: pooled and cross-section medians differ by .04 at most.",
  "Within metros, only the growth kernel strengthens (filers −.09 → −.15).",
  "Several grassroots terms change sign: short-run change, not the mechanism in reverse.",
  "2000–2022 adds 77% more metro-years but no precision; it crosses the 2011 splice." ) "Median partial r, default-model specifications, run v11" ( $i + 1 ) | ForEach-Object { Notes $_ "Grassroots poverty (−.07 → +.09), % nonwhite (+.12 → −.09), S&L earnings (+.07 → −.13), % 65+ (+.03 → −.14) and age SD (+.02 → +.13) change sign within metros: metros that age or lose government employment over the decade add registrations in the same years. Spending: % BA falls from .23 to .07; Medicaid is the one term that holds." }
$i++

# part 2 -----------------------------------------------------------------------------------------------
$s = Divider $pres "2" "Getting size and zeros right" "DV form, estimator and predictor form: in each, one option is wrong for a known reason (D4–D6)"; $i++
Notes $s "Part 2 covers the avoidable errors. They stay in the multiverse to show what they cost and are excluded from the recommendations."

# D4 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D4" "Per capita fixes the size coefficient at 1"
Claim $s "A per capita outcome is a log-log model with the coefficient on log P forced to 1. When that is wrong, the error flows into every covariate correlated with size."
Stats3 $s @( "1.29", "Spending scaling exponent: per capita spending still carries 0.29 × log P" ) @( "77th vs 39th", "Mean percentile of the ten largest metros, per capita vs. SAMI spending" ) @( "27%", "Spending verdicts that change with the DV form (filers 23%)" )
Design $s @( "Use log-log or the SAMI. For per capita presentation, keep log P on the right: it is the same model.",
             "Do not compare per capita spending across metros of different size: it ranks them by size.",
             "Treat size-scaled covariates like the outcome (D6): per capita covariates without log P re-create the constraint." )
Num $s $i
Notes $s "The omitted size term biases each covariate in proportion to its correlation with log P: % BA .40, foundation assets .42, % nonwhite .38, the Gini .42. Even when Y scales at 1, the coefficient on log P is no longer 1 once size-bearing covariates enter. Report the outcome's form, beta with its interval, whether log P is on the right, and the form of size-scaled covariates."

$s = NewSlide $pres; $i++
Head $s "D4" "Per capita spending ranks metros by size"
$d4a = @( @( "Spending percentile, 2022", "Per capita", "SAMI" ),
          @( "Ten largest metros (mean)", "77th", "39th" ),
          @( "New York", "94th", "51st" ),
          @( "Dallas", "66th", "20th" ),
          @( "Houston", "54th", "12th" ) )
Tbl $s $d4a 40 92 @( 220, 105, 105 ) 14 | Out-Null
$d4b = @( @( "Spending, partial r", "Log-log", "Log p.c.", "Per capita" ),
          @( "% BA", ".26", ".29", ".20" ),
          @( "% nonwhite", "−.08", ".00", ".07" ),
          @( "S&L govt earnings", ".10", ".04", ".02" ),
          @( "Pre-1970 endowment", ".16", ".09", "−.03" ) )
Tbl $s $d4b 490 92 @( 160, 90, 90, 90 ) 14 @( @( 3, 4 ), @( 5, 4 ) ) | Out-Null
NoteCards $s "Counts scale at 0.99, so per capita and SAMI ranks correlate .999. The constraint is nearly harmless for full-sector counts." "Spending scales at 1.29: ranks correlate .90, and the linear per capita form erases the endowment (.16 → −.03)." 256 96
Num $s $i
Notes $s "Adding log P to the right of the log per capita model reproduces log-log exactly, and the SAMI model gives the same partial r. With 13 covariates the per capita forms move count results less than with six, because the 13 terms leave less size for the constraint to misplace."

# D5 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D5" "OLS bias tracks the share of zeros; Poisson does not"
Pic $s "d5_zeros.png" 40 82 880 290 "County scaling exponents by NCCS group under three estimators" | Out-Null
Design $s @( "Use Poisson PML with robust SEs whenever more than a few percent of units are zero: subsectors, counties, changes.",
             "Never use log(Y + 1). An offset imposes β = 1, as per capita does (D4).",
             "Full-sector metros have no zeros: report OLS and Poisson side by side, and check spending without the largest metros." ) 40 382 880 130
Num $s $i
Notes $s "County models with the 13 covariates in SAMI form, 3,124 counties, 2022. Across 12 NCCS groups and five units, the OLS–Poisson gap correlates −.98 with the share of zeros. Dropping zeros keeps only the small places that happen to have an organization. For sparse groups OLS finds no foundation-asset effect where Poisson finds .06–.17. For full-sector spending, dropping the top 5% of metros moves Poisson % BA from .45 to .07."

# D6 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D6" "Enter size-scaled totals in SAMI form"
Claim $s "A total is mostly population. Entered raw, or logged without log P on the right, it acts as a second size measure and pushes size onto the rates that track it."
Stats3 $s @( ".80 / .94", "Correlation with log P of logged foundation assets / logged government earnings" ) @( "−.16 vs .11", "Medicaid for filers: logged total without log P vs. SAMI form" ) @( "32%", "Filer verdicts that change with predictor form (grassroots 22%, spending 26%)" )
Design $s @( "Enter totals in SAMI form, or as logged totals with log P on the right: the same model.",
             "Never enter raw dollars. Never log(1 + x) a total with zeros: floor it per capita first.",
             "Keep the model additive: interactions and squares add at most .006 to adjusted R² and raise prediction error." )
Num $s $i
Notes $s "The 13-covariate model has four totals: foundation assets, S&L earnings, Medicaid and the endowment. Raw dollars are extremely skewed (skew 9.4 for foundation assets; the five largest metros hold 43%). Predictor form explains eta-squared .70 of the spread for foundation assets and .61 for the endowment among filers."

$s = NewSlide $pres; $i++
Head $s "D6" "Only SAMI form is stable with or without log P"
T $s "Median partial r, filer counts, cross-section, with / without log P on the right (run v11)" 40 84 880 18 12 $FAINT $false $BODY 1 $false "Source" | Out-Null
$d6 = @( @( "Form", "Foundation assets", "S&L earnings", "Medicaid", "Endowment", "% BA" ),
         @( "Raw dollars", "−.02 / .01", ".04 / .04", "−.06 / −.04", ".07 / −.03", ".40 / .39" ),
         @( "Per capita", ".10 / .09", ".11 / .15", ".10 / .06", ".27 / .33", ".28 / .25" ),
         @( "Logged total", ".18 / .13", ".11 / −.04", ".07 / −.16", ".24 / .08", ".27 / .30" ),
         @( "SAMI form", ".19 / .19", ".12 / .12", ".11 / .11", ".26 / .26", ".30 / .28" ) )
Tbl $s $d6 40 108 @( 150, 146, 146, 146, 146, 146 ) 14 @( @( 2, 2 ), @( 4, 4 ), @( 2, 6 ) ) | Out-Null
NoteCards $s "Raw dollars erase foundation assets (−.02 vs. .19) and % BA absorbs the size they leave (.40)." "Eagle Pass, TX has no foundation assets: log(1 + x) put it 11 SD out and faked interactions. Floor at a low percentile." 290 96
T $s "The pre-registered meta-hypothesis holds: size dependence predicts a covariate's predictor-form sensitivity (Spearman .38–.79), not its DV-form sensitivity." 40 410 880 48 15 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s $i
Notes $s "Logged totals without log P turn Medicaid negative (−.16) and S&L earnings to −.04. Grassroots show the same pattern more weakly. Interactions: 10 of 104 survive Benjamini–Hochberg for counts; the four that looked stable with six covariates were standing in for omitted main effects. With selection repeated inside each cross-validation fold, the survivors raise prediction error 2.7% for counts."

# part 3 -----------------------------------------------------------------------------------------------
$s = Divider $pres "3" "Measures, controls and scope" "Measurement, history, geography, money, subsectors and sector age (D7–D12)"; $i++
Notes $s "Part 3 covers analyst discretion, the historical controls and the scope choices, including the two cards with a bad-control risk (D8, D12)."

# D7 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D7" "One item per construct, and the item matters"
Claim $s "Indexes help only where items are twins. Across separate constructs (diversity α .39, social capital .33, government .57) they cancel effects of opposite sign."
Stats3 $s @( "44%", "Filer and grassroots verdicts that change when one construct's measure is swapped" ) @( ".80 / .92", "η² of swapping turnout for census mail response, filers / grassroots" ) @( ".00", "η² of the growth kernel's half-life (10 vs. 5 years): the one swap that does not matter" )
Design $s @( "Use the 13 items of the variables chapter, one per construct.",
             "Report the wealth index as a check on foundation assets, and the racial-fractionalization swap for diversity claims.",
             "Do not build indices across constructs that load in opposite directions." )
Num $s $i
Notes $s "Settled in 02-variables.qmd. Where items are twins (growth, need, education and affluence), the index returns the item's estimate. Wealth is the one trade-off (alpha .85): the index gains .04–.05 for filers and spending but is significant in fewer cells. % nonwhite replaced racial fractionalization because fractionalization is not monotone in composition and ranks the border metros least diverse."

FigSlide $pres "D7" "Turnout is the fragile measure" "d7_swaps.png" "Bar chart of eta-squared when each construct's measure is swapped, filers and grassroots" @(
  "Swapping one construct moves the other terms much less (η² ≤ .21 for filers, ≤ .23 for grassroots).",
  "Unemployment for poverty matters more for filers (.38); S&L measures more for grassroots (.32).",
  "Spending is less sensitive: 27% of verdicts change.",
  "Report the measurement η² for any covariate the argument rests on." ) "One-at-a-time measurement swaps, run v11" ( $i + 1 ) | ForEach-Object { Notes $_ "Run v11 swaps one block's measure at a time: unemployment for poverty, racial fractionalization or % foreign-born for % nonwhite, four philanthropy measures for foundation assets, census mail response for turnout, the 5-year for the 10-year kernel, S&L jobs or direct general expenditure for S&L earnings, all transfers or local welfare for Medicaid. Across the seven swappable constructs, mean |delta r| is .088 for filers and .103 for grassroots." }
$i++

# D8 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D8" "The endowment and growth kernel split one fact"
Claim $s "Density is persistent, so a covariate high decades ago gets credit for density built then. Historical controls change the question to density relative to history."
Stats3 $s @( "−.62", "Correlation of the pre-1970 endowment and the growth kernel: fast growers have little old stock" ) @( ".02", "R² the endowment adds to the other 12 terms (.09 in the six-covariate model)" ) @( "91–96%", "Count specifications in which the endowment is positive" )
Design $s @( "Include the endowment for current conditions; leave it out to study how endowments arose. Report both.",
             "Include the growth kernel and read it with the endowment: neither alone measures the Sunbelt lag.",
             "Never control for market share in levels models: it is partly the outcome. Use it as a bound." )
Num $s $i
Notes $s "The endowment: 501(c)(3) public charities in the 2000 BMF stock with a ruling date before 1970, in SAMI form; Memphis and Mankato set to missing. The growth kernel: a geometric memory of past log population change at a 10-year half-life, built from 1970–2024 population; it varies over time, so it also runs within metros. Dropping the endowment changes 13% of filer verdicts; dropping the kernel 10%."

$s = NewSlide $pres; $i++
Head $s "D8" "What each historical control absorbs"
$d8 = @( @( "360 metros, SAMI form, 2022", "R², filers", "R², spending", "Endowment r", "Kernel r", "Share r" ),
         @( "No endowment", ".773", ".496", "", "−.25", "" ),
         @( "Default (13 covariates)", ".792", ".508", ".29", "−.09", "" ),
         @( "No endowment, + market share", ".784", ".557", "", "−.18", ".23" ),
         @( "Default + market share", ".801", ".565", ".28", "−.05", ".22" ),
         @( "No growth kernel", ".790", ".507", ".37", "", "" ) )
Tbl $s $d8 40 92 @( 250, 126, 126, 126, 126, 126 ) 14 | Out-Null
NoteCards $s "Adding the endowment moves the kernel from −.25 to −.09; dropping the kernel moves the endowment from .29 to .37." "Market share absorbs Medicaid (filers .28 → .22, spending .18 → .07) but leaves the endowment intact (.29 → .28)." 282 96
T $s "Partial r for filer counts. Market share is the BLS 501(c)(3) share of private employment, 2013–2022; it includes hospitals and universities." 40 400 880 40 14 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s $i
Notes $s "The endowment absorbs a little philanthropy and government: foundation assets .27 → .20, S&L earnings .28 → .22, % BA .41 → .38. With six covariates it took nearly half of each. In the multiverse, dropping the endowment moves the growth kernel from −.09 to −.24 for filers and from −.15 to −.29 for grassroots."

# D9 ---------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D9" "State-patterned terms move most under fixed effects"
Claim $s "Fixed effects remove the between-state part of every variable. Covariates set by state policy or regional demography lose most of their identifying variation."
Stats3 $s @( ".24 → .06", "Medicaid for filers, OLS vs. state FE (religious fractionalization .12 → .00)" ) @( ".83–.92", "Rank agreement of covariate t-statistics, OLS vs. state FE, across populations" ) @( "70% → 40%", "Filer specifications with religious fractionalization significant: robust vs. state-clustered SEs" )
Design $s @( "Report OLS, division FE and state FE side by side for key covariates; do not choose one silently.",
             "Report between-state variance shares. Above about .3, say whether the claim is regional or within-state.",
             "Cluster SEs by state (a wild cluster bootstrap with ~50 clusters); check state FE without multi-state metros." )
Num $s $i
Notes $s "Between-state variance shares: turnout .65, % nonwhite .64, religious fractionalization .58, growth kernel .52, Medicaid .50, endowment .46, poverty .39, S&L earnings .31, % BA .21, foundation assets .19. The estimator explains the most spread for grassroots % nonwhite (.54) and religious fractionalization (.51), and for filer poverty (.25) and % BA (.23)."

FigSlide $pres "D9" "Fixed effects move the state-patterned terms" "d9_statefe.png" "Arrows from OLS to state fixed-effects partial r for filers, sorted by between-state variance share" @(
  "The benchmarks hold: % nonwhite is −.18 to −.20 under all four estimators; poverty stays negative (−.18).",
  "The growth kernel is the exception: half its variance is between states, yet it barely moves.",
  "Clustering changes inference, not estimates: % BA stays significant in 100%.",
  "43 metros cross a state line; with them in, state FE understates the endowment (.18 vs. .25)." ) "13-covariate log-log model, 385 metros, 2022" ( $i + 1 ) | ForEach-Object { Notes $_ "Without the multi-state metros the typical term moves .009–.015. For spending the endowment roughly halves without them under every estimator: a sample effect, since the 43 include most of the largest old metros (New York, Philadelphia, Boston, Washington)." }
$i++

# D10 --------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D10" "Exclude hospitals and universities from spending"
Claim $s "Which money to count matters through one choice. Hospitals and universities make an all-sector spending SAMI a measure of ownership and filing location."
Stats3 $s @( "47%", "Hospital and university share of 501(c)(3) spending nationally; ~0 to 75% across metros" ) @( "23% vs 5%", "Spending verdicts changed by the exclusion vs. by expenses/revenue and gross/net" ) @( "100th → 34th", "Rochester, MN spending percentile without them; Missoula rises 55th → 91st" )
Design $s @( "Exclude hospitals and universities by default; report the all-sector version as a robustness check.",
             "Use expenses rather than revenue: the same cross-section, less year-to-year noise.",
             "Net out pass-through grants when the question is the size of the local sector." )
Num $s $i
Notes $s "Public universities and government hospitals file no 990, and system hospitals may file at a headquarters elsewhere. Excluding them moves S&L earnings from −.02 to .10, % BA from .19 to .26 and foundation assets from .05 to .11; religious fractionalization goes the other way (.12 to −.01). Netting pass-through lowers % BA only from .26 to .24. For counts the exclusion is immaterial (2% of verdicts). E-file NTEE codes were backfilled from the BMF on 2026-11-17."

# D11 --------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D11" "The full sector averages different rules"
Claim $s "A full-sector result is a weighted average of subsectors that scale differently and respond to different conditions. Human services alone is 35% of filers."
Stats3 $s @( "0.88–1.21", "Count scaling exponents across subsectors, hospitals to international (Poisson, 2022)" ) @( "11 of 11", "Subsectors sharing the full-sector sign for Medicaid and the endowment; turnout 5 of 11" ) @( ".997", "Correlation of results with and without religion-related and mutual-benefit filers" )
Design $s @( "Use the full sector for overall density, and report which subsectors carry the result.",
             "When the theory is about one kind of organization, model that subsector directly, with Poisson.",
             "Do not drop subsectors as noise. Treat diversity and turnout findings as subsector-specific." )
Num $s $i
Notes $s "The six civic subsectors (arts, education, environment, health, human services, public benefit) correlate .56–.79 with each other; religion-related, hospitals, universities and mutual benefit correlate .05–.46 with the rest. Subsector counts were rebuilt on 2026-11-17; no partial r moved by more than .004."

FigSlide $pres "D11" "How many subsectors share the full-sector sign" "d11_subsectors.png" "Bar chart of how many of 11 subsectors share the full-sector sign for each covariate" @(
  "Religion-related organizations reverse % BA, S&L earnings and turnout, and track religious fractionalization (.36).",
  "The growth kernel splits subsectors: environment, international and religion-related are denser where growth was recent.",
  "Poverty is now among the most consistent terms (10 of 11; 6 with six covariates)." ) "Poisson PML with log P, 13 covariates, 2022 counts" ( $i + 1 ) | ForEach-Object { Notes $_ "Dropping religion-related and mutual-benefit filers moves only a few ministry-heavy metros (Cleveland TN, Tulsa, Colorado Springs); religious fractionalization for filers moves most (.12 → .09)." }
$i++

# D12 --------------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Head $s "D12" "Sector age is a bad control"
Claim $s "Mean filer age mixes the sector's history with how fast filers were added. New filers are young, so metros that add many filers have young sectors."
Stats3 $s @( ".792 → .794", "R² of the levels model with mean filer age added: nothing left to explain" ) @( ".16 → .33", "R² of the 2010–2022 change model with 2022 age added: mechanical" ) @( "−.65", "Correlation of 2022 filer age with population growth in the change window" )
Design $s @( "Do not control for current sector age in levels or change models.",
             "Use the pre-1970 endowment for history: a cleaner measure, fixed before the outcome period.",
             "Use age descriptively, for regional differences in market share and organization size (Fact 5)." )
Num $s $i
Notes $s "Mean filer age correlates .60 with the pre-1970 endowment and −.69 with population growth. The metros that gained the most filers (Logan, Farmington, Santa Rosa, Las Vegas) now have young filer sectors; those that lost (Wheeling, Weirton, Elmira) have old ones. Age measured before the window does nothing (partial r −.06). The ruling year marks IRS recognition, not founding."

# synthesis ----------------------------------------------------------------------------------------------
$s = NewSlide $pres; $i++
Title $s "Following every card sharpens the answer"
$syn = @( @( "Population", "What the recommended specifications show" ),
          @( "Filers", "% nonwhite (−.17) and poverty (−.23) become clear findings; Medicaid (.13) and age SD (.15) emerge" ),
          @( "Grassroots", "Religious fractionalization (.36) and turnout (.25) are significant in every recommended specification" ),
          @( "Spending", "Medicaid (.17) and age SD (.15) join % BA and the pre-1970 endowment" ) )
Tbl $s $syn 40 92 @( 160, 720 ) 15 | Out-Null
$syn2 = $s.Shapes.Item( "Table" ).Table
for ( $r = 2; $r -le 4; $r++ ) { $syn2.Cell( $r, 2 ).Shape.TextFrame.TextRange.ParagraphFormat.Alignment = 1 }
$syn2.Cell( 1, 2 ).Shape.TextFrame.TextRange.ParagraphFormat.Alignment = 1
Stats3 $s @( "−.23", "Poverty for filers across the recommended specifications" ) @( ".36", "Religious fractionalization for grassroots, significant in every one" ) @( ".17", "Medicaid for spending excluding hospitals and universities" ) 236
T $s "Which covariates enter is itself a large decision: moving from six to 13 covariates makes poverty clearly negative for filers, roughly halves foundation assets and dissolves the interactions that looked stable." 40 412 880 60 15 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s $i
Notes $s "From 05-findings.qmd. Filer density follows education, need and institutional history; grassroots density follows civic and religious structure and recent growth; spending shares the filers' education and endowment terms, more weakly. % nonwhite is negative for filers and positive for grassroots."

# closing -------------------------------------------------------------------------------------------------
$s = NewSlide $pres $DARK; $i++
T $s "Recommended defaults" 60 32 840 50 34 "FFFFFF" $true $HEAD 1 $false "Title" 3 | Out-Null
$def = @(
  @( "D1", "Filers and grassroots, separately", "Match the population to the theory" ),
  @( "D2", "Metro CBSAs or CSAs", "Counties only with metro FE, for location" ),
  @( "D3", "Cross-section for levels", "Long differences for change; metro FE as a check" ),
  @( "D4", "Log-log or SAMI outcome", "Per capita only with log P on the right" ),
  @( "D5", "Poisson PML when zeros exist", "Never log(Y + 1)" ),
  @( "D6", "Totals in SAMI form", "Floor zeros per capita; keep the model additive" ),
  @( "D7", "One item per construct", "Report the key measurement swaps" ),
  @( "D8", "Endowment and growth kernel", "Read together; market share only as a bound" ),
  @( "D9", "OLS, division FE and state FE", "Side by side; cluster by state" ),
  @( "D10", "Expenses, without hospitals and universities", "Net out pass-through for sector size" ),
  @( "D11", "Full sector plus the theory's subsector", "Poisson for subsectors" ),
  @( "D12", "No current-age control", "Age for description only" ) )
for ( $k = 0; $k -lt 12; $k++ ) {
  $col = [math]::Floor( $k / 6 ); $row = $k % 6; $l = 60 + $col * 430; $t = 100 + $row * 62
  Badge $s $def[ $k ][ 0 ] $l $t 34 | Out-Null
  T $s $def[ $k ][ 1 ] ( $l + 48 ) ( $t + 1 ) 360 22 16 "FFFFFF" $true $HEAD 1 $false "Point" | Out-Null
  T $s $def[ $k ][ 2 ] ( $l + 48 ) ( $t + 25 ) 360 20 13 $LIGHT $false $BODY 1 $false "Point text" | Out-Null
}
T $s "Each card in 04-design-decisions.qmd gives the evidence behind the default and what to report." 60 488 840 22 14 $LIGHT $false $BODY 1 $true "Footer" | Out-Null
Notes $s "What to report, by card. D1: population definition and results for filers and grassroots. D2: unit, delineation vintage, number of units and zeros. D3: which variation identifies each estimate, within-metro variance shares, clustering. D4: outcome form, beta with interval, log P on the right. D5: share of zeros, estimator, sensitivity to the largest units. D6: form of every total and its correlation with log P. D7: item per construct and measurement eta-squared. D8: which historical controls and why. D9: between-state shares, three-estimator estimates, robust vs. clustered significance. D10: money measure and hospital/university handling. D11: subsector scaling and sign agreement. D12: age measure and timing, with and without it."

# ---- save and export ------------------------------------------------------------------------------
if ( Test-Path $out ) { Remove-Item $out -Force }
$pres.SaveAs( $out, 24 )
if ( $Export ) {
  New-Item -ItemType Directory -Force $Export | Out-Null
  for ( $k = 1; $k -le $pres.Slides.Count; $k++ ) { $pres.Slides.Item( $k ).Export( ( Join-Path $Export ( "s{0:D2}.png" -f $k ) ), "PNG", 1600, 900 ) }
}
$pres.Close(); $app.Quit()
[System.Runtime.InteropServices.Marshal]::ReleaseComObject( $app ) | Out-Null
"Saved $out ($( (Get-Item $out).Length ) bytes, $i slides)"
