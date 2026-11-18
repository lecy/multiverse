# build_findings_deck.ps1
# Draft deck for paper/05-findings.qmd (summarized multiverse results), built by driving PowerPoint (COM).
# Same design system as build_stylized_facts_deck.ps1 and build_design_decisions_deck.ps1.
# Usage (Windows PowerShell, from multiverse/paper/slides):  powershell -File build_findings_deck.ps1 [-Export <png folder>]
# Output: findings.pptx next to this script. Charts come from ./figs (make_findings_figures.R; impact.png from make_deck_figures.R).
param( [string]$Export = "" )
$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$fig  = Join-Path $here "figs"
$out  = Join-Path $here "findings.pptx"

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
# Badge: as in the stylized-facts deck; the label shrinks for longer labels.
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

# Wide-figure slide: chart on the left (600 wide), takeaways on the right.
function WideFig( $pres, [string]$n, [string]$title, [string]$file, [string]$alt, [string[]]$items, [string]$src ) {
  $s = NewSlide $pres
  Head $s $n $title
  Pic $s $file 40 84 600 420 $alt | Out-Null
  Bul $s $items 664 104 256 370 14 $INK "Takeaways" | Out-Null
  if ( $src ) { Source $s $src 664 486 256 }
  Num $s
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
T $s "What the multiverse finds" 60 100 840 110 44 "FFFFFF" $true $HEAD 1 $false "Title" 4 | Out-Null
T $s "Five findings from 432,432 specifications of the 13-covariate model" 60 232 840 34 22 $LIGHT $false $HEAD 1 $true "Subtitle" | Out-Null
T $s "Jesse Lecy and Jeremy P. Thornton" 60 302 840 24 16 "FFFFFF" $false $BODY 1 $false "Authors" | Out-Null
T $s "Multiverse run v11: 387 US metros and 925 CBSAs, 2010–2022" 60 328 840 22 14 $LIGHT $false $BODY 1 $false "Data" | Out-Null
for ( $k = 1; $k -le 5; $k++ ) { Badge $s "$k" ( 60 + ( $k - 1 ) * 52 ) 430 36 | Out-Null }
Notes $s "All results are from run v11: 432,432 specifications of the 13-covariate model over 387 metros and 925 CBSAs, 2010–2022. The registered-stock specifications were also refit with grassroots (registered public charities that do not file above the floor) as the outcome. Every summary reports filers, grassroots and spending separately, because the two count populations disagree on several covariates."

# 2. overview ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Title $s "Five findings"
$cards = @(
  @( "1", "Four decisions change conclusions", "Population, identification, measurement and predictor form change 22–44% of verdicts; the usual checks 5–17%" ),
  @( "2", "Robust covariates differ by population", "% BA for filers and spending; religious fractionalization and turnout for grassroots" ),
  @( "3", "Which covariates enter is a decision", "Six to 13 covariates turns poverty for filers from −.02 to −.17" ),
  @( "4", "Following every card sharpens the answer", "The totals strengthen once the weak options are set aside together" ),
  @( "5", "Three populations, three questions", "% nonwhite is negative for filers and positive for grassroots" ) )
$cw = 280; $ch = 196
for ( $k = 0; $k -lt 6; $k++ ) {
  $col = $k % 3; $row = [math]::Floor( $k / 3 ); $l = 40 + $col * ( $cw + 20 ); $t = 96 + $row * ( $ch + 16 )
  if ( $k -lt 5 ) {
    $c = $cards[ $k ]
    Box $s $l $t $cw $ch $TINT 5 "Finding card $( $c[0] )" | Out-Null
    Badge $s $c[ 0 ] ( $l + 16 ) ( $t + 16 ) 34 | Out-Null
    T $s $c[ 1 ] ( $l + 16 ) ( $t + 62 ) ( $cw - 32 ) 52 17 $DARK $true $HEAD 1 $false "Card title" | Out-Null
    T $s $c[ 2 ] ( $l + 16 ) ( $t + 120 ) ( $cw - 32 ) 66 14 $MUTED $false $BODY 1 $false "Card text" | Out-Null
  } else {
    Box $s $l $t $cw $ch $DARK 5 "Data card" | Out-Null
    T $s "The multiverse" ( $l + 16 ) ( $t + 18 ) ( $cw - 32 ) 24 17 "FFFFFF" $true $HEAD 1 $false "Card title" | Out-Null
    T $s ( "Run v11: 432,432 specifications of the 13-covariate model`r387 metros and 925 CBSAs, 2010–2022`rGrassroots: 39,312 registered-stock specifications refit with non-filers" ) ( $l + 16 ) ( $t + 52 ) ( $cw - 32 ) 134 13 $LIGHT $false $BODY 1 $false "Card text" | Out-Null
  }
}
Num $s
Notes $s "A verdict is significantly positive, not significant or significantly negative. Verdicts changed is the share of matched specification pairs, differing only in one decision, in which a covariate's verdict differs. With 10^4–10^5 specifications every F test is significant, so eta-squared (.06 threshold) is the screen for whether a decision matters for a covariate."

# 3. finding 1 ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "1" "Four decisions change conclusions"
Claim $s "Changing only the population, the identification, the measure of a construct or the predictor form changes 22–44% of covariate verdicts. The forks most papers check change 5–17%."
Stats3 $s @( "41%", "Verdicts changed by the population (filers vs. grassroots): the largest decision for filers" ) @( "42%", "Verdicts changed by identification for grassroots: their largest decision" ) @( "5–17%", "Verdicts changed by the year, winsorizing, log P on the right or the money measure" )
Design $s @( "Argue the estimand choices (population, identification, unit) from theory; never average across them.",
             "Spend robustness effort on measurement and predictor form, not on the year or winsorizing.",
             "The hospital and university exclusion matters only for spending (23% of verdicts vs. 2% for filers)." ) -header "What it means for practice"
Num $s
Notes $s "Mean |delta r| for the top decisions is .07–.11; for year, winsorizing, log P and the money measure it is .04 or less. Grassroots are more sensitive to identification and measurement and less to predictor form: their strongest covariates (religious fractionalization, turnout) are rates, so the form in which totals enter matters less (22% of verdicts vs. 32% for filers)."

WideFig $pres "1" "What moves the verdicts, decision by decision" "impact.png" "Dot plot of the share of covariate verdicts that change when only one decision changes, for filers, grassroots and spending" @(
  "Shaded: the four decisions that change 22–44% of verdicts.",
  "Population is the largest decision for filers (41%); identification for grassroots (42%).",
  "Unit changes 20–26% for the count populations.",
  "The usual robustness checks sit at the bottom: year, winsorizing, log P and the money measure." ) "Run v11; matched pairs differing in one decision" | ForEach-Object { Notes $_ "From the decision-impact table in 05-findings.qmd (tbl-impact). The population row compares filers and grassroots in matched pairs, so it is the same for both; for spending it compares counts and spending." }

$s = NewSlide $pres
Head $s "1" "Each decision governs different covariates"
$gov = @( @( "Decision", "Covariates it governs (η², share of the spread it explains)" ),
          @( "Predictor form", "The totals: foundation assets (.70 filers, .64 grassroots), endowment (.61, .48), growth kernel (.59, .63), Medicaid (.41, .40)" ),
          @( "Population", "% nonwhite (.69), turnout (.58), religious fractionalization (.57), % BA (.46): where filers and grassroots differ" ),
          @( "Identification, filers", "% BA (.73), poverty (.72), age SD (.69)" ),
          @( "Identification, grassroots", "% nonwhite (.87), religious fractionalization (.76), poverty (.63): the terms that flip within metros" ),
          @( "Estimator, grassroots", "% nonwhite (.54), religious fractionalization (.51), turnout (.32): the state-patterned terms" ) )
Tbl $s $gov 40 92 @( 210, 670 ) 14 | Out-Null
$gt = $s.Shapes.Item( "Table" ).Table
for ( $r = 1; $r -le 6; $r++ ) { $gt.Cell( $r, 2 ).Shape.TextFrame.TextRange.ParagraphFormat.Alignment = 1 }
NoteCards $s "No covariate is moved by every decision, and no decision moves every covariate." "A decision matters for a covariate when it explains η² ≥ .06 of its spread. With 10⁴–10⁵ specifications, every F test is significant." 276 96
Num $s
Notes $s "The full decision matrix (every decision by every covariate, one pair of figures per population) is in 05-findings.qmd: figures/decision_matrix_v11_filers_vertical_p1.png and p2, and the grassroots and spending versions. Option means pool strata while eta-squared is computed within them, so a decision can matter even when its pooled means sit close together."

# 4. finding 2 ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "2" "Robust covariates differ by population"
Claim $s "Across every cross-section specification, a handful of covariates keep their sign and significance. They are not the same for filers, grassroots and spending."
Stats3 $s @( "100%", "% BA: filer and spending cross-sections with a positive sign; significant in 99–100%" ) @( ".28 / .20", "Religious fractionalization / turnout for grassroots, significant in 98–100%" ) @( "90–96%", "Specifications in which the pre-1970 endowment is positive, in all three populations" )
Design $s @( "Poverty is a filer covariate (−.17, significant in 86%), not a grassroots one (−.07, 45%).",
             "% nonwhite has opposite signs for filers (−.07) and grassroots (+.11); pooling the two put it near zero.",
             "Spending is weaker than filer counts for every term except age SD, Medicaid and % 65+ (R² .45 vs. .79)." ) -header "Reading the stability table"
Num $s
Notes $s "The robust terms survive every decision except within-metro identification, where the endowment drops out and the others shrink. A few large organizations dominate spending, and the covariates explain less of it (R² .45 against .79 for filers in the single model)."

WideFig $pres "2" "Median effect and significance, all cross-sections" "results_stability.png" "Median partial r of the 13 covariates for filers, grassroots and spending, with the share of specifications significant" @(
  "Filled points: significant in 75% or more of specifications (robust SEs).",
  "Filers: % BA, the endowment and poverty. Grassroots: religious fractionalization, turnout, the endowment, the growth kernel and % BA. Spending: % BA alone.",
  "Within metros, grassroots keep more signal (% BA .14, growth kernel −.19) and reverse more signs.",
  "State-clustered SEs trim filer religious fractionalization (55% → 44%) and turnout (48% → 41%)." ) "Every cross-section specification of run v11 in which the covariate appears" | ForEach-Object { Notes $_ "From tbl-stability. Rows are ordered by the filer median. Sign agreement: % BA 100% for filers and spending; religious fractionalization and turnout 100% for grassroots; % nonwhite 83% (filers) and 98% (grassroots) with opposite signs. Medicaid agrees in sign in only 57–74% of specifications." }

# 5. finding 3 ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "3" "Which covariates enter is itself a decision"
Claim $s "Moving from the six-covariate to the 13-covariate default changes the filer results as much as any fork. A paper that treats its covariates as fixed hides that choice."
Stats3 $s @( "−.02 → −.17", "Poverty for filers, six to 13 covariates; significant in 20% → 89%" ) @( ".19 → .11", "Foundation assets for filers (significant in 82% → 65%)" ) @( ".16 → .07", "Turnout for filers (74% → 41%); it remains a grassroots covariate (.20)" )
Design $s @( "Report covariate selection as part of the multiverse, next to identification and measurement.",
             "Expect % BA to hold: it is the one benchmark that does not depend on what else is in the model (.33 → .31).",
             "Treat interactions found with fewer covariates as suspect: the stable four now keep their sign in only 62–76%." ) -header "What it means for practice"
Num $s
Notes $s "Run v10 used the six-covariate default; run v11 the 13-covariate default. Grassroots were not an outcome in v10. The four interactions that looked stable with six covariates (turnout squared, turnout x poverty, turnout x % nonwhite, % BA x % nonwhite) were standing in for omitted main effects."

WideFig $pres "3" "Six benchmarks, six vs. 13 covariates" "results_v10_v11.png" "Arrows from six-covariate to 13-covariate median partial r for six benchmark covariates, filers and spending" @(
  "Poverty moves most: from no effect to a clear negative one. It correlates .34 with Medicaid and .36 with the Gini.",
  "The old poverty reversal with metro size disappears (D2).",
  "Foundation assets and turnout lose about half their strength to the endowment, Medicaid and the civic terms.",
  "% BA barely moves." ) "Default-model cross-sections, runs v10 and v11" | ForEach-Object { Notes $_ "From tbl-v10-v11. For spending, poverty goes from .07 to −.03 and turnout from .07 to .02 (significant in 37% → 4%)." }

# 6. finding 4 ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "4" "Following every card sharpens the answer"
Claim $s "The naive answer pools every default specification; the recommended answer follows all twelve cards at once (42 specifications per population). The totals gain most."
Stats3 $s @( ".11 → .25", "Foundation assets for filers, naive to recommended (significant in 100%)" ) @( "−.23", "Poverty for filers in the recommended specifications (significant in 98%)" ) @( ".36", "Religious fractionalization for grassroots, significant in every recommended specification" )
Design $s @( "Cross-section and metro; a log-log or SAMI outcome; SAMI-form predictors, or logged with log P on the right.",
             "Expenses excluding hospitals and universities for spending; OLS, division FE or state FE.",
             "Each weak option pulled the totals down; removing them together removes the pull." ) -header "The recommended specification"
Num $s
Notes $s "Naive: every default-model specification of the population, all identifications (1,540 for each count population, 6,160 for spending across four money measures). Recommended: 42 specifications each."

WideFig $pres "4" "Naive vs. recommended, by population" "results_naive_rec.png" "Arrows from naive to recommended median partial r for 13 covariates and three populations, with the share significant when recommended" @(
  "Filers: % nonwhite (−.17) and poverty (−.23) become findings; turnout (.03) is never significant.",
  "Grassroots: religious fractionalization (.36), turnout (.25) and the endowment; not poverty, age SD or Medicaid.",
  "Spending: % BA, the endowment, Medicaid and age SD are significant in 98% or more.",
  "The growth kernel fades in the cross-section: with the endowment in, the two split one fact." ) "Run v11; bold: significant in 75%+ when recommended" | ForEach-Object { Notes $_ "From tbl-naive. Rows are ordered by the filers' recommended median. Within metros the growth kernel remains the main term." }

# 7. finding 5 ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Head $s "5" "Three populations, three questions"
Claim $s "Each population has its own robust covariates. A study has to say which population its theory is about and report it separately."
$pops = @(
  @( "Filers", $BLUE, "Education, need and institutional history", @( @( ".32", "% BA" ), @( "−.17", "Poverty rate" ), @( ".21", "Pre-1970 endowment" ), @( "−.07", "% nonwhite" ) ) ),
  @( "Grassroots", $ORANGE, "Civic and religious structure, and recent growth", @( @( ".28", "Religious fractionalization" ), @( ".20", "Turnout" ), @( "−.16", "Growth kernel" ), @( "+.11", "% nonwhite" ) ) ),
  @( "Spending", $DARK, "The filers' education and endowment terms, more weakly", @( @( ".24", "% BA" ), @( ".10", "Pre-1970 endowment" ), @( ".10", "Age SD" ), @( ".45", "R² (filers .79)" ) ) ) )
for ( $k = 0; $k -lt 3; $k++ ) {
  $pp = $pops[ $k ]; $l = 40 + $k * 300; $t = 162; $w = 280
  Box $s $l $t $w 268 $TINT 5 "Population card" | Out-Null
  T $s $pp[ 0 ] ( $l + 20 ) ( $t + 16 ) ( $w - 40 ) 28 22 $pp[ 1 ] $true $HEAD 1 $false "Population" | Out-Null
  T $s $pp[ 2 ] ( $l + 20 ) ( $t + 50 ) ( $w - 40 ) 40 14 $MUTED $false $BODY 1 $true "Population summary" | Out-Null
  $y = $t + 100
  foreach ( $row in $pp[ 3 ] ) {
    T $s $row[ 0 ] ( $l + 20 ) $y 70 28 22 $BLUE $true $HEAD 1 $false "Median" 3 | Out-Null
    T $s $row[ 1 ] ( $l + 96 ) $y ( $w - 116 ) 28 14 $INK $false $BODY 1 $false "Covariate" 3 | Out-Null
    $y += 38
  }
}
T $s "Median partial r across every cross-section specification. % nonwhite has opposite signs for filers and grassroots, each with 83–98% sign agreement." 40 446 880 44 14 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s
Notes $s "Filer density follows education, need and institutional history. Grassroots density follows civic and religious structure and recent growth. Spending shares the filers' education and endowment terms, more weakly: a few large organizations dominate it, and the covariates explain less of it."

# 8. sensitivity ---------------------------------------------------------------------------------------
$s = NewSlide $pres
Title $s "Sensitivity analyses: reported, not forked"
$sens = @( @( "Check", "Largest effect", "Treatment" ),
           @( "Outlying cases (DC, Alaska and Hawaii, size extremes)", "Median |Δr| ≤ .013, less than winsorizing (.016)", "One robustness sentence" ),
           @( "Covariate-outlier trim (|z| > 4)", "Drops Utah and retirement metros; % 65+ moves .07, age SD .05", "Age terms with and without" ),
           @( "City size (250k split; quadratic size term)", "Poverty for filers weakens in large metros but stays negative", "Note (D2)" ),
           @( "Interactions and squares (104 terms)", "10 (counts) and 4 (spending) survive BH; prediction error rises", "Robustness paragraph" ),
           @( "Religion-related and mutual-benefit filers dropped", "Partial r correlates .997 (counts) and .991 (spending)", "One sentence (D11)" ),
           @( "Market concentration (HHI, CR4)", "Partly the outcome; ΔR² ≤ .015", "Excluded; one extension" ),
           @( "Spending correction (e-file NTEE codes, 2020–24)", "Spending partial r moves ≤ .04", "One sentence (D10)" ),
           @( "2018 tax law", "Other covariates ≤ .023; contribution terms +.05", "Note (Fact 7)" ),
           @( "Revenue floor (`$50k real vs. none)", "Median |Δr| .018; no median moves more than .04", "One sentence" ),
           @( "Multi-state metros dropped", "Median |Δr| .009–.015; spending endowment about halves", "Report the endowment both ways" ) )
Tbl $s $sens 40 84 @( 330, 370, 180 ) 13 | Out-Null
$st = $s.Shapes.Item( "Table" ).Table
for ( $r = 1; $r -le 11; $r++ ) { for ( $c = 2; $c -le 3; $c++ ) { $st.Cell( $r, $c ).Shape.TextFrame.TextRange.ParagraphFormat.Alignment = 1 } }
T $s "None is a fork: each is smaller than winsorizing, itself a fork that changes 13% of verdicts, or confined to one term the paper can state directly." 40 452 880 36 14 $MUTED $false $BODY 1 $true "Takeaway" | Out-Null
Num $s
Notes $s "From tbl-sensitivity in 05-findings.qmd. Outlying-case and multi-state checks: R/notes/83_sample_exclusions.R and R/notes/101_lean_open_checks.R. City size: R/notes/90_size_moderation_screen.R. Interactions: R/paper/99_interaction_screen_v11.R. Religion and mutual benefit: R/notes/56_drop_religion_mutual.R. Concentration: R/paper/95_compete_niche.R. Tax law: R/notes/84_tcja_contrib.R and R/notes/86_sensitivity_tcja_size.R part A."

# 9. scope and gaps -------------------------------------------------------------------------------------
$s = NewSlide $pres
Title $s "Scope and open gaps"
$gaps = @(
  @( "Grassroots are a refit", "Every registered-stock specification was run again with registered non-filers as the outcome. Grassroots-specific forks, such as a series without hospitals and universities, are not covered." ),
  @( "Disaster recovery is a hypothesis", "Whether the risers (New Orleans, Gulfport, Santa Rosa, Chico, Redding) reflect disaster recovery is untested." ),
  @( "Not testable with these data", "Hospital ownership and state contracting behind the regional gap (AHA survey, CMS Provider of Services, Medicaid). Nothing in the paper is causal." ),
  @( "Not run, by design", "Commuting zones, government grants (990 line 1e, missing 2011–2019), crime, the Elazar typology, population weights, all block subsets and the kitchen-sink universe." ) )
for ( $k = 0; $k -lt 4; $k++ ) {
  $col = $k % 2; $row = [math]::Floor( $k / 2 ); $l = 40 + $col * 450; $t = 96 + $row * 200
  Box $s $l $t 430 184 $TINT 5 "Gap card" | Out-Null
  T $s $gaps[ $k ][ 0 ] ( $l + 22 ) ( $t + 20 ) 386 26 19 $DARK $true $HEAD 1 $false "Gap title" | Out-Null
  T $s $gaps[ $k ][ 1 ] ( $l + 22 ) ( $t + 58 ) 386 112 15 $INK $false $BODY 1 $false "Gap text" | Out-Null
}
Num $s
Notes $s "Leave-one-block-out was run; all block subsets were not. The kitchen-sink universe was built but not run."

# 10. closing ---------------------------------------------------------------------------------------------
$s = NewSlide $pres $DARK
T $s "What the multiverse says" 60 40 840 50 34 "FFFFFF" $true $HEAD 1 $false "Title" 3 | Out-Null
$imp = @(
  @( "1", "Argue estimand choices from theory", "Population, identification and unit change 20–42% of verdicts" ),
  @( "2", "Model each population separately", "Filers, grassroots and spending have different robust covariates" ),
  @( "3", "Treat covariate selection as a decision", "Six vs. 13 covariates moves poverty for filers from −.02 to −.17" ),
  @( "4", "Follow the cards together", "The recommended specifications sharpen the totals and the benchmarks" ),
  @( "5", "Report the checks; don't fork them", "Each sensitivity analysis is smaller than winsorizing" ) )
for ( $k = 0; $k -lt 5; $k++ ) {
  $col = $k % 2; $row = [math]::Floor( $k / 2 ); $l = 60 + $col * 430; $t = 124 + $row * 112
  Badge $s $imp[ $k ][ 0 ] $l $t 38 | Out-Null
  T $s $imp[ $k ][ 1 ] ( $l + 54 ) ( $t + 2 ) 350 24 18 "FFFFFF" $true $HEAD 1 $false "Point" | Out-Null
  T $s $imp[ $k ][ 2 ] ( $l + 54 ) ( $t + 30 ) 350 46 14 $LIGHT $false $BODY 1 $false "Point text" | Out-Null
}
T $s "Nothing here is causal: the multiverse shows which design choices move the associations, and by how much." 490 348 380 70 15 $LIGHT $false $BODY 1 $true "Footer" | Out-Null
Notes $s "Five lessons from the findings chapter. The decision cards (04-design-decisions.qmd) give the evidence and reporting rules behind each."

# ---- save and export ------------------------------------------------------------------------------
if ( Test-Path $out ) { Remove-Item $out -Force }
$pres.SaveAs( $out, 24 )
$n = $pres.Slides.Count
if ( $Export ) {
  New-Item -ItemType Directory -Force $Export | Out-Null
  for ( $k = 1; $k -le $n; $k++ ) { $pres.Slides.Item( $k ).Export( ( Join-Path $Export ( "s{0:D2}.png" -f $k ) ), "PNG", 1600, 900 ) }
}
$pres.Close(); $app.Quit()
[System.Runtime.InteropServices.Marshal]::ReleaseComObject( $app ) | Out-Null
"Saved $out ($( (Get-Item $out).Length ) bytes, $n slides)"
