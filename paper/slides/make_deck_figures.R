# Figures for the design-decisions deck (build_design_decisions_deck.ps1).
# Values are transcribed from paper/04-design-decisions.qmd (run v11, 13-covariate
# model); each block names the table it comes from. Re-run after the chapter changes.
# Palette matches the stylized-facts deck: blue 2A78D6 (filers), orange EB6834 (grassroots).
#   Rscript paper/slides/make_deck_figures.R

suppressPackageStartupMessages({ library(ggplot2) })

here <- function(...) {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  d <- if (length(f)) dirname(normalizePath(f)) else "paper/slides"
  file.path(d, ...)
}
out <- here("figs")
dir.create(out, showWarnings = FALSE)

ink <- "#1F2933"; muted <- "#5F6B7A"; grid <- "#E6EAF0"; tint <- "#F1F4F8"
blue <- "#2A78D6"; orange <- "#EB6834"; light_orange <- "#F4B08F"; gray <- "#8A94A3"
pop_cols <- c(Filers = blue, Grassroots = orange, Spending = "#1D2939")
font <- "Arial"

theme_deck <- function(base = 13) {
  theme_minimal(base_size = base, base_family = font) +
    theme(
      text = element_text(colour = ink),
      axis.text = element_text(colour = muted),
      axis.title = element_text(colour = muted, size = base - 1),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(colour = grid, linewidth = .5),
      legend.position = "top", legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(colour = ink, size = base - 1),
      strip.text = element_text(colour = ink, face = "bold", hjust = 0, size = base),
      plot.margin = margin(4, 10, 4, 4)
    )
}
save <- function(p, name, w, h) {
  ggsave(file.path(out, name), p, width = w, height = h, dpi = 250,
         device = ragg::agg_png, bg = "white")
}
r_lab <- function(x) sub("^(-?)0", "\\1", sprintf("%.1f", x))

# ---- Impact: verdicts changed by decision (tbl-impact-summary) ---------------
imp <- data.frame(
  decision = c("Identification (D3)", "Measure, own swap (D7)", "Population (D1)",
               "Predictor form (D6)", "Unit (D2)", "DV form (D4, D5)", "Estimator (D9)",
               "log P on the right (D6)", "Endowment in or out (D8)", "Winsorizing",
               "Growth kernel in or out (D8)", "Year (2022 vs. 2019)",
               "Hospitals & universities (D10)", "Money measure (D10)"),
  Filers     = c(38, 44, 41, 32, 26, 23, 19, 17, 13, 13, 10,  9,  2, NA),
  Grassroots = c(42, 44, 41, 22, 20, 16, 14, 13, 11, 12, 11, 13, NA, NA),
  Spending   = c(29, 27, 37, 26, 22, 27, 20, 14, 10, 14, 10, 14, 23,  5)
)
imp$decision <- factor(imp$decision, levels = rev(imp$decision))
il <- reshape(imp, direction = "long", varying = names(pop_cols), v.names = "pct",
              timevar = "pop", times = names(pop_cols), idvar = "decision")
il <- il[!is.na(il$pct), ]
rng <- aggregate(pct ~ decision, il, function(x) c(min(x), max(x)))
rng <- data.frame(decision = rng$decision, lo = rng$pct[, 1], hi = rng$pct[, 2])
il$y <- as.numeric(il$decision) + c(Filers = .2, Grassroots = 0, Spending = -.2)[il$pop]
p <- ggplot(il, aes(pct, y)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 10.5, ymax = 14.5, fill = "#FDF0EA") +
  geom_segment(data = rng, aes(x = lo, xend = hi, y = as.numeric(decision), yend = as.numeric(decision)),
               colour = "#D5DCE5", linewidth = 1.8) +
  geom_point(aes(colour = pop), size = 3.4) +
  scale_y_continuous(breaks = seq_along(levels(imp$decision)), labels = levels(imp$decision),
                     expand = expansion(add = .5)) +
  scale_colour_manual(values = pop_cols) +
  scale_x_continuous(limits = c(0, 46), breaks = seq(0, 40, 10), labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, .02))) +
  labs(x = "Covariate verdicts changed when only this decision changes", y = NULL) +
  theme_deck(13)
save(p, "impact.png", 7.8, 5.6)

# ---- D1: partial r by population (tbl-d1) -------------------------------------
d1 <- data.frame(
  cov = c("% BA", "Foundation assets", "% nonwhite", "Turnout", "Poverty rate",
          "S&L govt earnings", "Medicaid", "Growth kernel", "Pre-1970 endowment",
          "Gini (metro)", "Religious fract.", "Age SD", "% 65+"),
  Filers     = c(.37, .21, -.20, .01, -.29, .23, .24, -.05, .32, .13, .12, .15, -.04),
  Grassroots = c(.14, .12,  .13, .24, -.06, .08, .00, -.08, .26, .15, .51, .07, -.01),
  Spending   = c(.26, .11, -.08, .04, -.04, .10, .18, -.05, .16, .08, -.01, .13, -.06)
)
d1$cov <- factor(d1$cov, levels = d1$cov[order(d1$Grassroots - d1$Filers)])
dl <- reshape(d1, direction = "long", varying = names(pop_cols), v.names = "r",
              timevar = "pop", times = names(pop_cols), idvar = "cov")
seg <- data.frame(cov = d1$cov, lo = pmin(d1$Filers, d1$Grassroots), hi = pmax(d1$Filers, d1$Grassroots))
p <- ggplot(dl, aes(r, cov)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(data = seg, aes(x = lo, xend = hi, yend = cov), colour = "#D5DCE5", linewidth = 1.8) +
  geom_point(aes(colour = pop, shape = pop), size = 3.4, stroke = 1.1) +
  scale_colour_manual(values = pop_cols) + scale_shape_manual(values = c(16, 16, 1)) +
  scale_x_continuous(limits = c(-.32, .54), breaks = seq(-.2, .5, .1), labels = r_lab) +
  labs(x = "Partial r, 13-covariate log-log model, 2022", y = NULL) +
  theme_deck(13)
save(p, "d1_populations.png", 6.4, 5.4)

# ---- D3: cross-section -> within metros (tbl-d3) ------------------------------
d3 <- data.frame(
  cov = rep(c("% BA", "Poverty rate", "% nonwhite", "Turnout", "Religious fract.",
              "S&L govt earnings", "Age SD", "% 65+", "Growth kernel"), 2),
  pop = rep(c("Filers", "Grassroots"), each = 9),
  cs  = c(.31, -.17, -.06, .07, .09, .09, .08, .02, -.09,
          .15, -.07,  .12, .20, .28, .07, .02, .03, -.15),
  wi  = c(.08, -.02, -.10, .07, -.02, -.01, -.08, .04, -.15,
          .14,  .09, -.09, .10, .10, -.13, .13, -.14, -.20)
)
d3$cov <- factor(d3$cov, levels = rev(unique(d3$cov)))
p <- ggplot(d3, aes(y = cov)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(aes(x = cs, xend = wi, yend = cov, colour = pop), linewidth = 1,
               arrow = arrow(length = unit(.11, "in"), type = "closed")) +
  geom_point(aes(x = cs), colour = gray, size = 2.8) +
  facet_wrap(~pop) +
  scale_colour_manual(values = pop_cols, guide = "none") +
  scale_x_continuous(limits = c(-.24, .34), breaks = c(-.2, 0, .2), labels = c("-.2", "0", ".2")) +
  labs(x = "Median partial r: cross-section (dot) → within metros (arrow)", y = NULL) +
  theme_deck(13) + theme(panel.spacing = unit(1.4, "lines"))
save(p, "d3_within.png", 6.4, 5.4)

# ---- D5: scaling exponent by estimator and share of zeros (tbl-d5) -------------
d5 <- data.frame(
  grp = c("Full sector (2%)", "Human services (6%)", "Health (25%)", "Arts (30%)",
          "Hospitals (57%)", "Universities (72%)", "Mutual/membership (90%)"),
  `OLS, zeros dropped` = c(.98, .93, .77, .76, .35, .39, .19),
  `OLS, log(n + 1)`    = c(.93, .86, .68, .67, .27, .25, .10),
  Poisson              = c(1.00, .95, .99, 1.01, .81, 1.00, 1.06), check.names = FALSE
)
d5$grp <- factor(d5$grp, levels = d5$grp)
d5l <- reshape(d5, direction = "long", varying = names(d5)[-1], v.names = "beta",
               timevar = "est", times = names(d5)[-1], idvar = "grp")
d5l$est <- factor(d5l$est, levels = names(d5)[-1])
p <- ggplot(d5l, aes(grp, beta, colour = est, group = est)) +
  geom_hline(yintercept = 1, colour = "#B8C2CE", linetype = 2) +
  geom_line(linewidth = 1.1) + geom_point(size = 3.2) +
  scale_colour_manual(values = c(orange, light_orange, blue)) +
  scale_y_continuous(limits = c(0, 1.12), breaks = seq(0, 1, .25)) +
  scale_x_discrete(labels = function(x) sub(" \\(", "\n(", x)) +
  labs(x = "NCCS group (share of counties with no filer)", y = "County scaling exponent") +
  theme_deck(14) + theme(panel.grid.major.y = element_line(colour = grid, linewidth = .5),
                         panel.grid.major.x = element_blank(),
                         axis.text.x = element_text(lineheight = .9))
save(p, "d5_zeros.png", 12.2, 4.0)

# ---- D7: measurement swaps, eta^2 on the construct's own term -----------------
d7 <- data.frame(
  swap = c("Turnout → census mail response", "Poverty → unemployment",
           "S&L earnings → S&L jobs\nor direct expenditure", "% nonwhite → racial fract.\nor foreign-born",
           "Foundation assets → four\nphilanthropy measures", "Growth kernel: 10- vs\n5-year half-life"),
  Filers = c(.80, .38, .11, .18, .14, .00), Grassroots = c(.92, .18, .32, .26, .14, .00)
)
d7$swap <- factor(d7$swap, levels = rev(d7$swap))
d7l <- reshape(d7, direction = "long", varying = c("Filers", "Grassroots"), v.names = "eta",
               timevar = "pop", times = c("Filers", "Grassroots"), idvar = "swap")
d7l$pop <- factor(d7l$pop, levels = c("Grassroots", "Filers"))
p <- ggplot(d7l, aes(eta, swap, fill = pop)) +
  geom_col(position = position_dodge(width = .78), width = .72) +
  geom_text(aes(label = sub("^0", "", sprintf("%.2f", eta))), position = position_dodge(width = .78),
            hjust = -.15, size = 4.2, family = font, colour = ink) +
  scale_fill_manual(values = pop_cols[1:2], breaks = c("Filers", "Grassroots")) +
  scale_x_continuous(limits = c(0, 1.05), breaks = seq(0, 1, .25), labels = c("0", ".25", ".50", ".75", "1"),
                     expand = expansion(mult = c(0, 0))) +
  labs(x = expression(eta^2 ~ "on the construct's own partial r"), y = NULL) +
  theme_deck(12.5) + theme(axis.text.y = element_text(lineheight = .9))
save(p, "d7_swaps.png", 6.4, 5.4)

# ---- D9: OLS vs state FE, filers, sorted by between-state share ---------------
d9 <- data.frame(
  cov = c("Turnout", "% nonwhite", "Religious fract.", "Growth kernel", "Medicaid",
          "Pre-1970 endowment", "Poverty rate", "S&L govt earnings", "% BA", "Foundation assets"),
  share = c(.65, .64, .58, .52, .50, .46, .39, .31, .21, .19),
  ols = c(.01, -.20, .12, -.05, .24, .32, -.29, .23, .37, .21),
  sfe = c(.09, -.20, .00, -.04, .06, .34, -.18, .15, .26, .24)
)
d9$lab <- sprintf("%s  (%d%%)", d9$cov, round(100 * d9$share))
d9$lab <- factor(d9$lab, levels = rev(d9$lab))
p <- ggplot(d9, aes(y = lab)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(aes(x = ols, xend = sfe, yend = lab), colour = blue, linewidth = 1,
               arrow = arrow(length = unit(.11, "in"), type = "closed")) +
  geom_point(aes(x = ols), colour = gray, size = 3) +
  scale_x_continuous(limits = c(-.32, .4), breaks = seq(-.3, .3, .1), labels = r_lab) +
  labs(x = "Filers, partial r: OLS (dot) → state FE (arrow)",
       y = "Covariate (share of variance between states)") +
  theme_deck(13)
save(p, "d9_statefe.png", 6.4, 5.4)

# ---- D11: subsectors sharing the full-sector sign (of 11) ---------------------
d11 <- data.frame(
  cov = c("Medicaid", "Pre-1970 endowment", "% BA", "Poverty rate", "Foundation assets",
          "% nonwhite", "S&L govt earnings", "Turnout"),
  n = c(11, 11, 10, 10, 9, 8, 8, 5)
)
d11$cov <- factor(d11$cov, levels = rev(d11$cov))
p <- ggplot(d11, aes(n, cov)) +
  geom_col(fill = blue, width = .66) +
  geom_text(aes(label = paste(n, "of 11")), hjust = 1.15, colour = "white", size = 4.4, family = font) +
  scale_x_continuous(limits = c(0, 11), breaks = c(0, 5, 11), expand = expansion(mult = c(0, .01))) +
  labs(x = "Subsectors whose sign matches the full sector", y = NULL) +
  theme_deck(13)
save(p, "d11_subsectors.png", 6.4, 5.0)

cat("figures written to", out, "\n")
