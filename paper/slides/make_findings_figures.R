# Figures for the findings deck (build_findings_deck.ps1).
# Values are transcribed from paper/05-findings.qmd (run v11); each block names its table.
# Same palette as the other decks: blue 2A78D6 (filers), orange EB6834 (grassroots), navy (spending).
# The decision-impact chart (figs/impact.png) comes from make_deck_figures.R.
#   Rscript paper/slides/make_findings_figures.R

suppressPackageStartupMessages({ library(ggplot2) })

here <- function(...) {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  d <- if (length(f)) dirname(normalizePath(f)) else "paper/slides"
  file.path(d, ...)
}
out <- here("figs")
dir.create(out, showWarnings = FALSE)

ink <- "#1F2933"; muted <- "#5F6B7A"; grid <- "#E6EAF0"; gray <- "#8A94A3"
pop_cols <- c(Filers = "#2A78D6", Grassroots = "#EB6834", Spending = "#1D2939")
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
      panel.spacing = unit(1.2, "lines"),
      plot.margin = margin(4, 10, 4, 4)
    )
}
save <- function(p, name, w, h) {
  ggsave(file.path(out, name), p, width = w, height = h, dpi = 250,
         device = ragg::agg_png, bg = "white")
}
r_lab <- function(x) sub("^(-?)0\\.", "\\1.", sub("^0$", "0", format(x, trim = TRUE)))
long3 <- function(d, cols) {
  do.call(rbind, lapply(names(cols), function(p) {
    data.frame(cov = d$cov, pop = p, r = d[[cols[[p]][1]]], sig = d[[cols[[p]][2]]])
  }))
}

# ---- Stability: median r and % significant, all cross-sections (tbl-stability) ---------
st <- data.frame(
  cov = c("% BA", "Pre-1970 endowment", "Poverty rate", "Foundation assets", "Growth kernel",
          "Religious fract.", "S&L govt earnings", "Age SD", "Gini (metro)", "Turnout",
          "% nonwhite", "Medicaid", "% 65+"),
  fr = c(.32, .21, -.17, .11, -.11, .09, .09, .09, .08, .08, -.07, .03, .02),
  fs = c(100, 77, 86, 65, 57, 55, 56, 55, 51, 48, 49, 53, 20),
  gr = c(.15, .19, -.07, .05, -.16, .28, .07, .03, .07, .20, .11, -.02, .03),
  gs = c(78, 87, 45, 38, 82, 100, 48, 12, 41, 98, 72, 34, 20),
  sr = c(.24, .10, -.03, .06, -.10, .04, .05, .10, .08, .02, -.04, .05, -.07),
  ss = c(99, 59, 16, 39, 59, 25, 32, 67, 49, 8, 19, 43, 43)
)
sl <- long3(st, list(Filers = c("fr", "fs"), Grassroots = c("gr", "gs"), Spending = c("sr", "ss")))
sl$cov <- factor(sl$cov, levels = rev(st$cov))
sl$pop <- factor(sl$pop, levels = names(pop_cols))
sl$solid <- sl$sig >= 75
p <- ggplot(sl, aes(r, cov)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(aes(x = 0, xend = r, yend = cov, colour = pop), linewidth = .9, alpha = .45) +
  geom_point(aes(colour = pop, fill = ifelse(solid, as.character(pop), "white")),
             shape = 21, size = 3.4, stroke = 1.2) +
  geom_text(aes(x = .58, label = paste0(sig, "%")), size = 3.5, colour = muted, family = font, hjust = 1) +
  facet_wrap(~pop) +
  scale_colour_manual(values = pop_cols, guide = "none") +
  scale_fill_manual(values = c(pop_cols, white = "white"), guide = "none") +
  scale_x_continuous(limits = c(-.2, .59), breaks = c(-.2, 0, .2), labels = c("-.2", "0", ".2")) +
  labs(x = "Median partial r across cross-sections (right: % significant; filled: 75% or more)", y = NULL) +
  theme_deck(12.5)
save(p, "results_stability.png", 8.6, 5.8)

# ---- Six vs. 13 covariates, default-model cross-sections (tbl-v10-v11) ---------------------
vv <- data.frame(
  cov = rep(c("% BA", "Foundation assets", "Turnout", "Poverty rate", "% nonwhite", "S&L govt earnings"), 2),
  pop = rep(c("Filers", "Spending"), each = 6),
  v10 = c(.33, .19, .16, -.02, -.08, .10, .25, .13, .07, .07, -.03, .08),
  v11 = c(.31, .11, .07, -.17, -.06, .09, .23, .06, .02, -.03, -.04, .05),
  s10 = c(100, 82, 74, 20, 55, 77, 100, 70, 37, 40, 27, 55),
  s11 = c(100, 65, 41, 89, 47, 55, 99, 38, 4, 13, 18, 29)
)
vv$cov <- factor(vv$cov, levels = rev(unique(vv$cov)))
p <- ggplot(vv, aes(y = cov)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(aes(x = v10, xend = v11, yend = cov, colour = pop), linewidth = 1.1,
               arrow = arrow(length = unit(.12, "in"), type = "closed")) +
  geom_point(aes(x = v10), colour = gray, size = 3) +
  geom_text(aes(x = .74, label = paste0(s10, "% → ", s11, "%")), size = 3.6, colour = muted, family = font, hjust = 1) +
  facet_wrap(~pop) +
  scale_colour_manual(values = pop_cols, guide = "none") +
  scale_x_continuous(limits = c(-.2, .75), breaks = c(-.2, 0, .2), labels = c("-.2", "0", ".2")) +
  labs(x = "Median partial r: six covariates (dot) → 13 covariates (arrow); right: % significant", y = NULL) +
  theme_deck(13)
save(p, "results_v10_v11.png", 8.6, 5.6)

# ---- Naive vs. recommended medians (tbl-naive) --------------------------------------------
nv <- data.frame(
  cov = c("Pre-1970 endowment", "% BA", "Foundation assets", "Poverty rate", "S&L govt earnings",
          "% nonwhite", "Age SD", "Medicaid", "Religious fract.", "Gini (metro)", "Growth kernel",
          "% 65+", "Turnout"),
  fn = c(.21, .30, .11, -.16, .08, -.07, .08, .03, .09, .08, -.10, .02, .08),
  fr = c(.34, .33, .25, -.23, .17, -.17, .15, .13, .10, .09, -.05, -.04, .03),
  fs = c(100, 100, 100, 98, 100, 90, 90, 64, 52, 38, 14, 10, 0),
  gn = c(.19, .16, .04, -.06, .06, .11, .03, -.01, .25, .07, -.16, .03, .20),
  gr = c(.21, .14, .13, -.06, .13, .12, .06, .01, .36, .06, -.10, .00, .25),
  gs = c(98, 88, 86, 5, 76, 64, 0, 2, 100, 21, 45, 0, 100),
  sn = c(.10, .22, .06, -.04, .04, -.03, .09, .06, .03, .08, -.08, -.06, .03),
  sr = c(.19, .27, .10, -.02, .11, -.08, .15, .17, .00, .08, .03, -.13, .02),
  ss = c(100, 100, 43, 0, 50, 7, 100, 98, 0, 21, 2, 62, 0)
)
nl <- rbind(
  data.frame(cov = nv$cov, pop = "Filers", naive = nv$fn, rec = nv$fr, sig = nv$fs),
  data.frame(cov = nv$cov, pop = "Grassroots", naive = nv$gn, rec = nv$gr, sig = nv$gs),
  data.frame(cov = nv$cov, pop = "Spending", naive = nv$sn, rec = nv$sr, sig = nv$ss))
nl$cov <- factor(nl$cov, levels = rev(nv$cov))
nl$pop <- factor(nl$pop, levels = names(pop_cols))
p <- ggplot(nl, aes(y = cov)) +
  geom_vline(xintercept = 0, colour = "#B8C2CE") +
  geom_segment(aes(x = naive, xend = rec, yend = cov, colour = pop), linewidth = 1,
               arrow = arrow(length = unit(.1, "in"), type = "closed")) +
  geom_point(aes(x = naive), colour = gray, size = 2.6) +
  geom_text(aes(x = .62, label = paste0(sig, "%"), fontface = ifelse(sig >= 75, "bold", "plain")),
            size = 3.5, colour = muted, family = font, hjust = 1) +
  facet_wrap(~pop) +
  scale_colour_manual(values = pop_cols, guide = "none") +
  scale_x_continuous(limits = c(-.25, .63), breaks = c(-.2, 0, .2), labels = c("-.2", "0", ".2")) +
  labs(x = "Median partial r: naive (dot) → recommended (arrow); right: % significant when recommended", y = NULL) +
  theme_deck(12.5)
save(p, "results_naive_rec.png", 8.6, 5.8)

cat("figures written to", out, "\n")
