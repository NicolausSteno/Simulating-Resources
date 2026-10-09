# residual_qq.R -- normality and outlier diagnostics for the fitted model.
# Complements the existing ACF figure: ACF tests independence, this tests the
# Gaussian shape the objective assumes, and quantifies how much of the price
# NRMSE is carried by a handful of years.
#
# Run from the directory holding the fit outputs:
#   Rscript residual_qq.R

TAG <- "__slade_cycle_drop_core"

OBS_ALL <- list(
  Price      = "Real Prices (World Bank) $/Tg billion",
  Extraction = "Extraction (Tg/a)",
  f2         = "F2 (Tg/a)",
  s4         = "S4 (Tg)",
  s8         = "S8 (Tg)",
  f9         = "F9 (Tg/a)",
  semis      = "Semis (Tg/a)",
  f3         = "F3 (Tg/a)",
  f13        = "F13 (Tg/a)",
  f14        = "F14 (Tg/a)")

## Take the data exactly the way run_all.R does (line 244), NOT from the CSV
## export in the directory -- that file carries different column names.
source("gloser_data_v2.R")
d <- comparison_data
o <- read.csv(sprintf("fitted_trajectory%s.csv", TAG), check.names = FALSE)

stopifnot(nrow(d) == nrow(o))      # o$time 0..61 aligns row-wise with d$Year
miss_d <- setdiff(unlist(OBS_ALL), names(d))
miss_o <- setdiff(names(OBS_ALL),  names(o))
if (length(miss_d) || length(miss_o))
  stop("column mismatch.\n  missing in data: ", paste(miss_d, collapse = " | "),
       "\n  missing in trajectory: ", paste(miss_o, collapse = " | "),
       "\n  data has: ", paste(names(d), collapse = " | "))

resid_of <- function(k) {
  col <- OBS_ALL[[k]]; ok <- !is.na(d[[col]])
  list(ok = ok, yr = d$Year[ok], obs = d[[col]][ok],
       fit = o[[k]][ok], r = d[[col]][ok] - o[[k]][ok])
}

## ---- figure 1: Q-Q of residuals, one panel per series ---------------
png(sprintf("resid_qq%s.png", TAG), width = 1750, height = 760, res = 130)
par(mfrow = c(2, 5), mar = c(4.0, 4.0, 2.6, 0.8))
for (k in names(OBS_ALL)) {
  e <- resid_of(k)
  z <- e$r / sqrt(mean(e$r^2))         # scaled by RMSE: this tests SHAPE
  qqnorm(z, main = k, pch = 16, cex = .6, col = "firebrick",
         xlab = "theoretical quantile", ylab = "residual / RMSE")
  qqline(z, col = "grey40", lty = 2)
  if (length(z) >= 3 && length(z) <= 5000) {
    sw <- suppressWarnings(shapiro.test(z)$p.value)
    mtext(sprintf("Shapiro-Wilk p = %.3f", sw), side = 3, line = -1.2,
          cex = .62, col = if (sw < .05) "firebrick" else "grey35")
  }
  if (k == "Price") {                  # label the worst years
    w <- order(abs(z), decreasing = TRUE)[1:3]
    q <- qqnorm(z, plot.it = FALSE)
    text(q$x[w], q$y[w], e$yr[w], pos = 4, cex = .68, col = "grey20")
  }
}
invisible(dev.off())

## ---- figure 2: residual against fitted -------------------------------
png(sprintf("resid_vs_fitted%s.png", TAG), width = 1750, height = 760, res = 130)
par(mfrow = c(2, 5), mar = c(4.0, 4.0, 2.6, 0.8))
for (k in names(OBS_ALL)) {
  e <- resid_of(k)
  plot(e$fit, e$r, pch = 16, cex = .6, col = "firebrick", main = k,
       xlab = "fitted", ylab = "residual")
  abline(h = 0, col = "grey40", lty = 2)
  lines(lowess(e$fit, e$r), col = "steelblue", lwd = 1.6)
}
invisible(dev.off())

## ---- how much of the price NRMSE is a handful of years? --------------
e <- resid_of("Price")
nrmse <- function(res, obs) sqrt(mean(res^2)) / mean(abs(obs))   # as in metrics()
cat(sprintf("\nPrice NRMSE, all %d points        : %.1f%%\n",
            length(e$r), 100 * nrmse(e$r, e$obs)))
w <- order(abs(e$r), decreasing = TRUE)
for (n in 1:3) {
  drop <- w[1:n]
  cat(sprintf("  excluding %d worst year%s (%s): %.1f%%\n", n,
              if (n > 1) "s" else " ", paste(e$yr[drop], collapse = ", "),
              100 * nrmse(e$r[-drop], e$obs[-drop])))
}
cat("\nIf the NRMSE falls sharply here, say so in the text: a Gaussian loss with\n",
    "heavy-tailed residuals is partly hostage to events the model does not\n",
    "attempt to reproduce, and that is a cleaner claim than an unexplained 35%.\n",
    sep = "")

cat(sprintf("\nwrote resid_qq%s.png, resid_vs_fitted%s.png\n", TAG, TAG))
