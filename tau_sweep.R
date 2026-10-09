# tau_sweep.R -- why the tau_E sweep no longer moves supply levels in v2
#
# Runs the delay sweep at a FIXED parameter vector (the promoted best fit under
# free=core loss=cycle eff=off cost=slade) and varies one structural feature at
# a time. Nothing is refitted. These are mechanism demonstrations, not model
# comparisons -- see CAVEATS at the foot of this file before quoting anything.
#
#   Rscript tau_sweep.R                # all experiments
#   Rscript tau_sweep.R eps            # eps_P sweep only
#   Rscript tau_sweep.R demand         # v1 vs v2 demand form only
#   Rscript tau_sweep.R cost           # cost anchor specifications only
#   Rscript tau_sweep.R c2             # c2 sensitivity at fixed theta
#
# Expects model_v1demand.R (shipped alongside), plus model_v2.R and
# gloser_data_v2.R in PROJ below. model_v1demand.R is model_v2.R with ONE line
# changed -- the dlnQ line, gated behind p$v1demand, defaulting to v2 behaviour.
# Verify with:
#   diff <(sed 's/dmmcm_v1d/dmmcm_v2/' model_v1demand.R) model_v2.R
#
# LSODA writes stiffness chatter to STDOUT (not stderr) for marginal draws;
# those runs are reported as NA. To read the tables cleanly:
#   Rscript tau_sweep.R 2>/dev/null | grep -vE "DLSODA|In above message|such that|step size|issued|precision|TOLSF"

PROJ <- Sys.getenv("DMMCM_PROJ", ".")
WHAT <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "all"

HERE <- normalizePath(dirname(sub("^--file=", "",
          grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
setwd(PROJ)
suppressMessages(source(file.path(HERE, "model_v1demand.R")))
suppressMessages(source("gloser_data_v2.R"))
d <- comparison_data

# ---- the fitted parameter vector -----------------------------------------
# objective 808.15, price NRMSE 34.8%, Theil Um 0.000 / Uc 0.938,
# band-passed amplitude 0.867, phase correlation 0.77.
FIT <- list(c = 8.93968,   phi = 0.682005,  eps_P = 3.99353e-06,
            C0 = 0.347912, c1 = -0.0656737, k_E = 0.353272,
            z_R = 0.0314783, n = 2.19189e-06,
            alpha3 = 20.0644, alpha8 = 105.051,
            gamma = 0.555125, alpha9 = 0.0269607)

TAUS <- c(2, 5, 8, 12, 16)   # tau_E values; 8 is the reference used in the paper
REF  <- 8
YR   <- 60                   # index of 2020 in seq(0, 61, 1) from 1961

mkp <- function(tau_E = 8, eps_P = FIT$eps_P, c2 = 2e-4, c1 = FIT$c1,
                phi = FIT$phi, v1demand = FALSE, eta = NA) {
  p <- reference_parms()
  for (nm in c("c", "C0", "k_E", "z_R", "n", "gamma")) p[[nm]] <- FIT[[nm]]
  p$phi <- phi; p$c1 <- c1; p$c2 <- c2
  p$alpha[3] <- FIT$alpha3; p$alpha[8] <- FIT$alpha8; p$alpha[9] <- FIT$alpha9
  p$eps_P <- eps_P; p$tau_E <- tau_E
  p$v1demand <- v1demand; p$eta <- eta
  p
}

# data_y0() from run_all.R, reproduced so this script stands alone.
# Initial stocks are derived from the 1961 flows; no free parameters added.
mky0 <- function(p) {
  r <- reference_y0(); i1 <- which.min(d$Year)
  g <- function(col) { x <- d[[col]][i1]; if (is.finite(x)) x else NA_real_ }
  keep <- function(new, old, lo = 1e-3, hi = 1e4)
    if (is.finite(new) && new > lo && new < hi) new else old
  s <- r$s
  s[1] <- keep(g("Extraction (Tg/a)") / (p$alpha[1] + p$alpha[4]), s[1])
  s[2] <- keep(g("F2 (Tg/a)")         /  p$alpha[2],               s[2])
  s[3] <- keep(g("Semis (Tg/a)")      / (p$alpha[3] + p$alpha[8]), s[3])
  s[5] <- keep(p$s5_ref * (g("Recycling (Tg/a)") / r$Rmax)^(1 / p$n),
               s[5], lo = 1e-3, hi = 1e3)
  r$s <- s; r
}

run <- function(p, cost = "slade") {
  o <- try(dmmcm_v1d(p, mky0(p), times = seq(0, 61, 1), funGDP = funGDP,
                     cost_spec = cost), silent = TRUE)
  if (inherits(o, "try-error") || nrow(o) < 62) return(NULL)
  o
}

supply <- function(o) if (is.null(o)) NA_real_ else (o$Extraction + o$Recycling)[YR]
price  <- function(o) if (is.null(o)) NA_real_ else o$Price[YR]
demand <- function(o) if (is.null(o)) NA_real_ else o$Demand[YR]

header <- function(title, unit) {
  cat("\n", title, "\n", sep = "")
  cat(sprintf("%-28s", "")); for (t in TAUS) cat(sprintf("%9s", paste0("tau=", t)))
  cat(sprintf("%10s\n", unit))
}

row <- function(lab, f, stat = supply, rel = TRUE) {
  v <- vapply(TAUS, function(t) stat(f(t)), numeric(1))
  b <- v[TAUS == REF]
  cat(sprintf("%-28s", lab))
  for (x in v) cat(if (rel) sprintf("%9.3f", x / b) else sprintf("%9.2f", x))
  if (rel) {
    sp <- 100 * (max(v, na.rm = TRUE) - min(v, na.rm = TRUE)) / b
    nf <- sum(!is.finite(v))
    cat(sprintf("%9.1f%s\n", sp, if (nf) sprintf(" (%d NA)", nf) else ""))
  } else cat("\n")
  invisible(v)
}

# ---- 1. price elasticity of demand ---------------------------------------
if (WHAT %in% c("all", "eps")) {
  header("SUPPLY (E+R) 2020, relative to tau_E = 8 -- varying eps_P", "spread%")
  for (ep in c(FIT$eps_P, 0.1, 0.2, 0.4, 0.8))
    row(sprintf("eps_P = %.3g", ep), function(t) run(mkp(t, eps_P = ep)))
  cat("\n  Raising eps_P makes supply CONVERGE, not diverge. The elasticity form\n",
      " is a level relation, so a delay-induced price difference produces a\n",
      " bounded proportional shift in demand that cannot accumulate.\n", sep = "")
}

# ---- 2. demand functional form -------------------------------------------
if (WHAT %in% c("all", "demand")) {
  header("SUPPLY (E+R) 2020, relative to tau_E = 8 -- demand form", "spread%")
  row("v2 elasticity (fitted)", function(t) run(mkp(t)))
  for (e in c(1, 2, 4))
    row(sprintf("v1 form, eta = %.1f", e),
        function(t) run(mkp(t, v1demand = TRUE, eta = e)))
  header("Same rows, ABSOLUTE supply 2020 (Tg/a)", "")
  row("v2 elasticity (fitted)", function(t) run(mkp(t)), rel = FALSE)
  row("v1 form, eta = 2.0", function(t) run(mkp(t, v1demand = TRUE, eta = 2)),
      rel = FALSE)
  cat("\n  The v1 form puts the price in the demand GROWTH RATE, so the paths\n",
      " integrate apart. That, not the size of the elasticity, is what makes\n",
      " the published sweep show a permanent level effect.\n", sep = "")
}

# ---- 3. cost anchor specification ----------------------------------------
if (WHAT %in% c("all", "cost")) {
  header("SUPPLY (E+R) 2020, relative to tau_E = 8 -- cost anchor", "spread%")
  for (cs in c("slade", "constant", "endogenous"))
    row(sprintf("cost = %s", cs), function(t) run(mkp(t), cost = cs))
  row("phi = 0 (anchor removed)", function(t) run(mkp(t, phi = 0)))
  cat("\n  A null result worth reporting: the delay conclusions are invariant to\n",
      " the cost anchor. phi = 0 fails to integrate at this theta, so the\n",
      " no-anchor limit could not be evaluated.\n", sep = "")
}

# ---- 4. c2 sensitivity ---------------------------------------------------
if (WHAT %in% c("all", "c2")) {
  obs <- d[["Real Prices (World Bank) $/Tg billion"]]
  ok  <- !is.na(obs)
  nrmse <- function(o) if (is.null(o)) NA_real_ else
    sqrt(mean((obs[ok] - o$Price[ok])^2)) / diff(range(obs[ok]))
  cat("\nPrice NRMSE at the fitted theta, varying c2 only (no refit)\n")
  cat("  Slade-implied c2 for copper is 3.3e-05 to 6.2e-05; 2e-4 is the placeholder.\n\n")
  for (c2 in c(0, 3.3e-5, 6.2e-5, 1e-4, 2e-4, 4e-4)) {
    p <- mkp(c2 = c2); o <- run(p)
    if (is.null(o)) { cat(sprintf("  c2 = %-9.3g  solver failure\n", c2)); next }
    C <- p$C0 * exp(p$c1 * o$time + p$c2 * o$time^2)
    cat(sprintf("  c2 = %-9.3g  NRMSE = %6.2f%%   C(2022) = %8.5f\n",
                c2, 100 * nrmse(o), C[62]))
  }
}

# ---- CAVEATS -------------------------------------------------------------
# 1. Every run uses the v2-fitted theta. The v1-demand rows were NEVER fitted
#    under that demand form, and eta = 1, 2, 4 are arbitrary. These rows show a
#    MECHANISM; they are not a model comparison and must not be reported as one.
# 2. Several tau_E = 16 runs fail to integrate and are dropped. Spreads on those
#    rows rest on four points, and the count is printed.
# 3. NRMSE here is computed on the raw price series with no burn-in exclusion
#    and differs from the figure quoted for the fit (34.8%), which comes from
#    run_all.R's own reporting path. Use it for RELATIVE comparison across rows
#    only.
# 4. c2 is FIXED in the fit (KEEP for cost=slade adds only c1). Row 4 varies it
#    without refitting, so it bounds the effect from below: a refit would let
#    c1, C0 and phi absorb part of the change. Note that price NRMSE at this
#    theta happens to be near-minimised at c2 = 2e-4 (23.3%, against 25.0% at
#    c2 = 0 and 24.5% at 4e-4). That is a coincidence, not a justification --
#    the placeholder was never chosen by that criterion -- but it does mean a
#    refit with c2 free is unlikely to move it far.
