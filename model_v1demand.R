# =====================================================================
# model_v2.R  --  revised DMMCM
# =====================================================================
# Changes from the published model, in order of expected impact on the fit:
#
# 1. RETARDED, not neutral. lagderiv() is gone. Because dP/dt is given
#    explicitly, the price growth rate is an ALGEBRAIC function of the state:
#        g_P = c*(Q - E - R)/Q + phi*log(C/P)
#    so g_P(t-tau) is evaluated from lagvalue() of (P,Q,E,R) alone. Exact
#    rewrite. Discontinuities now smooth out as they propagate instead of
#    persisting forever, which is what made the loss surface noisy.
#
# 2. RECYCLING CONSTRAINT ENFORCED ALGEBRAICALLY. We integrate Rmax and set
#        R = Rmax * (s5/s5_ref)^n
#    at every step, instead of integrating the differentiated form. In the
#    published Eq. (32) that relation is only an invariant, and nothing stops
#    the solution drifting off it - which is how s5 went negative and s5^n
#    returned NaN. Removes g_s5, the 1/s5 division and the s5^(n-1) term.
#    alpha_R is absorbed into s5_ref, so the parameter q disappears.
#
#    NOTE ON THE PAPER'S CLAIM: mass conservation is structural here (G's
#    columns sum to zero and b sums to E), so it was never at risk. What
#    Eq. (28) protects is the NON-NEGATIVITY of s5.
#
# 3. COST ANCHOR replaces the additive drift delta. At Q = E + R the published
#    model needs (Q-S)/Q = -delta/(cP), i.e. a price-stationary state requiring
#    ~8% permanent excess supply at P=4 but ~17% at P=2 - an artefact, and the
#    source of a flat c-delta valley. Here price mean-reverts to a cost anchor
#    C(t), which is what "producers will not persistently sell below cost"
#    actually implies. Note the 1/P does NOT cancel here (unlike a purely
#    multiplicative delta, which would leave the price LEVEL unidentifiable).
#
# 4. DEMAND IN ELASTICITY FORM. d(lnQ)/dt = eps_Y*r(t) - eps_P*g_P integrates
#    exactly to Q = Q0*(Y/Y0)^eps_Y*(P/P0)^-eps_P, the standard constant-
#    elasticity demand function. The published (eta/P)*r(t)*Q form switches the
#    price channel off when r=0 and REVERSES it when r<0, so in 2009 and 2020
#    high prices made demand contract less.
#
# 5. ALL STATES INTEGRATED IN LOGS. Positivity is structural, and g_s5 and g_P
#    become state derivatives rather than quotients.
#
# 6. SWITCHES SMOOTHED with width eps_sw, annealable. Set eps_sw -> 0 to
#    recover the hard switch.
# =====================================================================

library(deSolve)

# ---------------------------------------------------------------------
# Cost anchor C(t).  Three defensible specifications; the choice is a model
# comparison result, not something you have to justify a priori.
# ---------------------------------------------------------------------
make_cost_anchor <- function(spec = c("slade", "constant", "endogenous"), p) {
  spec <- match.arg(spec)
  switch(spec,
    # Null baseline: flat real long-run cost.
    constant = function(t, X) p$C0,

    # Slade (1982): log-quadratic real cost path. Technology pushes cost down,
    # depletion pushes it up, giving the U-shape she found for 11 minerals.
    # c1 < 0 and c2 > 0 reproduce that; both are free parameters.
    slade = function(t, X) p$C0 * exp(p$c1 * t + p$c2 * t^2),

    # Endogenous depletion: cost rises with cumulative extraction X (state 13).
    # theta is the elasticity of cost w.r.t. cumulative production. Closest to
    # the depletion argument in the paper, at the price of one extra parameter.
    endogenous = function(t, X) p$C0 * exp(p$theta * (X / p$X_ref - 1))
  )
}

# ---------------------------------------------------------------------
# Structure: hierarchy matrix and the flow parametrisation f = U s.
# alpha_13 and alpha_14 stay zero - recycling enters through b.
# ---------------------------------------------------------------------
build_structure <- function() {
  G <- matrix(0, nrow = 8, ncol = 14)
  edges <- list(c(1,2,1), c(2,3,2), c(3,2,3), c(1,6,4), c(6,1,5), c(2,7,6),
                c(7,2,7), c(3,4,8), c(4,5,9), c(4,5,10), c(5,8,11),
                c(8,5,12), c(5,3,13), c(5,2,14))
  for (e in edges) { G[e[1], e[3]] <- -1; G[e[2], e[3]] <- 1 }
  G
}

make_U <- function(alpha) {
  U <- matrix(0, nrow = 14, ncol = 8)
  src <- c(1,2,3,1,6,2,7,3,4,4,5,8,5,5)   # source stock of each flow
  for (j in 1:14) U[j, src[j]] <- alpha[j]
  U
}

# ---------------------------------------------------------------------
# Right-hand side
# ---------------------------------------------------------------------
rhs_v2 <- function(t, y, p) {

  P <- exp(y[1]); Q <- exp(y[2]); E <- exp(y[3]); Rmax <- exp(y[4])
  s <- exp(y[5:12]); X <- y[13]

  Cfun <- p$Cfun
  smooth <- function(g) if (p$eps_sw <= 0) as.numeric(g > 0) else
                        1 / (1 + exp(-g / p$eps_sw))

  # scrap-availability limiter.  Two options, both vanishing at s5 = 0.
  lim <- function(s5) {
    if (identical(p$limiter, "saturating")) s5 / (p$K + s5)   # bounded, state-dependent elasticity
    else (s5 / p$s5_ref)^p$n                                  # constant-elasticity power law
  }

  R <- Rmax * lim(s[5])

  # --- price growth rate as an algebraic function of the state --------------
  gP <- function(P_, Q_, E_, R_, t_, X_)
    p$c * (Q_ - (E_ + R_)) / Q_ + p$phi * log(Cfun(t_, X_) / P_)

  g_now <- gP(P, Q, E, R, t, X)

  lagged_g <- function(tau) {
    if (t <= tau) return(p$g_hist)          # pre-1961 growth rate (default 0)
    L  <- lagvalue(t - tau, c(1, 2, 3, 4, 9, 13))
    Pl <- exp(L[1]); Ql <- exp(L[2]); El <- exp(L[3])
    Rl <- exp(L[4]) * lim(exp(L[5]))
    gP(Pl, Ql, El, Rl, t - tau, L[6])
  }
  gE <- lagged_g(p$tau_E)
  gR <- lagged_g(p$tau_R)

  # --- market block ---------------------------------------------------------
  dlnP <- g_now
  dlnQ <- if (isTRUE(p$v1demand)) p$eta * p$funGDP(t) / P else p$eps_Y * p$funGDP(t) - p$eps_P * g_now
  dQ   <- Q * dlnQ

  wE <- smooth(gE)
  dlnE <- wE * p$k_E * gE + (1 - wE) * p$k_E * p$z_E * dQ / E

  wR <- smooth(gR)
  dlnRmax <- wR * p$k_R * gR + (1 - wR) * p$k_R * p$z_R * dQ / Rmax

  # --- material cycle -------------------------------------------------------
  f <- p$U %*% s
  b <- c(E, (1 - p$gamma) * R, p$gamma * R, 0, -R, 0, 0, 0)
  d <- p$lambda * s
  ds <- as.vector(b + p$G %*% f - d)

  # state 14 accumulates dissipative loss, making the Gloser Fig.5 cumulative
  # band a fittable observable instead of an assumption about lambda.
  list(c(dlnP, dlnQ, dlnE, dlnRmax, ds / s, E, sum(d)))
}

# ---------------------------------------------------------------------
# Solver wrapper
# ---------------------------------------------------------------------
dmmcm_v1d <- function(parms, y0, times = seq(0, 61, 1), funGDP,
                     cost_spec = "slade", limiter = "power",
                     eps_sw = 0.02, atol = 1e-8, rtol = 1e-8,
                     maxsteps = 5000) {

  p <- as.list(parms)
  p$funGDP <- funGDP; p$eps_sw <- eps_sw; p$limiter <- limiter
  p$G <- build_structure()
  p$U <- make_U(p$alpha)
  p$Cfun <- make_cost_anchor(cost_spec, p)
  if (is.null(p$g_hist)) p$g_hist <- 0

  yinit <- c(log(y0$P), log(y0$Q), log(y0$E), log(y0$Rmax), log(y0$s), 0, 0)
  names(yinit) <- c("lnP","lnQ","lnE","lnRmax", paste0("ln_s",1:8), "X", "Dcum")

  out <- dede(y = yinit, times = times, func = rhs_v2, parms = p,
              atol = atol, rtol = rtol, maxsteps = maxsteps,
              control = list(mxhist = 1e6))

  o <- as.data.frame(out)
  res <- data.frame(
    time = o$time,
    Price = exp(o$lnP), Demand = exp(o$lnQ),
    Extraction = exp(o$lnE), Rmax = exp(o$lnRmax), X = o$X, Dcum = o$Dcum)
  for (i in 1:8) res[[paste0("s", i)]] <- exp(o[[paste0("ln_s", i)]])
  res$Recycling <- res$Rmax * if (limiter == "saturating")
      res$s5 / (p$K + res$s5) else (res$s5 / p$s5_ref)^p$n
  res$f2  <- p$alpha[2]  * res$s2
  res$f9  <- p$alpha[9]  * res$s4
  res$f3  <- p$alpha[3]  * res$s3
  res$f13 <- p$gamma       * res$Recycling
  res$f14 <- (1 - p$gamma) * res$Recycling
  res$f8  <- p$alpha[8] * res$s3
  # mass balance at manufacturing: fabrication output splits into product
  # entering use (f8) and new scrap returned to metallurgy (f3).
  res$semis <- res$f8 + res$f3
  res
}

# ---------------------------------------------------------------------
# Reference parameters. alpha_9 is set back to the INVERSE-MODELLED 0.02,
# not the inflated 0.049: that inflation was compensating for a recycling
# target ~1.85x too high, so it should not survive the data correction.
#
# lambda is no longer a flat 0.01/a. Gloser's Table S5 dissipation figures are
# fractions of a DISCARD FLOW, not per-annum leach rates on a stock, and their
# Fig. 5 puts cumulative dissipation + abandonment at only ~28 Tg over a
# century. A flat 0.01/a on ~700 Tg of stock gives ~7 Tg/a in 2010 alone.
# Process losses to tailings and slag are already explicit flows (f4, f6), so
# a leach term on s1/s2 would double-count them.
# ---------------------------------------------------------------------
reference_parms <- function() list(
  alpha  = c(9.60, 11.20, 0.60, 1.30, 0.006, 0.30, 0, 11.80,
             0.02, 0, 1.80, 0, 0, 0),
  lambda = c(0, 0, 0, 3e-4, 0, 1e-4, 1e-4, 1e-4),
  gamma  = 0.44,        # data give 0.44 mean (0.29 -> 0.43); paper used 1/3
  c = 1.6, phi = 0.15, C0 = 3.5, c1 = -0.01, c2 = 2e-4,
  theta = 0.3, X_ref = 500,
  eps_Y = 1.1, eps_P = 0.2,          # literature priors for copper
  k_E = 0.60, z_E = 0.85,
  k_R = 0.55, z_R = 0.85,
  n = 0.5, s5_ref = 1.0, K = 1.0,
  tau_E = 8, tau_R = 2, g_hist = 0
)

reference_y0 <- function() list(
  P = 3.24, Q = 5.2, E = 3.979, Rmax = 0.92,
  s = c(1, 1, 1, 72.4, 1, 50, 20, 39.5)   # s4, s8 now from the rebuilt data
)
