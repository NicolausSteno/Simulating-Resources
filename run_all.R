# =====================================================================
# run_all.R  (v3)  --  everything in one file. Pure R, only deSolve needed.
#
# HOW TO RUN
# ----------
#   1. Install R, then in R:  install.packages("deSolve")
#   2. Put these four files in one folder:
#        run_all.R  model_v2.R  gloser_data_v2.R  paper1_plotting.R
#   3. Open a terminal in that folder and run ONE of:
#
#        Rscript run_all.R diag                 # ~3 min   sensitivity + collinearity
#        Rscript run_all.R recover maxit=auto   # recovery test, runs to convergence
#        Rscript run_all.R fit     maxit=auto   # fit to real data, runs to convergence
#        Rscript run_all.R plot                 # ~1 min   figures + metrics
#
# ARGUMENTS are key=value and may appear in any order after the mode. The old
# positional form (mode maxit loss cap) still works.
#
#   ---- v4 additions ----
#   free=full|core      core drops k_R, z_E, lam4 -- see the block at KEEP.
#                       15 -> 12 parameters. Nelder-Mead degrades badly above
#                       ~10 dimensions and that, not identifiability, is what
#                       the failed noise=0.001 recovery was measuring.
#   starts=N            NOW A REAL MULTISTART: optimises from each candidate
#                       and reports the spread of FINAL objectives. v3
#                       evaluated the objective at N points and started from
#                       the best, which is start selection, not multistart.
#   stage1=600          NM iterations per multistart candidate
#   global=none|de      optional DEoptim pre-stage (needs the DEoptim package)
#   holdout=YEAR        fit <= YEAR, predict the rest. Out-of-sample test.
#   mode=profile        profile likelihood (Raue et al. 2009)
#     which=c,phi       which parameters (default: c, phi, eps_P, gamma)
#     grid=12           grid points per parameter
#     span=1.5          half-width in TRANSFORMED units (log/logit)
#     pit=400           NM iterations per grid point
#     promote=chi2      (v5d) how far a grid point must beat the stored fit
#                       before it is promoted and a refit advised:
#                         chi2  (default) chi2(1)/2 = 1.921, the resolution at
#                               which the intervals on these very profiles are
#                               quoted. A descent smaller than that cannot move
#                               a reported bound, so it is banked and noted
#                               rather than chased.
#                         noise pre-v5d behaviour: promote anything above the
#                               solver/optimiser floor (~0.02 here)
#                         <num> an explicit delta-nll
#                       The rule in force is written to the manifest, so
#                       "we stopped here" is auditable as a decision taken in
#                       advance rather than after seeing the number.
#
#   ---- v5 additions ----
#   cycw=1              EXPLICIT multiplier on the cycle/moment term
#   warm=FILE.rds       seed a new fit from another state file, skip multistart
#
#   ---- v5b additions ----
#   loss=bpamp          band-passed point-wise term PLUS a band-passed
#                       amplitude anchor. See the v5b note below.
#   adjust_s0=true|false
#                       true  (default): s1, s2, s3, s5 at t=0 are derived from
#                               the 1961 observed flows -- see data_y0().
#                       false : the pre-v5b placeholder y0 (s1=s2=s3=s5=1).
#                               Kept ONLY to reproduce the archived
#                               loss=cycle fit (price NRMSE ~35%). It is part
#                               of the state signature and of the file suffix,
#                               so the two cannot silently share a state file.
#   profile now stores the FULL parameter vector at every grid point in
#   state_profile*.rds, so a point that beats the fit can be promoted into the
#   fit state instead of being thrown away. (The CSV drops that column.)
#
#   BACKWARD COMPATIBILITY. State files written before adjust_s0 existed still
#   load. sig_norm() reads a MISSING y0 field as "placeholder", which is the
#   only thing it can mean: the field was introduced at the same commit as the
#   data-derived stocks, precisely to stop old states resuming. So:
#     - `fit` will resume such a file only under adjust_s0=false, as it should;
#     - `plot` takes the convention FROM THE STATE and ignores the command
#       line, because y0 is part of the theta -> trajectory map and plotting a
#       placeholder fit under derived stocks would silently draw a trajectory
#       those parameters were never at.
#
#   state=FILE.rds      (plot) re-plot ANY saved fit without renaming it. The
#                       output suffix is taken from the state's own filename.
#                       Use this to tweak figure aesthetics against an archived
#                       result.
#   out=SUFFIX          (plot) override the OUTPUT suffix. Needed when the
#                       archived state has the same filename as the live one --
#                       a copy under archive/ derives the same suffix and would
#                       overwrite the current figures. plot warns before it does.
#
#   ---- v5c additions ----
#   local=SD            with warm=FILE, run a LOCAL multistart CENTRED on that
#                       file's parameters instead of skipping the multistart.
#                       SD is the per-parameter perturbation in TRANSFORMED
#                       units (log/logit), so local=0.5 is a factor-1.65 spread
#                       on a log-scaled parameter. Default 0 = old behaviour
#                       (warm start is a probe, multistart skipped).
#
#   ---- v5e additions ----
#   paper=true|false    (plot) ALSO write the figures of Gambaro et al. (2025)
#                       -- Figures 3 to 10 -- in the style of
#                       paper1_plotting.R, ready to \includegraphics into the
#                       chapter. Default true. These are separate from the
#                       diagnostic panels: no titles, panels lettered a), b),
#                       ... for the caption to refer to, axis labels in the
#                       "quantity, symbol (unit)" form, and the red curve is
#                       THIS fit rather than the paper's reference parameters.
#                       A fifth figure, paper_newflows, groups the four series
#                       the rebuilt dataset added and the paper never had:
#                       semis, new scrap (f3), f13 and f14.
#   dev=pdf|png         (plot) device for those figures. pdf (default) is
#                       vector, so \includegraphics[width=12cm] scales without
#                       resampling; png matches what the paper shipped.
#   taus=2,5,16,32      (plot) the four tau_E values for the delay figures,
#                       ascending, two below the reference value and two
#                       above. These are the paper's; the range 2..32 years
#                       spans the mine development times in Winton.
#                       COMPUTE COST: four extra dede solves at TOL_HI,
#                       seconds, not minutes. paper=false skips them.
#   end_year=2050       (plot) run the DELAY figures (7-10) out to that year
#                       instead of stopping at the last observation. Only
#                       those four: the reference-case figures exist to be
#                       compared against data and are left on the data window.
#                       NOT A FORECAST. funGDP uses rule = 2, so past the data
#                       the GDP growth forcing is held FLAT at its final
#                       observed value, and under cost=slade the anchor
#                       C0*exp(c1 t + c2 t^2) with c2 > 0 is being
#                       extrapolated well outside the window it was fitted on.
#                       The delay-induced OSCILLATION -- period, amplitude,
#                       phase -- is a property of theta and tau_E and can be
#                       read off a longer window; the price LEVEL cannot. A
#                       dotted rule marks where the data stop. Costs one extra
#                       solve, for the reference tau_E on the same grid.
#
#   ---------------------------------------------------------------------
#   v5c NOTE: WHY THE GLOBAL MULTISTART MISSES THE OSCILLATING BASIN
#   ---------------------------------------------------------------------
#   The candidate set below is: the REFERENCE vector, a deterministic grid over
#   c in {1, 2, 3.5, 5, 7} x phi in {0.01, 0.03, 0.08, 0.15}, and NSTART Latin-
#   hypercube draws centred on the REFERENCE at sd 0.8. The grid exists because
#   the surface has an oscillating basin and a flat one and they are far apart.
#
#   On the copper data the fitted optimum sits at c = 8.93, phi = 0.680 --
#   ABOVE the largest c in the grid and 4.5x the largest phi, with the LHS
#   centred somewhere else entirely. A 24-start run converged to 845.80; the
#   basin at 808.15 was located instead by promoting a profile grid point.
#   So the multistart is evidence about the region it covers, and NOT a
#   certificate of global optimality. Report it as such.
#
#   local= exists for the other half of that claim: once an optimum is in hand,
#   perturb AROUND it and show what fraction of independent descents return.
#   That is a basin-of-attraction result, and it is the honest thing to publish
#   next to a fit that a global multistart did not find. Do NOT shrink SD until
#   every candidate returns -- a perturbation small enough to guarantee return
#   proves nothing. Report the fraction you get.
#
#   local= changes only the STARTING POINT, never the objective, so it is
#   deliberately kept out of the state signature and the file suffix. It is
#   recorded in the manifest.
#
#   ---------------------------------------------------------------------
#   v5 NOTE: WHY THE v4 DEFAULT LOST THE PRICE CYCLE
#   ---------------------------------------------------------------------
#   The v4 default (loss=moment, eff=off) fitted the real data to a near-flat
#   price: band-passed amplitude 0.198 against 0.823 in the data, Theil
#   Us 0.773, price NRMSE 42.5%.
#
#   The proximate cause is arithmetic. At that optimum the objective was
#   pointwise 970.5 + cycle 100.0, and price alone contributed
#   0.5*62*(1.864/0.6)^2 = 299 of the pointwise term. Flattening cut the price
#   pointwise term from ~2516 (at parameters that do oscillate) to 299 -- a
#   gain of ~2200 -- while the cycle term charged only 100 for the amplitude it
#   destroyed. The optimiser took that trade, correctly.
#
#   The ROOT cause is that loss=moment scores only (logamp, logfreq). It is
#   BLIND TO PHASE. Nothing in it says where the peaks should fall, so the only
#   thing ever constraining phase was the raw point-wise price term.
#
#   That matters because of a false lead worth recording. The obvious fix looks
#   like eff=on: with eff=off, 62 price observations whose residuals have ACF1
#   0.86-0.92 are counted as 62 independent draws when they carry nearer 4-5
#   observations' worth of information, so the point-wise term is over-weighted
#   by an order of magnitude. That reasoning is correct as far as it goes, and
#   eff=on does restore amplitude -- measured, 24% -> 79% of the data's.
#
#   It also destroys the fit. Measured on this data:
#
#     loss=moment eff=off   NRMSE 42.5%   amp  24%   raw corr +0.45   SSE 215.5
#     loss=moment eff=on    NRMSE 54.4%   amp  79%   raw corr -0.06   SSE 352.4
#                                                    flat-line baseline SSE 240.5
#
#   The eff=on fit is WORSE THAN A HORIZONTAL LINE. A correctly-sized cycle in
#   the wrong phase costs more point-wise error than no cycle at all. eff=on
#   removed the only phase constraint in the objective and a phase-blind cycle
#   term happily spent the freedom.
#
#   So: eff=off is right for POINT ESTIMATION here, because the point-wise term
#   is carrying phase information that the ESS correction throws away. eff=on
#   belongs at the INFERENCE stage (profile intervals), where the independence
#   assumption really does understate interval width. Do not use it to fit.
#
#   THE FIX IS loss=bp. Match the BAND-PASSED price point by point instead of
#   matching two summary statistics of it. That constrains amplitude, frequency
#   and phase simultaneously, and the filter discards the annual noise a
#   23-year oscillator can never reproduce and that the raw point-wise term
#   keeps charging it for. Standard practice for cycle models (Sterman 1984).
#
#   WHAT IS ACHIEVABLE. Worth knowing before setting a target. mean|price| =
#   4.39; flat-line SSE 240.5 over 62 points gives data sd 1.97; detrended sd
#   is 1.29 and band-passed amplitude 0.823, so the high-frequency component is
#   sqrt(1.29^2 - 0.823^2) = 0.99. A model matching trend AND cycle perfectly
#   still carries RMSE 0.99, i.e. NRMSE ~23%. That is the wall. Anything below
#   it would be fitting annual noise. 25-32% is the realistic target and every
#   bit of it has to come from phase.
#
#   cycw= is a second, blunter lever: an explicit multiplier on whichever cycle
#   term is active. It appears in the filename and the manifest because a
#   hand-set weight you must declare is defensible in a way that one buried in
#   a bootstrap standard error is not. If you use cycw != 1, report the value
#   AND show the conclusions are not an artefact of it by refitting at two or
#   three values and tabulating price NRMSE against the amplitude ratio. That
#   trade-off curve is a better thesis result than any single fit.
#
#   ---------------------------------------------------------------------
#   v5b NOTE: DERIVED y0, AND WHY cycw= DOES NOT DEFEND THE CYCLE
#   ---------------------------------------------------------------------
#   Deriving s1, s2, s3, s5 from the 1961 flows (adjust_s0=true) removed a real
#   initialisation transient and cut the flow errors sharply -- s4 3.6%, s8
#   5.0%, f9 11.1%, extraction 6.2%. It also cost price fit: NRMSE 35% -> 38.3%
#   with a visibly shorter price cycle.
#
#   That is NOT a reweighting of the objective. It is a change in CURVATURE.
#   Before the fix the flow residuals were dominated by a fixed offset that no
#   theta could remove: it contributed to the LEVEL of the objective but almost
#   nothing to its GRADIENT, so k_E, z_R, n, gamma and alpha9 were effectively
#   free and the optimiser could spend them on the price cycle. With the
#   transient gone those residuals are genuinely parameter-sensitive, and k_E
#   in particular now has a strong extraction gradient. k_E sets how tightly E
#   tracks Q through tau_E, which is the SAME knob that sets the cycle period:
#   a smoothly-tracking E means a small (Q - E - R)/Q and therefore a fast,
#   weak oscillation. Extraction fit and cycle period compete through k_E.
#
#   WHAT ACTUALLY DEGRADED WAS THE PERIOD, NOT THE AMPLITUDE:
#
#       detrended amplitude   1.20 vs 1.29   (93% -- fine)
#       band-passed amplitude 0.754 vs 0.823 (92% -- fine)
#       band-passed PERIOD    19.0 a vs 23.2 a   (18% short)
#
#   And loss=cycle's term is AMPLITUDE-ONLY:
#       Lc = 30 * n * ((dsd(model) - dsd(data)) / dsd(data))^2
#   dsd() is sd(x - loess(span=.9)), i.e. the sd of everything above the trend,
#   ANNUAL NOISE INCLUDED. A 19-year cycle and a 23-year cycle of the same size
#   score identically. So raising cycw= puts all of its weight on the one
#   quantity that is already at 93%. It will not buy the long swing back.
#
#   THE FIX IS loss=bpamp. loss=bp matches the band-passed price point by
#   point, which constrains phase AND period -- but not size, because the
#   filtered residual can be cut by SHRINKING the cycle as well as by aligning
#   it, which is exactly the loss=bp failure mode (better NRMSE, amplitude down
#   about a third). bpamp adds the amplitude anchor back, scored on the
#   BAND-PASSED series rather than on dsd(), so it charges for the cycle rather
#   than for total detrended variance -- which annual noise supplies for free.
#
#   At the loss=cycle optimum the bp part alone would charge roughly
#   0.5 * 62 * 0.611^2 / B_sd^2, comparable to the whole price point-wise block
#   (242), without any hand-set cycw. The amplitude part contributes ~9 there
#   and rises quadratically the moment the optimiser tries to pay for phase
#   with size.
#
#   adjust_s0=false is the reproducible fallback to the archived loss=cycle
#   result. It is not a recommendation: the flow errors it produces are an
#   initialisation artefact, and the write-up should say so.
#   ---------------------------------------------------------------------
#
#   maxit=3000 | auto   iterations per call, or restart until converged
#   cap=45              minutes before "auto" gives up (rerun to continue)
#   loss=moment|sse|cycle|bp|bpamp   objective, see note 1 below       [moment]
#     loss=bp   v5. Point-wise fit to the BAND-PASSED price: constrains cycle
#               amplitude, frequency AND PHASE. Use with eff=off. See v5 note.
#     loss=bpamp v5b. loss=bp PLUS a band-passed amplitude anchor, so phase
#               cannot be bought by shrinking the cycle. See the v5b note.
#   cost=slade|constant|endogenous   cost anchor C(t)                  [slade]
#   eff=off|on          effective-sample-size correction               [off]
#   redundant=drop|keep drop data series that are exact identities     [drop]
#   tol=1e-8            ODE solver tolerance inside the objective
#   reltol=1e-6         optim() convergence tolerance (see note 6)
#   at=ref|fit          (diag) evaluate sensitivities where?           [ref]
#   reps=1              (recover) number of synthetic replicates
#   noise=1             (recover) multiplier on sigma; noise=0.001 is the
#                       structural-identifiability test
#   starts=0            (fit) extra Latin-hypercube multistarts
#   seed=7
#
#   Examples
#     Rscript run_all.R fit maxit=auto cap=120
#     Rscript run_all.R fit maxit=auto starts=40
#     Rscript run_all.R recover reps=50 maxit=4000
#     Rscript run_all.R recover noise=0.001 maxit=auto     # structural test
#     Rscript run_all.R fit loss=sse cost=constant         # model comparison
#     Rscript run_all.R fit loss=cycle redundant=keep      # reproduce v2 results
#     Rscript run_all.R fit free=core loss=bpamp eff=off maxit=auto \
#             warm=state_fit__slade_cycle_drop_core.rds    # v5b, seeded
#     Rscript run_all.R fit free=core loss=cycle eff=off adjust_s0=false \
#             maxit=auto starts=24                         # archived ~35% fit
#
#   "auto" keeps restarting the optimiser until it converges and a restart
#   yields no further gain. It saves state every round, so you can Ctrl-C at any
#   point and rerun to pick up where it left off.
#
#   Delete state_fit.rds / state_recover.rds to start over. The script refuses
#   to resume a state written under a different parameterisation, objective or
#   cost anchor rather than silently corrupting the fit.
#
#   Every run writes manifest_<mode>.txt: arguments, seeds, file checksums,
#   sessionInfo and the optimiser history. Quote it in the thesis appendix.
#
#   If you prefer clicking: open run_all.R in RStudio, edit the `opt` defaults
#   in section 0, and press Source.
#
# ---------------------------------------------------------------------
# WHAT CHANGED IN v3, and why
# ---------------------------------------------------------------------
# 1. LOSS. `cycle` added a hand-picked 30*n penalty to a Gaussian likelihood,
#    so the objective was not a likelihood and no likelihood-based inference
#    (AIC, BIC, likelihood ratio, Hessian standard errors) was available from
#    it. The motivation was sound: point-wise squared error prefers a flat line
#    to a phase-shifted oscillation, because amplitude A in the wrong phase
#    costs ~A^2 while flattening costs only A^2/2 -- so plain SSE actively
#    destroys the three price cycles that are the model's headline behaviour.
#    v3 keeps that protection but puts it on a stated footing:
#
#      loss=sse     pure Gaussian weighted least squares. A genuine negative
#                   log-likelihood (up to a constant). Use for LRT / AIC / BIC.
#      loss=moment  DEFAULT. sse plus a summary-statistic (moment-matching)
#                   term on the amplitude and frequency of the band-passed
#                   price cycle, with weights DERIVED from a moving-block
#                   bootstrap of the data rather than chosen. This is a
#                   composite-likelihood / indirect-inference estimator.
#      loss=cycle   the v2 term, bit-identical, for reproducing old results.
#
#    On this data the moment term charges ~2250 for a flat price path, ~42 for
#    a halved-amplitude cycle and ~3 for a correct one. The v2 term charged
#    1860 / 812 / 192: it defended against flatness slightly less firmly while
#    punishing amplitude error about twenty times harder than the calibrated
#    weight justifies, so it was over-steering the fit. Every run prints the
#    objective split into components, so you always know how much of the number
#    is likelihood and how much is moment matching.
#
#    Reporting rule: use loss=sse for any formal model comparison; report the
#    loss=moment fit as the preferred estimate; note that composite likelihoods
#    need a Godambe/CLIC adjustment before their curvature is read as a
#    covariance (a next step, not done here).
#
# 2. REDUNDANT DATA. `Recycling` is identically f13 + f14 in the rebuilt data
#    (max discrepancy 0.001 Tg/a, i.e. rounding). Fitting all three counted two
#    independent numbers three times and inflated the weight on the recycling
#    block by 50%. Dropped by default; redundant=keep restores v2 behaviour.
#    Both are still reported in the metrics table and plotted either way.
#
# 3. PARAMETER BOUNDS. gamma is a SHARE and was estimated unconstrained, so the
#    optimiser could push it outside [0,1], where (1-gamma)*R is a negative
#    flow. It is now estimated on a logit scale. eps_P is a positive elasticity
#    entering as -eps_P*g_P and was likewise unconstrained; now on a log scale.
#    Positivity and boundedness are structural rather than hoped for.
#
# 4. COST ANCHOR is now selectable from the command line (v2 hard-coded
#    "slade" inside sim() while the README asked for all three to be reported),
#    and the free-parameter set follows it: c1 is dropped for cost=constant,
#    where it does nothing and would be a perfectly flat direction; theta is
#    freed for cost=endogenous, where it was previously unreachable.
#
# 5. STALE COMMENTS. The v2 comment block listed gamma, alpha9 and eps_Y as
#    FIXED while the code freed the first two and fixed the third. Corrected.
#
# 6. TOLERANCES. v2 evaluated the objective at solver tolerance 1e-5 and asked
#    optim for reltol 1e-10, i.e. five orders of magnitude below the objective's
#    own numerical noise floor, so the last digits were chasing solver noise.
#    Defaults are now 1e-7 / 1e-8, and every fit ends with a MEASURED noise-
#    floor check reporting the actual objective jitter against reltol.
#
# 7. DIAGNOSTICS. Sensitivities now use central rather than forward differences
#    and are swept over three step sizes, so step-dependence of the conclusion
#    is visible rather than assumed away. `at=fit` repeats the analysis at the
#    fitted optimum: v2 only ever evaluated identifiability at the reference
#    point, which says nothing about identifiability where you actually are.
#
# 8. RECOVERY TEST. v2 ran one truth, one noise draw, one start -- a smoke
#    test. reps=N runs a proper simulation study and reports per-parameter bias
#    and spread. noise=0.001 gives a numerical structural-identifiability test:
#    if parameters are not recovered from near-noise-free data the problem is
#    structural, and no amount of better data or optimisation will fix it. The
#    synthetic dissipation bound is now regenerated from the synthetic truth
#    (v2 kept the real one, so the truth could violate its own constraint and
#    was then not the minimiser -- the test was measuring the wrong thing).
#
# 9. REPRODUCIBILITY. Resuming from a state file made results depend on how
#    many times you had run the script. Every state now carries a signature of
#    the parameterisation, the round history and the seeds, and a manifest is
#    written beside it.
#
# 10. PLOT BUGS. v2 looped over 11 series into a 3x3 device with a 7-entry
#     title vector, so four panels were titled NA and -- because the png()
#     filename has no %d -- fit_v2.png ended up holding the SECOND page, not
#     the first. Now 4x3 with all titles, plus a residual diagnostics figure.
#
# 11. METRICS now report, per series, the residual lag-1 autocorrelation, the
#     Durbin-Watson statistic, the implied effective sample size, and the
#     maximum-likelihood sigma (= RMSE) beside your assumed sigma -- so the
#     sigma guesses in section 1 can be checked rather than trusted. Price also
#     gets a Theil inequality decomposition (Theil 1966; Sterman 1984), which
#     splits misfit into bias, unequal variation and unequal covariation. Error
#     that is mostly covariation (Uc) is phase/point noise and is the tolerable
#     kind for an oscillator; error concentrated in Um or Us is systematic.
# =====================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- 0. arguments ---------------------------------------------------
args <- commandArgs(TRUE)
MODE <- if (length(args) >= 1 && !grepl("=", args[1], fixed = TRUE)) args[1] else "diag"

opt <- list(maxit = "3000", loss = "moment", cap = "45", cost = "slade",
            eff = "off", redundant = "drop", tol = "1e-8", reltol = "1e-6",
            at = "ref", reps = "1", noise = "1", starts = "0", seed = "7",
            # ---- v4 ----
            free = "full",     # full | core   (core drops the inestimable three)
            stage1 = "600",    # NM iterations per multistart candidate
            global = "none",   # none | de     (DEoptim pre-stage, optional)
            holdout = "0",     # last training YEAR; 0 = use all data
            which = "",        # (profile) comma-separated parameter names
            grid = "12",       # (profile) grid points per parameter
            span = "1.5",      # (profile) half-width in TRANSFORMED units
            pit = "400",       # (profile) NM iterations per grid point
            cores = "1",       # parallel workers (multistart + profile)
            # ---- v5 ----
            cycw = "1",        # multiplier on the cycle/moment term (see v5 note)
            warm = "",         # seed a NEW fit from another state file's par
            # ---- v5b ----
            adjust_s0 = "true",  # derive s1,s2,s3,s5 at t=0 from the 1961 flows
            state = "",          # (plot) read THIS state file instead of the
                                 #        name implied by the flags
            out = "",            # (plot) override the OUTPUT filename suffix
            # ---- v5c ----
            local = "0",         # with warm=: sd of a LOCAL multistart centred
                                 #             on the warm file's parameters
            chain = "",          # (plot) comma-separated ANCESTOR state files,
                                 #        oldest first. Their $history is spliced
                                 #        in front of this fit's own, so the
                                 #        convergence figure shows the whole
                                 #        descent and not just the last link.
            # ---- v5e ----
            paper = "true",      # (plot) also write the Gambaro et al. (2025)
                                 #        figures (3-10) for the chapter
            dev = "pdf",         # (plot) device for them: pdf | png
            taus = "2,5,16,32",  # (plot) tau_E values for the delay figures,
                                 #        ascending: two below the reference
                                 #        value, two above
            end_year = "0",      # (plot) run the delay figures out to THIS
                                 #        year instead of stopping at the last
                                 #        observation. 0 = data window.
            # ---- v5d ----
            promote = "chi2")    # (profile) how far a grid point must beat the
                                 #   stored fit before it is written out as a
                                 #   promotable seed and a refit is advised:
                                 #     chi2   chi2(1)/2 = 1.921, the resolution
                                 #            at which every interval here is
                                 #            quoted
                                 #     noise  the solver/optimiser floor (pre-v5d
                                 #            behaviour: promote anything real)
                                 #     <num>  an explicit delta-nll
                                 #   Recorded in the manifest, so the rule that
                                 #   was in force is part of the run's provenance.

rest <- if (length(args) > 1) args[-1] else character(0)
pos  <- rest[!grepl("=", rest, fixed = TRUE)]          # legacy positional form
if (length(pos) >= 1) opt$maxit <- pos[1]
if (length(pos) >= 2) opt$loss  <- pos[2]
if (length(pos) >= 3) opt$cap   <- pos[3]
for (s in rest[grepl("=", rest, fixed = TRUE)]) {
  kv <- strsplit(s, "=", fixed = TRUE)[[1]]
  if (!kv[1] %in% names(opt)) stop("unknown argument: ", kv[1],
                                   "\n  known: ", paste(names(opt), collapse = ", "))
  opt[[kv[1]]] <- paste(kv[-1], collapse = "=")
}

AUTO   <- identical(opt$maxit, "auto")
MAXIT  <- if (AUTO) NA_integer_ else as.integer(opt$maxit)
LOSS   <- match.arg(opt$loss,      c("moment", "sse", "cycle", "bp", "bpamp"))
COST   <- match.arg(opt$cost,      c("slade", "constant", "endogenous"))
EFF    <- match.arg(opt$eff,       c("off", "on"))
REDUND <- match.arg(opt$redundant, c("drop", "keep"))
DIAGAT <- match.arg(opt$at,        c("ref", "fit"))
# v5d: validate promote= HERE, not where it is used. The place it is used is
# after every grid point has been evaluated, so a typo would be caught at the
# end of a multi-hour profile run instead of at the start of one.
if (!opt$promote %in% c("chi2", "noise")) {
  .pv <- suppressWarnings(as.numeric(opt$promote))
  if (!is.finite(.pv) || .pv < 0)
    stop("promote= must be chi2, noise, or a non-negative number; got '",
         opt$promote, "'", call. = FALSE)
  rm(.pv)
}
CAPMIN <- as.numeric(opt$cap);    TOL    <- as.numeric(opt$tol)
RELTOL <- as.numeric(opt$reltol); REPS   <- as.integer(opt$reps)
NOISEM <- as.numeric(opt$noise);  NSTART <- as.integer(opt$starts)
SEED   <- as.integer(opt$seed)
TOL_HI <- 1e-9                     # reference tolerance for reporting/plotting
FREE   <- match.arg(opt$free,   c("full", "core"))
GLOBAL <- match.arg(opt$global, c("none", "de"))
STAGE1 <- as.integer(opt$stage1)
HOLDOUT <- as.integer(opt$holdout)
PGRID  <- as.integer(opt$grid)
# v5c: span= accepts EITHER a scalar (global, as before) OR per-parameter
# overrides, "gamma:1.0,k_E:0.10,c1:0.004", with a bare number among them
# setting the default. A single global span cannot serve a profile set whose
# curvatures differ by two orders of magnitude: too wide and the interval
# collapses to below one grid step, too narrow and it never crosses. Splitting
# into one launch per span is not an alternative -- every launch shares one
# state_profile RDS, read at start and written at end, so concurrent launches
# race and the loser's grid points are lost.
PSPAN_SET <- local({
  parts <- trimws(strsplit(opt$span, ",", fixed = TRUE)[[1]])
  parts <- parts[nzchar(parts)]
  named <- grepl(":", parts, fixed = TRUE)
  dflt  <- if (any(!named)) as.numeric(parts[!named][1]) else NA_real_
  ov <- numeric(0)
  if (any(named)) {
    kv <- do.call(rbind, strsplit(parts[named], ":", fixed = TRUE))
    ov <- setNames(as.numeric(trimws(kv[, 2])), trimws(kv[, 1]))
  }
  if (is.na(dflt)) dflt <- if (length(ov)) max(ov) else 1.5
  if (!is.finite(dflt) || dflt <= 0 || any(!is.finite(ov)) || any(ov <= 0))
    stop("span= must be positive numbers, e.g. span=0.10 or span=gamma:1.0,k_E:0.10",
         call. = FALSE)
  list(default = dflt, override = ov)
})
PSPAN  <- PSPAN_SET$default            # kept for messages that predate overrides
span_of <- function(nm) if (nm %in% names(PSPAN_SET$override))
                          unname(PSPAN_SET$override[[nm]]) else PSPAN_SET$default
PIT    <- as.integer(opt$pit)
CYCW   <- as.numeric(opt$cycw)
if (!is.finite(CYCW) || CYCW < 0) stop("cycw must be a non-negative number")
WARM   <- opt$warm
# v5c: LOCAL is the sd of a multistart centred on the warm= parameters. It
# moves the STARTING POINT only, so it stays out of SIGNATURE and CFG.
LOCAL  <- as.numeric(opt$local)
if (!is.finite(LOCAL) || LOCAL < 0) stop("local must be a non-negative number")
if (LOCAL > 0 && !nzchar(WARM))
  stop("local=", LOCAL, " needs warm=FILE.rds: it centres the multistart on\n",
       "  that file's parameters. Without warm= use the ordinary starts= form.",
       call. = FALSE)
if (LOCAL > 0 && NSTART <= 0)
  stop("local=", LOCAL, " needs starts=N (the number of perturbed candidates).",
       call. = FALSE)
# v5b: adjust_s0 selects the INITIAL STOCK convention. It changes theta ->
# trajectory for every theta, so it is a different objective, not a different
# starting guess: it belongs in the state signature and in the file suffix.
ADJ_S0 <- switch(tolower(opt$adjust_s0),
                 "true" = , "t" = , "yes" = , "on" = , "1" = TRUE,
                 "false" = , "f" = , "no" = , "off" = , "0" = FALSE,
                 stop("adjust_s0 must be true or false, got: ", opt$adjust_s0))
STATE  <- opt$state
OUTSFX <- opt$out
# ---- v5e: paper figures ---------------------------------------------
# These affect FIGURES ONLY -- never the objective, the state file or the
# suffix -- so unlike state= and out= they are simply ignored outside `plot`
# rather than being an error, and they stay out of SIGNATURE and CFG.
PAPER <- switch(tolower(opt$paper),
                "true" = , "t" = , "yes" = , "on" = , "1" = TRUE,
                "false" = , "f" = , "no" = , "off" = , "0" = FALSE,
                stop("paper must be true or false, got: ", opt$paper))
PDEV  <- match.arg(opt$dev, c("pdf", "png"))
PTAUS <- suppressWarnings(as.numeric(trimws(strsplit(opt$taus, ",", fixed = TRUE)[[1]])))
if (length(PTAUS) != 4L || any(!is.finite(PTAUS)) || any(PTAUS <= 0) ||
    is.unsorted(PTAUS, strictly = TRUE))
  stop("taus= wants FOUR distinct positive numbers in ascending order,\n",
       "  e.g. taus=2,5,16,32 -- two shorter delays then two longer ones.",
       call. = FALSE)
# end_year extends the DELAY figures beyond the data. It cannot extend the
# reference-case figures, which exist to be compared against observations, and
# it is not a forecast: see the note where it is used.
ENDYR <- suppressWarnings(as.integer(opt$end_year))
if (is.na(ENDYR) || (ENDYR != 0L && ENDYR < 1961L))
  stop("end_year= must be 0 (the data window) or a year, e.g. end_year=2050",
       call. = FALSE)
# v5c: chain= is a RECONSTRUCTION path, not a re-fit. Every warm= start before
# v5c discarded $history, so the descent that produced the current optimum is
# scattered across the archived states of the provenance chain. Listing them
# here splices those records back together for the convergence figure. It reads
# the files and nothing else -- no parameter is taken from them.
CHAIN  <- if (nzchar(opt$chain))
            trimws(strsplit(opt$chain, ",", fixed = TRUE)[[1]]) else character(0)
CHAIN  <- CHAIN[nzchar(CHAIN)]
if (length(CHAIN) && MODE != "plot")
  stop("chain= is only meaningful for `plot`.", call. = FALSE)
if (length(CHAIN)) {
  miss <- CHAIN[!file.exists(CHAIN)]
  if (length(miss)) stop("chain=: these files do not exist:\n  ",
                         paste(miss, collapse = "\n  "), call. = FALSE)
}
if (nzchar(STATE) && MODE != "plot")
  stop("state= is only meaningful for `plot`. For fit, use warm=.", call. = FALSE)
if (nzchar(OUTSFX) && MODE != "plot")
  stop("out= is only meaningful for `plot`.", call. = FALSE)
# Nelder-Mead is inherently SERIAL -- a single fit cannot be parallelised. But
# the multistart candidates and the profiled parameters are independent, and
# those are the two expensive stages, so this is where the cores go. Forking
# (mclapply) works on macOS and Linux; on Windows it silently falls back to one
# core. Run via Rscript rather than inside RStudio, where forking a session
# with open graphics devices is a known source of trouble.
CORES <- max(1L, as.integer(opt$cores))
if (CORES > 1L) {
  if (.Platform$OS.type != "unix") {
    cat("   cores>1 needs a unix-alike (fork); falling back to cores=1\n"); CORES <- 1L
  } else {
    suppressMessages(library(parallel))
    avail <- parallel::detectCores(logical = FALSE)
    if (is.finite(avail) && CORES > avail)
      cat(sprintf("   WARNING: cores=%d exceeds %d physical cores; oversubscribing slows this down\n",
                  CORES, avail))
    cat(sprintf("   parallel: %d workers for multistart / profile stages\n", CORES))
  }
}

if (!MODE %in% c("diag", "recover", "fit", "plot", "profile"))
  stop("MODE must be one of: diag recover fit plot profile")

suppressMessages(library(deSolve))
source("model_v2.R"); source("gloser_data_v2.R")
d <- comparison_data

# ---- 1. observation mapping and assumed measurement error -----------
# sigma = assumed 1-sd measurement error, in the units of each series.
# Printed data (E, f2, semis) are tight; digitised MFA outputs are looser.
# CAVEAT: these are estimates of data precision, not measurements of it. They
# set the RELATIVE weight between series, so the fit is sensitive to them.
# metrics() prints the maximum-likelihood sigma (= RMSE) beside each assumed
# value so the gap is visible. Gloser SI Table S7 carries standard deviations
# for the recycling indicators and is worth substituting.
OBS_ALL <- list(
  Price      = "Real Prices (World Bank) $/Tg billion",
  Extraction = "Extraction (Tg/a)",
  f2         = "F2 (Tg/a)",
  Recycling  = "Recycling (Tg/a)",   # EXACTLY f13 + f14 in the data: redundant
  s4         = "S4 (Tg)",
  s8         = "S8 (Tg)",
  f9         = "F9 (Tg/a)",
  semis      = "Semis (Tg/a)",       # exact, SI Table S2; constrains f8 + f3
  f3         = "F3 (Tg/a)",          # new scrap; constrains alpha_3
  f13        = "F13 (Tg/a)",         # direct-melt old scrap; constrains gamma
  f14        = "F14 (Tg/a)")         # old scrap to refining; constrains gamma

REDUNDANT <- "Recycling"
OBS <- if (REDUND == "drop") OBS_ALL[setdiff(names(OBS_ALL), REDUNDANT)] else OBS_ALL

# ---- 1b. out-of-sample split ----------------------------------------
# holdout=2005 fits 1961-2005 and predicts 2006-2022. The split must apply to
# EVERY channel through which data enters the objective, not just the pointwise
# term: the cycle statistics and their bootstrap weights are recomputed on the
# training price series alone, and the dissipation bound is masked too.
# Anything less leaks the test period into the fit and the forecast is no
# longer out of sample. Given the central claim is that production delays
# generate the price cycle ENDOGENOUSLY, a held-out forecast that reproduces
# the TIMING of the 2006-08 boom is worth more than any in-sample RMSE -- and
# if it fails, that is a result too, not an embarrassment.
TRAIN <- if (HOLDOUT > 0) d$Year <= HOLDOUT else rep(TRUE, nrow(d))
TEST  <- !TRAIN
if (HOLDOUT > 0)
  cat(sprintf("   HOLD-OUT: training on %d..%d (%d yrs), predicting %d..%d (%d yrs)\n",
              min(d$Year), HOLDOUT, sum(TRAIN), HOLDOUT + 1L, max(d$Year), sum(TEST)))

SIG <- c(Price = .60, Extraction = .35, f2 = .40, Recycling = .45,
         s4 = 12, s8 = 6, f9 = .70,
         semis = .60, f3 = .35, f13 = .30, f14 = .30,
         Dcum = 2)                   # was hard-coded as /2^2 in the v2 loss
DCOL <- "Dissip abandoned cum (Tg)"

# ---- 2. parameterisation --------------------------------------------
# Transform per parameter, so positivity and [0,1] bounds are structural rather
# than left to the optimiser. gamma is a SHARE; eps_P is a positive elasticity
# entering the demand equation as -eps_P * g_P.
TRANS <- c(c = "log", phi = "log", C0 = "log", c1 = "id", c2 = "id",
           eps_Y = "log", eps_P = "log", k_E = "log", z_E = "log",
           k_R = "log", z_R = "log", n = "log", s5_ref = "log",
           alpha9 = "log", gamma = "logit", alpha3 = "log", alpha8 = "log",
           lam4 = "log", theta = "id")
PN  <- names(TRANS)
enc <- function(nm, x) switch(TRANS[[nm]], log = log(x), logit = qlogis(x), id = x)
dec <- function(nm, x) switch(TRANS[[nm]], log = exp(x), logit = plogis(x), id = x)

# FREE: the parameters with no external information, including the three that
# govern price dynamics -- c (excess-demand gain), phi (cost-anchor damping)
# and eps_P (price elasticity, the stabilising channel). Fixing all three at
# guesses, as an earlier version did, suppresses the delay-induced price cycle
# that is the model's headline result. Collinearity tells you what cannot be
# estimated JOINTLY; it does not license pinning the dynamics at arbitrary
# values. The honest consequence, which belongs in the write-up, is that c, phi
# and eps_P have no individually meaningful point estimate -- only a joint
# manifold -- which is precisely the argument for a posterior over a point fit.
KEEP_CORE <- c("c", "phi", "eps_P", "C0", "k_E", "z_E", "k_R", "z_R", "n",
               # estimable BECAUSE the rebuilt series constrain them:
               "alpha3",  # <- f3 (new scrap)
               "alpha8",  # <- semis = f8 + f3
               "gamma",   # <- f13 and f14 observed separately
               "lam4",    # <- cumulative dissipation band
               "alpha9")  # <- f9; tests whether 0.049 survives the R correction
KEEP <- switch(COST,
  slade      = append(KEEP_CORE, "c1", after = 4),  # C0*exp(c1 t + c2 t^2)
  constant   = KEEP_CORE,                           # c1, c2 do nothing here
  endogenous = c(KEEP_CORE, "theta"))               # C0*exp(theta(X/X_ref - 1))

# --- free=core: the reduced set ---------------------------------------
# Nelder-Mead degrades badly above roughly ten dimensions -- the simplex
# collapses along the flat directions and reports convergence at a point it can
# still improve on. `recover` at noise=0.001 sitting ~9.5 above a minimum of 0
# is that failure, not an identifiability result. Reducing the dimension is the
# cheapest fix, and these three removals are the ones `diag` licenses:
#
#   k_R   backward elimination drops it at ALL THREE step sizes; individual
#         sensitivity 2.1, and it is collinear with z_R, which is kept.
#   z_E   dropped at h=1e-3; collinear with k_E (gamma 4.1), and k_E carries
#         ten times the sensitivity (25.6 vs 3.9), so k_E is the one to keep.
#   lam4  individual sensitivity 0.04 -- two orders of magnitude below every
#         other parameter. It appears in the step-invariant CORE, but that is
#         an ARTEFACT of the collinearity index: gam_of() normalises each
#         column to unit length, so a near-zero-sensitivity column is scaled up
#         and looks independent when it is really just uninformative. Its only
#         constraint is the cumulative-dissipation band, which is a ONE-SIDED
#         UPPER bound and is inactive at the fit, so it carries no information
#         at all. Worth saying explicitly in the write-up: collinearity and
#         sensitivity are different failures and the index only sees the first.
#
# c, phi and eps_P are NOT dropped even though elimination reaches for them,
# because they govern the price dynamics that are the model's headline result;
# pinning them at guesses would assume the conclusion. That is the whole
# argument for profiling them instead (mode=profile).
FREE_DROP <- c("k_R", "z_E", "lam4")
if (FREE == "core") KEEP <- setdiff(KEEP, FREE_DROP)

# FIXED, each with a reason:
#   eps_Y  = 1.1   income elasticity of copper demand, from the literature
#   c2     = 2e-4  weakly informed; free it by adding "c2" to KEEP above
#   s5_ref = 1.0   absorbed into alpha_R, so only the product is identified
#   tau_E, tau_R   production lead times, taken from the industry data
# NOTE gamma and alpha9 are FREE. The v2 comment block listed them as fixed
# while the code freed them; that comment was stale.

# Parameters that do nothing under the selected cost anchor are excluded from
# the diagnostics as well, so they cannot show up as spurious flat directions.
PN_ACTIVE <- switch(COST,
  slade      = setdiff(PN, "theta"),
  constant   = setdiff(PN, c("c1", "c2", "theta")),
  endogenous = setdiff(PN, c("c1", "c2")))

base <- reference_parms()
base$alpha9 <- base$alpha[9]; base$alpha3 <- base$alpha[3]
base$alpha8 <- base$alpha[8]; base$lam4   <- base$lambda[4]
y0 <- reference_y0()

# ---- initial stocks derived from the 1961 flows ---------------------
# reference_y0() leaves s1, s2, s3 and s5 at the placeholder value 1. With
# alpha_2 = 11.2 that puts f2(0) at 11.2 Tg/a against an observed 5.13, so the
# opening years of f2, f3, f9 and f13 are an INITIALISATION TRANSIENT rather
# than model error -- visible as a monotone lowess in the residual-vs-fitted
# panels and as five failed Shapiro tests. Each stock below is instead set to
# the level that reproduces its own observed outflow at t = 0.
#
# No new free parameters. But alpha_3 and alpha_8 ARE fitted, so s3(0) moves
# with the parameter vector and y0 has to be rebuilt at every evaluation --
# hence a function of p rather than the constant above.
#
# s1 has no observed outflow, so it is set from the mass balance
# E = (alpha_1 + alpha_4) * s1 at t = 0. That is an assumption, not a
# measurement, and belongs in the text as such.
#
# v5b: adjust_s0=false short-circuits ALL of the above and returns the pre-v5b
# placeholder y0 unchanged (s1 = s2 = s3 = s5 = 1). That reproduces the
# archived loss=cycle fit -- price NRMSE ~35% with a well-captured cycle -- at
# the cost of an initialisation transient in f2, f3, f9 and f13 that is model
# ERROR only in the sense that the model was started in the wrong place. It is
# a REPRODUCIBILITY FALLBACK, not a modelling recommendation, and the write-up
# should not present it as one.
data_y0 <- function(p, dat = d) {
  r  <- y0
  if (!ADJ_S0) return(r)             # v5b fallback: placeholder stocks
  i1 <- which.min(dat$Year)
  g  <- function(col) { x <- dat[[col]][i1]; if (is.finite(x)) x else NA_real_ }
  keep <- function(new, old, lo = 1e-3, hi = 1e4)
    if (is.finite(new) && new > lo && new < hi) new else old

  s <- r$s
  s[1] <- keep(g(OBS_ALL$Extraction) / (p$alpha[1] + p$alpha[4]), s[1])
  s[2] <- keep(g(OBS_ALL$f2)         /  p$alpha[2],               s[2])
  s[3] <- keep(g(OBS_ALL$semis)      / (p$alpha[3] + p$alpha[8]), s[3])
  R1   <- g(OBS_ALL$Recycling)
  s5   <- if (identical(p$limiter, "saturating")) {
            rr <- R1 / r$Rmax
            if (is.finite(rr) && rr < 1) p$K * rr / (1 - rr) else s[5]
          } else p$s5_ref * (R1 / r$Rmax)^(1 / p$n)
  s[5] <- keep(s5, s[5], lo = 1e-3, hi = 1e3)
  r$s  <- s
  r
}

setp <- function(v) {
  p <- base
  for (i in seq_along(PN)) p[[PN[i]]] <- dec(PN[i], v[i])
  p$alpha[9] <- p$alpha9; p$alpha[3] <- p$alpha3
  p$alpha[8] <- p$alpha8; p$lambda[4] <- p$lam4
  p
}
v0 <- vapply(PN, function(nm) enc(nm, base[[nm]]), numeric(1))
ki <- match(KEEP, PN)

# LSODA prints stiffness chatter straight to stdout for extreme parameter
# draws; those draws just get the 1e10 penalty, so swallow the noise.
sim <- function(v, tol = TOL) {
  p <- setp(v)                       # y0 depends on alpha3/alpha8 and n, so it
  o <- NULL                          # is rebuilt at every evaluation
  invisible(capture.output(suppressWarnings(
    o <- try(dmmcm_v2(p, data_y0(p), seq(0, 61, 1), funGDP, COST,
                      eps_sw = .02, atol = tol, rtol = tol,
                      maxsteps = 3000), silent = TRUE))))
  if (inherits(o, "try-error") || is.null(o) || any(!is.finite(as.matrix(o)))) NULL else o
}

# ---- 3. cycle summary statistics ------------------------------------
# Point-wise squared error PREFERS A FLAT LINE to an out-of-phase oscillation:
# amplitude A in the wrong phase costs ~A^2, flattening costs only A^2/2. So a
# pure SSE objective actively destroys the price cycle. Rather than bolting an
# arbitrary penalty onto the likelihood, match summary statistics of the cycle
# and derive their weights from the data.
#
# Both series are band-passed the same way before the statistics are taken --
# a difference of two loess smooths, the same idea as a Baxter-King band-pass
# filter. This matters: the model is a deterministic DDE and is smooth, while
# the data carry short-run noise, so ANY statistic sensitive to high-frequency
# content would be systematically mismatched for reasons having nothing to do
# with the parameters. Band-passing puts both on the same footing first.
#
#   logamp  = log sd of the band-passed series   -> cycle AMPLITUDE
#   logfreq = 0.5 log( sum(diff^2)/sum(x^2) )    -> cycle FREQUENCY
#             (for a sinusoid this ratio is 2(1-cos w) ~ w^2, so the statistic
#              is log w; the implied period is 2*pi/exp(logfreq))
# Both are smooth in the parameters, unlike a turning-point count.
lo   <- function(x, s) as.numeric(predict(loess(x ~ seq_along(x), span = s, degree = 1)))
band <- function(x) lo(x, .25) - lo(x, .90)

cyc_stats <- function(x) {
  b  <- band(x); a <- sd(b)
  rr <- if (a <= 0) 1e-6 else sum(diff(b)^2) / sum(b^2)
  # +1e-3 is a SOFT floor: it keeps a perfectly flat trajectory finite and
  # differentiable instead of introducing a cliff, and is ~0.1% of the observed
  # amplitude, so it does not bias the statistic anywhere near the data.
  c(logamp = log(a + 1e-3), logfreq = 0.5 * log(min(max(rr, 1e-6), 4)))
}

# Sampling variability of those statistics, by moving-block bootstrap of the
# high-frequency residual of the DATA. This is what makes the moment term a
# calibrated composite likelihood rather than a hand-picked penalty: the weight
# answers "how precisely does this dataset pin down the cycle amplitude and
# frequency?", and the answer comes from the data.
stat_sd <- function(x, B = 400, L = 6, seed = 20260813) {
  set.seed(seed); n <- length(x)
  sm <- lo(x, .25); rs <- x - sm
  nb <- ceiling(n / L); st <- seq_len(n - L + 1)
  S <- t(vapply(seq_len(B), function(b) {
    idx <- unlist(lapply(sample(st, nb, TRUE), function(i) i:(i + L - 1)))[seq_len(n)]
    cyc_stats(sm + rs[idx])
  }, numeric(2)))
  pmax(apply(S, 2, sd), 1e-3)
}

# v5: sampling sd of the BAND-PASSED SERIES ITSELF, pointwise, by the same
# moving-block bootstrap as stat_sd. This is what calibrates loss=bp: it
# answers "how precisely does this dataset pin down the band-passed price in
# any given year?", and the answer again comes from the data rather than from a
# weight someone chose.
bp_sd <- function(x, B = 400, L = 6, seed = 20260813) {
  set.seed(seed); n <- length(x)
  sm <- lo(x, .25); rs <- x - sm
  S <- vapply(seq_len(B), function(b) {
    idx <- unlist(lapply(sample(seq_len(n - L + 1), ceiling(n / L), TRUE),
                         function(i) i:(i + L - 1)))[seq_len(n)]
    band(sm + rs[idx])
  }, numeric(n))
  max(mean(apply(S, 1, sd)), 1e-3)
}

# v2 detrended standard deviation, kept EXACTLY for loss=cycle reproducibility.
dsd <- function(x) sd(x - predict(loess(x ~ seq_along(x), span = .9)))

# ---- 4. effective sample size ---------------------------------------
# Residuals of a deterministic dynamic model against 60 years of annual data
# are strongly autocorrelated, so the Gaussian-independence assumption in the
# point-wise term overstates how much independent information each series
# carries. n_eff = n(1-rho)/(1+rho) is the standard first-order correction.
# It is REPORTED always, and APPLIED only with eff=on, because it changes the
# point estimates and its main payoff is at the uncertainty-quantification
# stage. Either way it is computed ONCE at startup from the reference run and
# frozen in the state file, so the objective stays a fixed function during the
# optimisation rather than moving under the optimiser.
eff_weights <- function(o, dat) {
  vapply(names(OBS), function(k) {
    ok <- !is.na(dat[[OBS[[k]]]]); r <- dat[[OBS[[k]]]][ok] - o[[k]][ok]
    if (length(r) < 10) return(1)
    rho <- suppressWarnings(stats::cor(r[-1], r[-length(r)]))
    if (!is.finite(rho)) return(1)
    rho <- min(max(rho, 0), .98)
    max((1 - rho) / (1 + rho), .05)
  }, numeric(1))
}

# ---- 5. objective ---------------------------------------------------
mkloss <- function(dat, effw, S_dat, S_sd, B_dat = NULL, B_sd = 1) {
  # TRAIN is all-TRUE unless holdout= is set. Applied here so the split reaches
  # the pointwise term, the dissipation bound and the cycle window alike.
  okl <- lapply(names(OBS), function(k) !is.na(dat[[OBS[[k]]]]) & TRAIN)
  names(okl) <- names(OBS)
  okd  <- if (is.null(dat[[DCOL]])) NULL else !is.na(dat[[DCOL]]) & TRAIN
  okp  <- okl[["Price"]]; pcol <- OBS$Price
  function(vk, parts = FALSE) {
    bad <- c(pointwise = NA, bound = NA, cycle = NA, total = 1e10)
    v <- v0; v[ki] <- vk; o <- sim(v)
    if (is.null(o)) return(if (parts) bad else 1e10)

    Lp <- sum(vapply(names(OBS), function(k) {
      ok <- okl[[k]]
      effw[[k]] * .5 * sum(((dat[[OBS[[k]]]][ok] - o[[k]][ok]) / SIG[[k]])^2)
    }, numeric(1)))

    # Cumulative dissipation: Gloser's Fig.5 band is dissipation PLUS
    # abandoned-in-place, and the model treats only the former, so the band is
    # an UPPER BOUND. One-sided penalty: charge only if the model exceeds it.
    Lb <- 0
    if (!is.null(okd)) {
      bnd <- dat[[DCOL]][okd] - dat[[DCOL]][okd][1]
      Lb  <- .5 * sum(pmax(0, o$Dcum[okd] - bnd)^2) / SIG[["Dcum"]]^2
    }

    Lc <- 0
    if (LOSS == "cycle") {                       # v2 term, bit-identical
      a_o <- dsd(dat[[pcol]][okp]); a_m <- dsd(o$Price[okp])
      Lc  <- 30 * sum(okp) * ((a_m - a_o) / a_o)^2
    } else if (LOSS == "moment") {               # calibrated composite term
      Lc <- .5 * sum(((cyc_stats(o$Price[okp]) - S_dat) / S_sd)^2)
    } else if (LOSS == "bp") {
      # v5: point-wise fit to the BAND-PASSED price. loss=moment matched the
      # amplitude and the frequency of the cycle but said nothing about WHERE
      # the peaks fall, so the optimiser was free to put a correctly-sized
      # cycle in the wrong phase -- which it did (corr -0.06). Matching the
      # filtered series point by point constrains amplitude, frequency AND
      # phase at once, while the filter discards the annual noise that a
      # 23-year oscillator can never reproduce and that the raw point-wise
      # term keeps charging it for.
      Lc <- .5 * sum((band(o$Price[okp]) - B_dat)^2) / B_sd^2
    } else if (LOSS == "bpamp") {
      # v5b: loss=bp constrains phase and PERIOD but not SIZE -- the filtered
      # residual can be cut by shrinking the cycle just as well as by aligning
      # it, which is the measured loss=bp failure (better NRMSE, amplitude down
      # about a third). The v2 amplitude anchor is added back, but scored on the
      # BAND-PASSED series rather than on dsd(). That matters: dsd() is the sd
      # of everything above the loess trend, annual noise included, so a
      # 19-year cycle and a 23-year cycle of equal size score identically under
      # loss=cycle and cycw= has nothing to push on. sd(band(.)) charges for
      # the CYCLE.
      bm  <- band(o$Price[okp])
      a_o <- sd(B_dat); a_m <- sd(bm)
      Lc  <- .5 * sum((bm - B_dat)^2) / B_sd^2 +
             30 * sum(okp) * ((a_m - a_o) / a_o)^2
    }
    # v5: CYCW is an EXPLICIT, reported multiplier on the cycle term. Default 1
    # leaves the objective exactly as v4 wrote it. It exists because the
    # relative weight of the cycle term against the point-wise term is a
    # modelling CHOICE, and a choice you have to state is better than one
    # buried in a bootstrap standard error. Quote the value you used, and show
    # the fit is not an artefact of it -- see the note at the top of the file.
    Lc <- CYCW * Lc

    tot <- Lp + Lb + Lc
    if (!is.finite(tot)) tot <- 1e10
    if (parts) c(pointwise = Lp, bound = Lb, cycle = Lc, total = tot) else tot
  }
}

# ---- 6. metrics -----------------------------------------------------
# Theil (1966) inequality decomposition as applied to dynamic models by Sterman
# (1984). MSE splits into bias (Um), unequal variation (Us) and unequal
# covariation (Uc), summing to 1. Error concentrated in Uc is point-by-point or
# phase error, which for an oscillator is the tolerable kind; error in Um or Us
# is systematic and is not. This is the diagnostic that justifies preferring a
# phase-shifted cycle to a flat line, and it belongs in the thesis.
theil <- function(ym, yo) {
  mse <- mean((ym - yo)^2)
  if (mse <= 0) return(c(Um = 0, Us = 0, Uc = 0))
  sm <- sqrt(mean((ym - mean(ym))^2)); so <- sqrt(mean((yo - mean(yo))^2))
  r  <- suppressWarnings(stats::cor(ym, yo)); if (!is.finite(r)) r <- 0
  c(Um = (mean(ym) - mean(yo))^2 / mse, Us = (sm - so)^2 / mse,
    Uc = 2 * (1 - r) * sm * so / mse)
}

metrics <- function(o, dat, label, effw = NULL, win = NULL) {
  if (is.null(win)) win <- rep(TRUE, nrow(dat))
  cat("\n ", label, "\n")
  cat("  series        n  n_eff     RMSE   NRMSE     bias  sigma_set  sd_ratio   ACF1     DW      obj\n")
  tot <- 0
  for (k in names(OBS_ALL)) {
    col <- OBS_ALL[[k]]; ok <- !is.na(dat[[col]]) & win
    if (sum(ok) < 3) next
    r  <- dat[[col]][ok] - o[[k]][ok]
    rm <- sqrt(mean(r^2)); nr <- rm / mean(abs(dat[[col]][ok]))
    rho <- suppressWarnings(stats::cor(r[-1], r[-length(r)])); if (!is.finite(rho)) rho <- 0
    dw  <- sum(diff(r)^2) / sum(r^2)
    rc  <- min(max(rho, 0), .98)
    ne  <- sum(ok) * max((1 - rc) / (1 + rc), .05)
    inobj <- k %in% names(OBS)
    # v5: the per-series contribution to the point-wise term, in the SAME units
    # as the objective. Without this column the balance between series -- and
    # between the point-wise term and the cycle term -- is invisible, and you
    # find out that one channel owns a third of the objective only after the
    # optimiser has spent it. That is exactly what happened in v4.
    oc <- if (inobj) (if (is.null(effw)) 1 else effw[[k]]) * .5 * sum((r / SIG[[k]])^2) else NA_real_
    if (inobj) tot <- tot + oc
    cat(sprintf("  %-11s%4d %6.1f %8.3f %6.1f%% %+8.3f %10.2f %9.2f %+6.2f %6.2f %8s%s\n",
                k, sum(ok), ne, rm, 100 * nr, mean(r), SIG[[k]], rm / SIG[[k]], rho, dw,
                if (inobj) sprintf("%.1f", oc) else "-",
                if (inobj) "" else "   [not in objective]"))
  }
  ok <- !is.na(dat[[OBS_ALL$Price]]) & win; yo <- dat[[OBS_ALL$Price]][ok]; ym <- o$Price[ok]
  # cyc_stats() band-passes with two loess smooths, which need a reasonable
  # number of points. A short hold-out window would produce nonsense rather
  # than fail loudly, so skip the cycle block instead of printing garbage.
  if (sum(ok) < 12) {
    cat(sprintf("  (window has %d price points: cycle statistics skipped)\n", sum(ok)))
    return(invisible(tot))
  }
  tp <- function(x) sum(diff(sign(diff(x))) != 0)
  Sm <- cyc_stats(ym); So <- cyc_stats(yo); U <- theil(ym, yo)
  cat(sprintf("  PRICE CYCLE : corr %+.2f | amplitude model %.2f vs data %.2f | turning pts %d vs %d\n",
              stats::cor(yo, ym), dsd(ym), dsd(yo), tp(ym), tp(yo)))
  bm <- band(ym); bo <- band(yo)
  bpc <- suppressWarnings(stats::cor(bm, bo)); if (!is.finite(bpc)) bpc <- 0
  cat(sprintf("  band-passed : amp %.3f vs %.3f | period %.1f a vs %.1f a\n",
              exp(Sm[[1]]), exp(So[[1]]), 2 * pi / exp(Sm[[2]]), 2 * pi / exp(So[[2]])))
  # v5: PHASE. loss=moment matches amplitude and frequency and is blind to this.
  # A correctly-sized cycle in antiphase scores well on amplitude and is worse
  # than useless -- it costs MORE point-wise error than a flat line.
  cat(sprintf("  cycle phase : corr(band-passed) %+.2f  |  RMSE %.3f vs data amplitude %.3f%s\n",
              bpc, sqrt(mean((bm - bo)^2)), sd(bo),
              if (bpc < 0.3) "   <-- PHASE NOT MATCHED" else ""))
  cat(sprintf("  THEIL       : Um %.3f (bias)  Us %.3f (variation)  Uc %.3f (covariation)\n",
              U[[1]], U[[2]], U[[3]]))
  cat(sprintf("  price SSE %.1f  (flat-line baseline %.1f)\n",
              sum((yo - ym)^2), sum((yo - mean(yo))^2)))
  cat(sprintf("  weighted point-wise nll = %.1f\n", tot))

  # ---- v5: OBJECTIVE BUDGET and the flattening guard -----------------
  # The v4 fit lost the price cycle silently. It was not an optimiser failure:
  # the point-wise term was ~10x the cycle term, so flattening the oscillation
  # was a good trade for the objective as specified. That imbalance was
  # visible in the numbers all along and nothing printed it. This does.
  Lc_rep <- if (LOSS == "cycle") CYCW * 30 * length(yo) * ((dsd(ym) - dsd(yo)) / dsd(yo))^2
            else if (LOSS == "moment") CYCW * .5 * sum(((Sm - S_dat) / S_sd)^2)
            else if (LOSS == "bp") CYCW * .5 * sum((band(ym) - band(yo))^2) / B_sd^2
            else if (LOSS == "bpamp")
              CYCW * (.5 * sum((band(ym) - band(yo))^2) / B_sd^2 +
                      30 * length(yo) *
                        ((sd(band(ym)) - sd(band(yo))) / sd(band(yo)))^2)
            else 0
  if (LOSS != "sse") {
    cat(sprintf("  OBJECTIVE BUDGET: pointwise %.1f (%.0f%%) + cycle %.1f (%.0f%%)%s\n",
                tot, 100 * tot / max(tot + Lc_rep, 1e-12),
                Lc_rep, 100 * Lc_rep / max(tot + Lc_rep, 1e-12),
                if (CYCW != 1) sprintf("  [cycw=%g applied]", CYCW) else ""))
    cat("                    (cycle term scored against the TRAINING-window targets)\n")
  }
  amp_ratio <- exp(Sm[[1]]) / exp(So[[1]])
  if (is.finite(amp_ratio) && amp_ratio < 0.6) {
    cat(sprintf(paste0(
      "  [!] CYCLE FLATTENED: band-passed amplitude is %.0f%% of the data's.\n",
      "      Point-wise error prefers a flat line to a phase-shifted oscillation,\n",
      "      so this is the objective working as specified, not a failed solve.\n",
      "      Levers, in order of defensibility:\n",
      "        eff=on    price residuals here have ACF1 %+.2f, so eff=off is\n",
      "                  asserting ~%d independent price observations when there\n",
      "                  are nearer %.0f. That over-weights the point-wise term.\n",
      "        cycw=N    explicit multiplier on the cycle term. State the value.\n"),
      100 * amp_ratio,
      { rp <- yo - ym; suppressWarnings(stats::cor(rp[-1], rp[-length(rp)])) },
      length(yo),
      { rp <- yo - ym; rr <- suppressWarnings(stats::cor(rp[-1], rp[-length(rp)]))
        rr <- min(max(if (is.finite(rr)) rr else 0, 0), .98)
        length(yo) * max((1 - rr) / (1 + rr), .05) }))
  }
  invisible(tot)
}

report_parts <- function(nll, par, tag = "objective") {
  p <- nll(par, parts = TRUE)
  cat(sprintf("  %s [loss=%s]: pointwise %.2f + bound %.2f + cycle %.2f = %.2f\n",
              tag, LOSS, p[[1]], p[[2]], p[[3]], p[[4]]))
  invisible(p)
}

# ---- 7. reproducibility ---------------------------------------------
# `noise` belongs in the signature: a recover run at noise=0.001 and one at
# noise=1 are DIFFERENT experiments, and without this they would silently resume
# from each other's state file.
SIGNATURE <- list(keep = KEEP, trans = unname(TRANS[KEEP]), loss = LOSS, cost = COST,
                  obs = names(OBS), sig = SIG, eff = EFF, redundant = REDUND,
                  noise = if (MODE == "recover") NOISEM else NA_real_,
                  holdout = HOLDOUT,
                  # y0 is part of the objective: derived initial stocks change
                  # theta -> trajectory for EVERY theta, so a state file written
                  # under the old placeholder y0 must not resume here. v5b makes
                  # the convention explicit rather than implicit, so both
                  # settings can coexist on disk without either corrupting the
                  # other on resume.
                  y0 = if (ADJ_S0) "data-derived" else "placeholder")

# Variant runs (a different cost anchor, objective, or noise level) are separate
# experiments and must not share state files or overwrite each other's figures.
# The DEFAULT configuration keeps the plain names, so nothing changes for the
# ordinary workflow; anything else is suffixed automatically.
CFG <- {
  dflt <- COST == "slade" && LOSS == "moment" && REDUND == "drop" && EFF == "off" &&
          FREE == "full" && HOLDOUT == 0 && CYCW == 1 && ADJ_S0 &&
          (MODE != "recover" || NOISEM == 1)
  if (dflt) "" else paste0("__", COST, "_", LOSS, "_", REDUND,
    if (EFF != "off") paste0("_eff", EFF) else "",
    if (FREE != "full") paste0("_", FREE) else "",
    if (CYCW != 1) paste0("_cw", format(CYCW, scientific = FALSE)) else "",
    if (!ADJ_S0) "_s0ref" else "",
    if (HOLDOUT > 0) paste0("_ho", HOLDOUT) else "",
    if (MODE == "recover" && NOISEM != 1) paste0("_noise", format(NOISEM, scientific = FALSE)) else "")
}
tagged <- function(stem, ext) paste0(stem, CFG, ext)
if (nzchar(CFG)) cat("   variant run: files suffixed with '", CFG, "'\n", sep = "")

# ---- v5b: legacy signature upgrade ----------------------------------
# State files written before a field existed are still USABLE, provided the
# absence of that field has an unambiguous historical meaning. Two do:
#
#   y0      Introduced at the same commit as the data-derived initial stocks,
#           precisely so that placeholder-y0 states would stop resuming. A
#           state with NO y0 field therefore predates that change and can only
#           be a PLACEHOLDER fit. That inference is sound, not a guess.
#   holdout Added with the out-of-sample split; absent means none was used.
#
# Anything else missing is a genuine unknown and is left missing, so the
# identical() comparison still fails and the state is still refused. This
# widens compatibility without widening the blast radius.
sig_norm <- function(sg) {
  if (is.null(sg)) return(NULL)
  if (is.null(sg$y0))      sg$y0      <- "placeholder"
  if (is.null(sg$holdout)) sg$holdout <- 0L
  sg[names(SIGNATURE)]                # same field ORDER, so identical() works
}

check_state <- function(st, SF) {
  if (is.null(st$sig) || !identical(sig_norm(st$sig), SIGNATURE)) {
    old <- if (is.null(st$sig)) "a pre-v3 version" else
      sprintf("keep=%d params, loss=%s, cost=%s, eff=%s, redundant=%s, y0=%s",
              length(st$sig$keep), st$sig$loss, st$sig$cost, st$sig$eff,
              st$sig$redundant,
              if (is.null(st$sig$y0)) "placeholder (inferred: pre-v5b file)"
              else st$sig$y0)
    stop(sprintf(paste0("%s was written under a different setup (%s);\n",
      "  this run wants keep=%d params, loss=%s, cost=%s, eff=%s, redundant=%s, y0=%s.\n",
      "  Resuming would corrupt the fit. Start again:\n",
      "      file.remove(\"%s\")      # in R\n      rm %s                     # in a terminal"),
      SF, old, length(KEEP), LOSS, COST, EFF, REDUND,
      if (ADJ_S0) "data-derived" else "placeholder", SF, SF), call. = FALSE)
  }
  st
}

write_manifest <- function(mode, extra = list()) {
  f <- paste0("manifest_", mode, CFG, ".txt")
  md5 <- tryCatch(tools::md5sum(c("run_all.R", "model_v2.R", "gloser_data_v2.R")),
                  error = function(e) NULL)
  con <- file(f, "w")
  cat("DMMCM v4 run manifest\n", file = con)
  cat("timestamp   : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n", sep = "", file = con)
  cat("mode        : ", mode, "\n", sep = "", file = con)
  cat("arguments   : ", paste(args, collapse = " "), "\n", sep = "", file = con)
  cat("resolved    : ", paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "),
      "\n", sep = "", file = con)
  cat("cycle weight: ", CYCW, if (CYCW != 1) "  (NON-DEFAULT -- state this in the text)" else "",
      "\n", sep = "", file = con)
  cat("warm start  : ", if (nzchar(WARM)) WARM else "none", "\n", sep = "", file = con)
  cat("local mstart: ",
      if (LOCAL > 0) paste0("sd ", LOCAL, " (transformed), ", NSTART,
                            " candidates centred on the warm fit")
      else "none", "\n", sep = "", file = con)
  cat("free params : ", paste(KEEP, collapse = ", "), "\n", sep = "", file = con)
  cat("transforms  : ", paste(TRANS[KEEP], collapse = ", "), "\n", sep = "", file = con)
  cat("objective   : ", paste(names(OBS), collapse = ", "), "\n", sep = "", file = con)
  if (!is.null(md5)) { cat("file md5    :\n", file = con)
    for (i in seq_along(md5)) cat(sprintf("  %-20s %s\n", names(md5)[i], md5[i]), file = con) }
  for (nm in names(extra)) { cat(nm, ":\n", sep = "", file = con)
    cat(paste0("  ", extra[[nm]], collapse = "\n"), "\n", file = con) }
  cat("\n--- sessionInfo ---\n", file = con)
  cat(paste(capture.output(sessionInfo()), collapse = "\n"), "\n", file = con)
  close(con)
  cat("wrote ", f, "\n", sep = "")
}

cat(sprintf(paste0("== DMMCM v5b | mode=%s loss=%s cost=%s eff=%s cycw=%g redundant=%s ",
                   "adjust_s0=%s tol=%g reltol=%g\n"),
            MODE, LOSS, COST, EFF, CYCW, REDUND, tolower(ADJ_S0), TOL, RELTOL))
if (!ADJ_S0)
  cat("   [!] adjust_s0=false: REPRODUCIBILITY FALLBACK. Initial stocks s1, s2, s3, s5\n",
      "       are left at the placeholder value 1 instead of being derived from the\n",
      "       1961 flows, so f2(0) etc. start in the wrong place and the opening\n",
      "       years of f2, f3, f9 and f13 carry an INITIALISATION TRANSIENT that is\n",
      "       reported as model error. This exists to reproduce the archived\n",
      "       loss=cycle fit (price NRMSE ~35%). Do not present it as the preferred\n",
      "       specification. See the v5b note at the top of this file.\n", sep = "")
if (EFF == "off" && MODE %in% c("fit", "recover"))
  cat("   NOTE eff=off weights every observation equally. With residual ACF1 near\n",
      "        1 that over-states the information in the point-wise term and the\n",
      "        optimiser will buy point-wise fit by flattening the price cycle.\n",
      "        See the v5 note at the top of this file.\n", sep = "")
cat(sprintf("   free parameters (%d): %s\n", length(KEEP), paste(KEEP, collapse = ", ")))
if (REDUND == "drop")
  cat("   NOTE Recycling dropped from the objective: it is exactly f13+f14 in the\n",
      "        data (max discrepancy 0.001 Tg/a). It is still reported and plotted.\n", sep = "")

PRICE_OK <- !is.na(d[[OBS$Price]]) & TRAIN
S_dat <- cyc_stats(d[[OBS$Price]][PRICE_OK])
S_sd  <- stat_sd(d[[OBS$Price]][PRICE_OK])
B_dat <- band(d[[OBS$Price]][PRICE_OK])
B_sd  <- bp_sd(d[[OBS$Price]][PRICE_OK])
if (LOSS == "moment")
  cat(sprintf("   cycle targets: logamp %.3f (sd %.3f), logfreq %.3f (sd %.3f) -> period %.1f a\n",
              S_dat[[1]], S_sd[[1]], S_dat[[2]], S_sd[[2]], 2 * pi / exp(S_dat[[2]])))
if (LOSS %in% c("bp", "bpamp"))
  cat(sprintf(paste0("   band-pass target: %d points, amplitude %.3f, bootstrap sd %.3f\n",
                     "   PHASE IS CONSTRAINED: the filtered series is matched point by point,\n",
                     "   not just its amplitude and frequency.\n",
                     if (LOSS == "bpamp")
                       "   AMPLITUDE IS ANCHORED too: phase cannot be bought by shrinking the cycle.\n"
                     else ""),
              length(B_dat), sd(B_dat), B_sd))

# build_nll lives at top level (not inside the fit block) because `profile`
# needs the SAME objective the fit minimised. With eff=on the weights are
# computed once and frozen in the state file, so a profile that recomputed them
# would be profiling a subtly different function than the one that was
# optimised, and the resulting curve would not be a likelihood profile at all.
build_nll <- function(dat, effw_fixed = NULL) {
  op <- sim(v0, TOL_HI)
  effw <- effw_fixed %||%
          (if (EFF == "on" && !is.null(op)) eff_weights(op, dat) else
           setNames(rep(1, length(OBS)), names(OBS)))
  pk <- !is.na(dat[[OBS$Price]]) & TRAIN
  list(nll  = mkloss(dat, effw, cyc_stats(dat[[OBS$Price]][pk]),
                     stat_sd(dat[[OBS$Price]][pk]),
                     band(dat[[OBS$Price]][pk]), bp_sd(dat[[OBS$Price]][pk])),
       effw = effw)
}

# =====================================================================
# MODE diag -- local sensitivity and practical identifiability
# =====================================================================
# Brun, Reichert & Kuensch (2001, Water Resour. Res.): collinearity index
# gamma = 1/sqrt(lambda_min) of the normalised sensitivity matrix; gamma > 20
# is the conventional threshold for "not jointly estimable".
# This is a LOCAL analysis. v2 only ever ran it at the reference parameters,
# which says nothing about identifiability at the optimum -- run `at=fit` too
# and check that the estimable subset is stable between the two.
if (MODE == "diag") {
  vbase <- v0
  if (DIAGAT == "fit") {
    SFD <- tagged("state_fit", ".rds")
    if (!file.exists(SFD)) stop("at=fit needs ", SFD, "; run the matching `fit` first.\n",
      "  available: ", paste(list.files(pattern = "^state_fit.*\\.rds$"), collapse = ", "))
    stf <- check_state(readRDS(SFD), SFD)
    vbase[ki] <- stf$par; cat("   sensitivities evaluated AT THE FITTED OPTIMUM\n")
  } else cat("   sensitivities evaluated at the REFERENCE parameters\n")

  JP <- match(PN_ACTIVE, PN)
  vecout <- function(o) unlist(lapply(names(OBS), function(k) {
    ok <- !is.na(d[[OBS[[k]]]]); o[[k]][ok] / SIG[[k]] }))
  o_base <- sim(vbase, TOL_HI)
  if (is.null(o_base)) stop("the base parameters do not solve; cannot run diag")
  y_0 <- vecout(o_base)

  # CENTRAL differences (v2 used forward), swept over three step sizes so that
  # step-dependence of the conclusion is visible rather than assumed away.
  sens <- function(h) {
    S <- vapply(JP, function(j) {
      vp <- vbase; vm <- vbase; vp[j] <- vp[j] + h; vm[j] <- vm[j] - h
      op <- sim(vp, TOL_HI); om <- sim(vm, TOL_HI)
      if (is.null(op) || is.null(om)) rep(NA_real_, length(y_0))
      else (vecout(op) - vecout(om)) / (2 * h)
    }, numeric(length(y_0)))
    colnames(S) <- PN_ACTIVE; S
  }
  gam_of <- function(S, idx) {
    A <- S[, idx, drop = FALSE]
    if (anyNA(A) || any(colSums(A^2) <= 0)) return(Inf)
    A  <- sweep(A, 2, sqrt(colSums(A^2)), "/")
    ev <- eigen(crossprod(A), symmetric = TRUE, only.values = TRUE)$values
    if (min(ev) <= 0) Inf else 1 / sqrt(min(ev))
  }

  subsets <- list()
  for (h in c(1e-3, 1e-4, 1e-5)) {
    S <- sens(h)
    cat(sprintf("\n================ step h = %g ================\n", h))
    if (anyNA(S)) cat("  WARNING: solver failed for ",
                      paste(PN_ACTIVE[apply(is.na(S), 2, any)], collapse = ", "), "\n")
    cat("=== INDIVIDUAL SENSITIVITY (RMS, sigma-scaled) ===\n")
    print(round(sort(apply(S, 2, function(z) sqrt(mean(z^2, na.rm = TRUE))),
                     decreasing = TRUE), 2))
    cat("\n=== PAIRWISE COLLINEARITY (gamma > 20 => not jointly estimable) ===\n")
    np <- length(PN_ACTIVE)
    pr <- do.call(rbind, lapply(1:(np - 1), function(i) do.call(rbind,
          lapply((i + 1):np, function(j)
            data.frame(a = PN_ACTIVE[i], b = PN_ACTIVE[j], g = gam_of(S, c(i, j)))))))
    pr <- pr[is.finite(pr$g), ]
    print(head(transform(pr[order(-pr$g), ], g = round(g, 1)), 10), row.names = FALSE)
    keep <- which(!apply(is.na(S), 2, any))
    cat("\n=== BACKWARD ELIMINATION ===\n")
    while (gam_of(S, keep) > 20 && length(keep) > 2) {
      g  <- vapply(keep, function(j) gam_of(S, setdiff(keep, j)), numeric(1))
      dr <- keep[which.min(g)]
      cat(sprintf("  drop %-7s -> gamma %6.1f\n", PN_ACTIVE[dr], min(g)))
      keep <- setdiff(keep, dr)
    }
    cat("ESTIMABLE SUBSET (gamma =", round(gam_of(S, keep), 1), "):",
        paste(PN_ACTIVE[keep], collapse = ", "), "\n")
    subsets[[as.character(h)]] <- sort(PN_ACTIVE[keep])
  }
  cat("\n=== STEP-SIZE ROBUSTNESS ===\n")
  same <- length(unique(vapply(subsets, paste, character(1), collapse = ","))) == 1
  for (h in names(subsets)) cat(sprintf("  h=%-6s %s\n", h, paste(subsets[[h]], collapse = ", ")))
  core  <- Reduce(intersect, subsets); union <- sort(Reduce(base::union, subsets))
  cat(sprintf("  step-invariant CORE (%d): %s\n", length(core), paste(core, collapse = ", ")))
  cat(sprintf("  union            (%d): %s\n", length(union), paste(union, collapse = ", ")))
  cat(if (same) "  STABLE: the estimable subset does not depend on the step size.\n"
      else paste0("  UNSTABLE: the estimable subset depends on the step size. Report the CORE\n",
                  "  as the defensible estimable set and treat any single-step subset as\n",
                  "  indicative only -- a subset derived from one forward difference at one\n",
                  "  step size is a numerical artefact as much as a property of the model.\n"))
  write_manifest("diag", list(`estimable subsets` = c(
    vapply(names(subsets), function(h) paste0("h=", h, ": ", paste(subsets[[h]], collapse = ", ")),
           character(1)),
    paste("core:", paste(core, collapse = ", ")),
    paste("union:", paste(union, collapse = ", ")))))
}

# =====================================================================
# MODE recover / fit
# =====================================================================
if (MODE %in% c("recover", "fit")) {

  make_synth <- function(rep_i) {
    set.seed(1000L * rep_i + 42L)
    vt <- v0; vt[ki] <- v0[ki] + rnorm(length(ki), 0, .15)
    ot <- sim(vt, TOL_HI)
    if (is.null(ot)) return(NULL)
    dat <- d
    for (k in names(OBS_ALL)) { col <- OBS_ALL[[k]]; ok <- !is.na(d[[col]])
      dat[[col]][ok] <- ot[[k]][ok] + rnorm(sum(ok), 0, NOISEM * SIG[[k]]) }
    # v2 kept the REAL dissipation bound here, so the synthetic truth could
    # violate its own constraint and was then not the minimiser -- the recovery
    # test was measuring the wrong thing. Regenerate it from the truth, placed
    # 25% above so it stays an inactive UPPER BOUND rather than becoming a
    # target (an inactive one-sided constraint carries no information, which is
    # the honest statement of what this series does).
    okd <- !is.na(d[[DCOL]])
    dat[[DCOL]][okd] <- 1.25 * ot$Dcum[okd] + d[[DCOL]][okd][1]
    list(dat = dat, truth = vt[ki])
  }

  build_nll <- function(dat, effw_fixed = NULL) {
    op <- sim(v0, TOL_HI)
    effw <- effw_fixed %||%
            (if (EFF == "on" && !is.null(op)) eff_weights(op, dat) else
             setNames(rep(1, length(OBS)), names(OBS)))
    pk <- !is.na(dat[[OBS$Price]]) & TRAIN
    list(nll  = mkloss(dat, effw, cyc_stats(dat[[OBS$Price]][pk]),
                       stat_sd(dat[[OBS$Price]][pk]),
                       band(dat[[OBS$Price]][pk]), bp_sd(dat[[OBS$Price]][pk])),
         effw = effw)
  }

  # ------------------------------------------------------------------
  # MULTISTART  (v4 -- this is a real multistart; v3's was not)
  # ------------------------------------------------------------------
  # v3 evaluated the objective at N Latin-hypercube points and started
  # Nelder-Mead from whichever scored best. That is start SELECTION: it reports
  # the distribution of STARTING values, and a low objective at a random draw
  # says almost nothing about the basin it sits in. The evidence you actually
  # need -- "many independent optimisations, here is the spread of their FINAL
  # objectives" -- requires optimising from each candidate. That is what this
  # does. It costs STAGE1 iterations per candidate rather than one evaluation,
  # so it is genuinely expensive; it is also the only version of the claim that
  # survives a referee.
  #
  # The spread of final values is a two-for-one: it is the evidence you found
  # the global optimum, AND it is an identifiability diagnostic. Many starts
  # reaching the SAME objective at VERY DIFFERENT parameters is a flat
  # direction, not a coincidence -- and that is reported explicitly below.
  #
  # Resumable: every completed candidate is written to disk, so a killed run
  # picks up where it stopped.
  multistart <- function(nll, truth, ctr = NULL, sd_lhs = 0.8) {
    if (MODE == "recover") return(truth + rnorm(length(ki), 0, .30))

    # v5c: ctr = NULL is the global form (centred on the reference, plus the
    # c x phi coverage grid). ctr = a fitted vector is the LOCAL form: perturb
    # around a known optimum and measure what comes back. The coverage grid is
    # skipped there -- it is deliberately far away, and including it would
    # contaminate a basin-of-attraction statistic with a global search.
    local_mode <- !is.null(ctr)
    if (!local_mode) ctr <- v0[ki]

    cand <- list(); lab <- character(0)
    cand[[1]] <- ctr; lab[1] <- if (local_mode) "centre" else "reference"

    # c x phi grid: with c and phi free the surface has an oscillating basin and
    # a flat one, and they are far apart, so this coarse structure is worth
    # covering deterministically rather than hoping the LHS lands in both.
    if (!local_mode && all(c("c", "phi") %in% KEEP)) {
      for (cc in c(1.0, 2.0, 3.5, 5.0, 7.0)) for (ph in c(0.01, 0.03, 0.08, 0.15)) {
        z <- v0[ki]
        z[match("c", KEEP)]   <- enc("c", cc)
        z[match("phi", KEEP)] <- enc("phi", ph)
        cand[[length(cand) + 1]] <- z
        lab[length(cand)] <- sprintf("c=%.1f,phi=%.2f", cc, ph)
      }
    }

    if (NSTART > 0) {
      set.seed(SEED + 991L)
      L <- vapply(seq_along(ki), function(j)
             (sample(seq_len(NSTART)) - 1 + runif(NSTART)) / NSTART, numeric(NSTART))
      # vapply drops to a vector when NSTART == 1, and L[i, ] then fails. That
      # is exactly the value used for a smoke test, so pin the shape.
      if (!is.matrix(L)) L <- matrix(L, nrow = NSTART)
      for (i in seq_len(NSTART)) {
        cand[[length(cand) + 1]] <-
          ctr + qnorm(pmin(pmax(L[i, ], 1e-4), 1 - 1e-4)) * sd_lhs
        lab[length(cand)] <- sprintf("lhs%02d", i)
      }
    }

    # Optional DEoptim pre-stage. Kept OPTIONAL and off by default: on this
    # problem it needs ~NP*generations = 10*d*200 evaluations to be worth
    # anything, which is the same order as the multistart, and it does not
    # produce the spread-of-final-values diagnostic. Use it if the multistart
    # shows many distinct basins; skip it otherwise.
    if (GLOBAL == "de") {
      if (!requireNamespace("DEoptim", quietly = TRUE))
        stop("global=de needs DEoptim: install.packages(\"DEoptim\")", call. = FALSE)
      cat("  DEoptim pre-stage (this is slow; it is a global search) ...\n")
      lo_b <- v0[ki] - 2.5; hi_b <- v0[ki] + 2.5
      de <- DEoptim::DEoptim(nll, lower = lo_b, upper = hi_b,
              control = DEoptim::DEoptim.control(NP = 10 * length(ki),
                          itermax = 150, trace = 25, strategy = 2))
      cand[[length(cand) + 1]] <- as.numeric(de$optim$bestmem)
      lab[length(cand)] <- "deoptim"
      cat(sprintf("  DEoptim best: %.2f\n", de$optim$bestval))
    }

    MS <- tagged("state_multistart", ".rds")
    ms <- if (file.exists(MS)) check_state(readRDS(MS), MS) else
          list(sig = SIGNATURE, res = NULL, par = list())
    cat(sprintf("  MULTISTART: %d candidates x %d NM iterations each\n",
                length(cand), STAGE1))

    t0 <- Sys.time()
    todo <- setdiff(seq_along(cand), if (is.null(ms$res)) integer(0) else ms$res$id)
    if (length(todo) < length(cand))
      cat(sprintf("  resuming: %d already done, %d to go\n",
                  length(cand) - length(todo), length(todo)))

    one <- function(i) {
      v_start <- nll(cand[[i]])
      f <- if (is.finite(v_start) && v_start < 1e9)
             optim(cand[[i]], nll, method = "Nelder-Mead",
                   control = list(maxit = STAGE1, reltol = RELTOL))
           else list(par = cand[[i]], value = 1e10, convergence = 9L)
      list(id = i, label = lab[i], start = v_start,
           final = f$value, conv = f$convergence, par = f$par)
    }

    # Chunked so state is written every CORES candidates: a killed run loses at
    # most one chunk, not the whole stage.
    chunks <- if (length(todo)) split(todo, ceiling(seq_along(todo) / CORES)) else list()
    for (ch in chunks) {
      out <- if (CORES > 1L) parallel::mclapply(ch, one, mc.cores = min(CORES, length(ch)))
             else lapply(ch, one)
      for (o in out) {
        if (inherits(o, "try-error") || is.null(o$id)) {
          cat("    a worker failed; that candidate is skipped\n"); next }
        ms$res <- rbind(ms$res, data.frame(id = o$id, label = o$label,
                          start = o$start, final = o$final, conv = o$conv))
        ms$par[[as.character(o$id)]] <- o$par
        cat(sprintf("    [%3d/%3d] %-16s start %10.1f -> final %10.2f  (%.1f min)\n",
                    o$id, length(cand), o$label, o$start, o$final,
                    as.numeric(Sys.time() - t0, units = "mins")))
      }
      ms$sig <- SIGNATURE; saveRDS(ms, MS)
      flush.console()
    }

    r <- ms$res[order(ms$res$final), ]
    write.csv(r, tagged("multistart", ".csv"), row.names = FALSE)
    fin <- r$final[is.finite(r$final) & r$final < 1e9]
    cat(sprintf("\n  === MULTISTART: %d/%d candidates feasible ===\n",
                length(fin), nrow(r)))
    if (length(fin)) {
      cat(sprintf("  final objective: best %.2f | q25 %.2f | median %.2f | max %.2f\n",
                  min(fin), quantile(fin, .25), median(fin), max(fin)))
      # How many DISTINCT basins? Count starts landing within 1% of the best.
      # "Same optimum" = within 1% OR within 0.05 absolute, whichever is looser.
      # A pure relative tolerance collapses to nothing as the objective
      # approaches zero, and 0.05 is comfortably above the measured solver
      # noise floor, so genuinely equivalent optima are not split apart.
      tolb <- max(0.01 * abs(min(fin)), 0.05)
      near <- r[is.finite(r$final) & r$final < 1e9 & r$final <= min(fin) + tolb, ]
      cat(sprintf("  %d of %d land within 1%% of the best objective\n",
                  nrow(near), length(fin)))
      if (nrow(near) > 1) {
        P <- do.call(rbind, lapply(as.character(near$id), function(z) ms$par[[z]]))
        sdp <- apply(P, 2, sd)
        cat("  spread of PARAMETERS among those near-equal optima (transformed sd):\n   ")
        cat(paste(sprintf("%s=%.2f", KEEP, sdp), collapse = "  "), "\n")
        flat <- KEEP[sdp > 0.25]
        if (length(flat))
          cat("  FLAT DIRECTIONS: ", paste(flat, collapse = ", "),
              "\n  vary by more than 0.25 in transformed units across optima that are\n",
              "  numerically indistinguishable. That is non-identifiability observed\n",
              "  directly, and it is stronger evidence than any collinearity index.\n", sep = "")
        else
          cat("  No parameter varies by more than 0.25 across the near-equal optima:\n",
              "  the optimum looks locally unique on this evidence.\n", sep = "")
      }
      if (length(fin) > 1 && min(fin) < sort(fin)[2] * 0.9)
        cat("  NOTE the best start is well clear of the runner-up: the surface has\n",
            "  distinct basins, so report the multistart, not a single fit.\n", sep = "")
    }
    best_id <- as.character(r$id[1])
    cat(sprintf("  handing the best (%s, %.2f) to the main optimiser\n\n",
                r$label[1], r$final[1]))
    ms$par[[best_id]]
  }

  run_opt <- function(nll, st, SF, quiet = FALSE) {
    chunk <- if (AUTO) 500L else MAXIT
    t0 <- Sys.time(); ri <- 0L
    repeat {
      ri <- ri + 1L; prev <- st$value
      f  <- optim(st$par, nll, method = "Nelder-Mead",
                  control = list(maxit = chunk, reltol = RELTOL))
      el <- as.numeric(Sys.time() - t0, units = "mins")
      nh <- if (is.null(st$history)) 0L else nrow(st$history)
      st$par <- f$par; st$value <- f$value; st$it <- st$it + chunk
      # v5c: nh is recounted from st$history every pass, so it already grows by
      # one per round. Adding ri on top double-counted and printed rounds
      # 1, 3, 5, ... -- the rounds were not missing, the label was wrong.
      # v5c: carried history (from warm=) has a $link column and these new rows
      # did not, so rbind failed on mismatched columns. Rows written by THIS
      # run are labelled with the state file they are saved into; a state that
      # predates the column gets one before the bind.
      if (!is.null(st$history) && !"link" %in% names(st$history))
        st$history$link <- basename(SF)
      # v5d: `iters` is a BUDGET counter -- it advances by the full chunk every
      # round even when optim() exits early (conv 10, a collapsed simplex) or
      # improves nothing at all, so it is a constant stride and not a measure
      # of work done. optim() reports the real cost in f$counts and it was
      # simply being thrown away. Record it alongside; `iters` keeps its old
      # meaning so states written before this still resume and still plot.
      if (!is.null(st$history) && !"fevals" %in% names(st$history))
        st$history$fevals <- NA_integer_
      st$history <- rbind(st$history, data.frame(round = nh + 1L, iters = st$it,
        value = f$value, gain = prev - f$value, conv = f$convergence,
        minutes = round(el, 2), link = basename(SF),
        fevals = as.integer(f$counts[["function"]]), stringsAsFactors = FALSE))
      saveRDS(st, SF)                    # crash-safe: state saved every round
      if (!quiet) cat(sprintf(
        "[%s] round %d | %d iters | obj %.2f | gain %.3f | conv %d | %.1f min\n",
        MODE, nh + 1L, st$it, f$value, prev - f$value, f$convergence, el))
      flush.console()
      if (!AUTO) break
      # AUTO: keep restarting until it converges AND a fresh restart from its
      # own optimum yields no further improvement. Nelder-Mead collapses its
      # simplex and can report convergence at a point it can still improve on,
      # so a single "conv 0" is NOT sufficient.
      if (f$convergence == 0 && (prev - f$value) < 1e-6) {
        if (!quiet) cat("  CONVERGED: restart from the optimum gave no further improvement.\n")
        break }
      if (el > CAPMIN) { if (!quiet) cat(sprintf(
        "  time cap (%.0f min) reached - rerun to continue.\n", CAPMIN)); break }
    }
    st
  }

  # ---------------- recovery as a simulation study ------------------
  if (MODE == "recover" && REPS > 1) {
    if (AUTO) stop("reps>1 needs a fixed iteration budget, e.g. maxit=4000")
    SF   <- tagged("state_recover_reps", ".rds")
    done <- if (file.exists(SF)) check_state(readRDS(SF), SF) else
            list(sig = SIGNATURE, res = NULL)
    for (i in seq_len(REPS)) {
      if (!is.null(done$res) && i %in% done$res$rep) next
      sy <- make_synth(i)
      if (is.null(sy)) { cat("[rep ", i, "] truth failed to solve; skipped\n", sep = ""); next }
      bl <- build_nll(sy$dat)
      set.seed(SEED + i)
      p0 <- multistart(bl$nll, sy$truth)
      st <- run_opt(bl$nll, list(par = p0, value = bl$nll(p0), it = 0L, sig = SIGNATURE),
                    tempfile(), quiet = TRUE)
      rel <- vapply(seq_along(ki), function(j)
        abs(dec(KEEP[j], st$par[j]) - dec(KEEP[j], sy$truth[j])) /
          max(abs(dec(KEEP[j], sy$truth[j])), 1e-8), numeric(1))
      done$res <- rbind(done$res, data.frame(rep = i, param = KEEP, rel = rel,
        truth = vapply(seq_along(ki), function(j) dec(KEEP[j], sy$truth[j]), numeric(1)),
        est   = vapply(seq_along(ki), function(j) dec(KEEP[j], st$par[j]),   numeric(1)),
        value = st$value))
      done$sig <- SIGNATURE; saveRDS(done, SF)
      cat(sprintf("[rep %d/%d] obj %.1f | median rel.err %.1f%%\n",
                  i, REPS, st$value, 100 * median(rel)))
    }
    r <- done$res
    write.csv(r, tagged("recover_reps", ".csv"), row.names = FALSE)
    cat(sprintf("\n=== RECOVERY OVER %d REPLICATES (noise x%.3g) ===\n",
                length(unique(r$rep)), NOISEM))
    cat("  param     median      q25      q75   frac>25%  frac>100%\n")
    for (nm in KEEP) { z <- r$rel[r$param == nm]
      cat(sprintf("  %-8s %8.1f%% %7.1f%% %7.1f%% %8.2f %10.2f%s\n", nm,
        100 * median(z), 100 * quantile(z, .25), 100 * quantile(z, .75),
        mean(z > .25), mean(z > 1), if (median(z) > .25) "  <--" else "")) }
    cat("\n  Report the per-parameter column, not one median across parameters: a good\n",
        "  overall median routinely hides two or three parameters that are never\n",
        "  recovered, and those are the informative result.\n", sep = "")
    write_manifest("recover", list(replicates =
      paste(length(unique(r$rep)), "completed, written to recover_reps.csv")))

  } else {
  # ---------------- single fit or single recovery -------------------
    SF <- tagged(paste0("state_", MODE), ".rds")
    if (MODE == "recover") {
      sy <- make_synth(1)
      if (is.null(sy)) stop("the synthetic truth failed to solve; change the seed")
      dat <- sy$dat; truth <- sy$truth
    } else { dat <- d; truth <- NULL }

    bl <- build_nll(dat); nll <- bl$nll; effw <- bl$effw
    cat("\n  effective-sample-size weights (applied: ", EFF, ")\n   ", sep = "")
    cat(paste(sprintf("%s=%.2f", names(effw), effw), collapse = "  "), "\n")

    # v5: warm=FILE seeds a NEW state from another fit's parameters and skips
    # multistart entirely. The point is to test a change to the OBJECTIVE
    # (eff=, cycw=) in ~30 minutes instead of re-running a multi-hour
    # multistart, since the old optimum is a perfectly good starting point for
    # a reweighted objective -- it is simply no longer a minimum of it.
    # Nelder-Mead is local, so a warm start is a PROBE, not a headline fit:
    # if the probe says the reweighting works, re-run it with a real
    # multistart before quoting the numbers.
    st <- if (file.exists(SF)) check_state(readRDS(SF), SF) else if (nzchar(WARM)) {
      if (!file.exists(WARM)) stop("warm=", WARM, " does not exist.\n  available: ",
        paste(list.files(pattern = "^state_(fit|recover).*\\.rds$"), collapse = ", "),
        call. = FALSE)
      w <- readRDS(WARM)
      if (is.null(w$par)) stop("warm=", WARM, " has no $par; is it a state file?", call. = FALSE)
      if (!identical(w$sig$keep, KEEP)) stop("warm=", WARM, " was fitted over a different\n",
        "  free set (", paste(w$sig$keep, collapse = ", "), ").\n",
        "  This run wants (", paste(KEEP, collapse = ", "), "). Pass the same free= .",
        call. = FALSE)
      cat("   WARM START from ", WARM, " (its objective there: ",
          sprintf("%.2f", w$value), ")\n", sep = "")
      # v5c: carry the ancestor's descent record forward. Discarding it made
      # the convergence figure a single point whenever a fit was warm-started
      # at (or near) its own optimum -- which is exactly what happens after a
      # profile promotion. The descent is real; it just happened upstream.
      # $link marks where each round came from, so the figure can rule the
      # boundaries and the manifest can name the ancestors.
      hist0 <- w$history
      if (!is.null(hist0) && nrow(hist0)) {
        if (is.null(hist0$link)) hist0$link <- basename(WARM)
        IT0 <- max(hist0$iters)
        cat(sprintf("   carrying %d prior rounds (%d iters) from its history.\n",
                    nrow(hist0), IT0))
      } else { hist0 <- NULL; IT0 <- 0L }
      if (LOCAL > 0) {
        # v5c: LOCAL multistart. The centre is candidate 1, so the returned
        # start can never be worse than the warm file itself.
        cat(sprintf("   LOCAL MULTISTART centred on that fit, sd %.3f (transformed),\n",
                    LOCAL))
        cat(sprintf("   %d perturbed candidates. Coverage grid skipped -- see the v5c note.\n",
                    NSTART))
        set.seed(SEED)
        p0 <- multistart(nll, truth, ctr = w$par, sd_lhs = LOCAL)
        list(par = p0, value = nll(p0), it = IT0, sig = SIGNATURE,
             truth = truth, seed = SEED, history = hist0,
             warm_from = WARM, local_sd = LOCAL)
      } else {
        cat("   multistart SKIPPED. This is a probe -- re-fit with starts= before quoting.\n")
        list(par = w$par, value = nll(w$par), it = IT0, sig = SIGNATURE,
             truth = truth, seed = SEED, history = hist0, warm_from = WARM)
      }
    } else {
      set.seed(SEED)
      p0 <- multistart(nll, truth)
      list(par = p0, value = nll(p0), it = 0L, sig = SIGNATURE,
           truth = truth, seed = SEED, history = NULL)
    }
    cat(sprintf("[%s] starting from objective %.2f after %d iters\n", MODE, st$value, st$it))
    st$truth <- truth; st$effw <- effw
    st <- run_opt(nll, st, SF)

    v <- v0; v[ki] <- st$par
    report_parts(nll, st$par, "final")
    metrics(sim(v, TOL_HI), dat, "fit quality", effw)

    # --- MEASURED numerical noise floor of the objective ---------------
    # v2 asked optim for reltol 1e-10 while evaluating the objective at solver
    # tolerance 1e-5, i.e. demanded a precision far below the objective's own
    # jitter. Rather than assert a safe pairing, measure it.
    TOL_KEEP <- TOL
    L_lo <- nll(st$par)                      # at the tolerance actually used
    TOL  <- 1e-10; L_hi <- nll(st$par)       # near-exact reference
    TOL  <- TOL_KEEP
    jit <- abs(L_lo - L_hi) / max(abs(L_hi), 1e-12)
    cat(sprintf("\n  NUMERICAL NOISE FLOOR: at tol=%.0e the objective is uncertain by %.2e\n  (relative, against a 1e-10 solve); optim reltol is %.0e.\n", TOL, jit, RELTOL))
    if (is.finite(jit) && jit > RELTOL)
      cat("  WARNING: reltol is below the solver noise floor -- the last digits of the\n  objective are solver noise. Loosen reltol, or tighten tol=.\n")

    if (!is.null(truth)) {
      cat("\n  param      true  recovered   rel.err\n"); e <- c()
      for (i in seq_along(ki)) { nm <- KEEP[i]
        tr <- dec(nm, truth[i]); es <- dec(nm, st$par[i])
        ee <- abs(es - tr) / max(abs(tr), 1e-8); e <- c(e, ee)
        cat(sprintf("  %-8s %8.4f %9.4f %8.1f%%%s\n", nm, tr, es, 100 * ee,
                    if (ee > .25) "  <--" else "")) }
      vt <- nll(truth)
      cat(sprintf("\n  median rel.err %.1f%%  |  objective at truth %.2f\n", 100 * median(e), vt))
      if (vt < st$value - 1e-6)
        cat("  NOTE the truth scores BETTER than the recovered optimum. That is an\n  OPTIMISER failure, not an identifiability result. Raise maxit and rerun.\n")
      cat(if (median(e) < .25)
        "  VERDICT: RECOVERY SUCCEEDS. The machinery works, so a poor fit to the\n  REAL data means MODEL MISSPECIFICATION, not optimiser failure.\n"
        else "  VERDICT: RECOVERY FAILS -> identifiability problem remains.\n")
      cat("  This is ONE replicate. Run reps=50 before drawing a conclusion, and\n  noise=0.001 to separate structural from practical non-identifiability.\n")
    }
    write_manifest(MODE, list(history = if (is.null(st$history)) "none" else
      apply(st$history, 1, function(rr)
        paste(sprintf("%s=%s", names(st$history), rr), collapse = " "))))
  }
}

# =====================================================================
# MODE profile -- profile likelihood (Raue et al. 2009, Bioinformatics)
# =====================================================================
# Fix one parameter across a grid, RE-OPTIMISE all the others, plot the
# resulting curve. This is the diagnostic the collinearity index only gestures
# at, and it separates three cases that `diag` cannot tell apart:
#
#   structurally non-identifiable  profile is FLAT -- the data contain no
#                                  information about this parameter at all, and
#                                  no amount of better data or optimisation
#                                  helps; only a reparameterisation does
#   practically non-identifiable   profile rises on one side but never crosses
#                                  the threshold on the other: no finite CI
#   identifiable                   profile crosses the threshold both sides
#
# THRESHOLD CAVEAT, and it matters for what you may claim:
# the chi-square threshold below is exact only for a genuine likelihood. With
# loss=moment the objective is a COMPOSITE likelihood, and with eff=off the
# residuals are strongly autocorrelated (ACF1 up to 1.00, n_eff ~ 2.5 against
# n = 50). Both inflate the curvature, so intervals read off this threshold are
# TOO NARROW -- they are a lower bound on the true width. A Godambe/CLIC
# adjustment would fix it and is not done here. Report the SHAPE of the profile
# as the finding (flat vs curved is robust to the scaling) and label any
# interval as nominal. Running `profile loss=sse` gives a real likelihood and
# is the honest source for a quoted CI, at the price of the flat-line problem.
if (MODE == "profile") {
  SFP <- tagged("state_fit", ".rds")
  if (!file.exists(SFP)) stop("no ", SFP, "; run the matching `fit` first.\n",
    "  available: ", paste(list.files(pattern = "^state_fit.*\\.rds$"), collapse = ", "),
    "\n  (pass the same cost=/loss=/free=/holdout= arguments you fitted with)")
  stf <- check_state(readRDS(SFP), SFP)
  cat("   profiling around the fit in ", SFP, " (objective ", sprintf("%.3f", stf$value),
      ")\n", sep = "")

  WHICH <- if (nzchar(opt$which)) trimws(strsplit(opt$which, ",")[[1]]) else
           intersect(c("c", "phi", "eps_P", "gamma"), KEEP)
  bad <- setdiff(WHICH, KEEP)
  if (length(bad)) stop("not free in this fit: ", paste(bad, collapse = ", "),
                        "\n  free: ", paste(KEEP, collapse = ", "), call. = FALSE)
  cat(sprintf("   profiling %d of %d: %s\n", length(WHICH), length(KEEP),
              paste(WHICH, collapse = ", ")))
  cat(sprintf("   grid %d points, %d NM iters/point\n", PGRID, PIT))
  cat("   half-width (transformed): ",
      paste(sprintf("%s=%.4g", WHICH, vapply(WHICH, span_of, 0)), collapse = "  "),
      "\n", sep = "")
  # v5b: two ways to get a profile that LOOKS informative and is not.
  if (PGRID %% 2 == 0)
    cat("   [!] grid= is EVEN, so no grid point sits at offset zero and the\n",
        "       anchor is interpolated rather than evaluated. Use an odd grid.\n", sep = "")
  # v5c: the identity-transform trap is now a PER-PARAMETER check against that
  # parameter's own fitted magnitude, not a blanket warning. span is raw units
  # for an identity transform, so what matters is span/|value|: a step that is
  # a large fraction of the parameter is a different model, not a local profile.
  for (nm in WHICH[TRANS[WHICH] == "id"]) {
    v_at <- abs(dec(nm, stf$par[[match(nm, KEEP)]]))
    rel  <- if (v_at > 0) span_of(nm) / v_at else Inf
    if (rel > 0.5)
      cat(sprintf(paste0(
        "   [!] %s has an IDENTITY transform: span=%.4g is +/-%.4g in RAW units\n",
        "       against a fitted |%s| = %.4g, i.e. +/-%.0f%%. Every off-centre point\n",
        "       is a different model and 'identifiable' will mean 'width below\n",
        "       resolution'. Use span=%s:%.3g or similar.\n"),
        nm, span_of(nm), span_of(nm), nm, v_at, 100 * rel, nm, 0.15 * v_at))
  }
  cat(sprintf("   estimated cost ~%.0f min at 0.7 s/iteration (~%.0f min on %d workers)\n",
              length(WHICH) * PGRID * PIT * 0.7 / 60,
              ceiling(length(WHICH) / max(1L, min(CORES, length(WHICH)))) *
                PGRID * PIT * 0.7 / 60, min(CORES, length(WHICH))))

  bl <- build_nll(d, effw_fixed = stf$effw); nll <- bl$nll

  PF <- tagged("state_profile", ".rds")
  pf <- if (file.exists(PF)) check_state(readRDS(PF), PF) else
        list(sig = SIGNATURE, res = NULL)
  # v5b: a profile state written before the par column existed still resumes,
  # but its old rows cannot be promoted into a fit state. Mark them NULL rather
  # than refusing to resume -- the grid points themselves are still valid.
  if (!is.null(pf$res) && !"par" %in% names(pf$res)) {
    pf$res$par <- I(vector("list", nrow(pf$res)))
    cat("   NOTE this profile state predates v5b and has no stored parameter\n",
        "        vectors. Points added from here on will have them.\n", sep = "")
  }

  THRESH <- qchisq(0.95, 1) / 2          # 1.921 -- LR interval half-width

  # One parameter's grid must stay SEQUENTIAL -- each point warm-starts from
  # its neighbour, which is what keeps the re-optimisation cheap. Different
  # parameters are independent, so that is the axis to parallelise.
  # v5c: completeness was a bare ROW COUNT (>= PGRID), so a parameter already
  # profiled at some other span was declared done and silently skipped -- the
  # reported interval then came from the old grid while the log showed the new
  # span. Completeness now means "every offset this launch asked for is
  # present", which is what resuming was supposed to mean.
  want_of <- function(nm) seq(-span_of(nm), span_of(nm), length.out = PGRID)
  have_of <- function(nm) if (is.null(pf$res)) numeric(0) else
                            pf$res$offset[pf$res$param == nm]
  n_new <- vapply(WHICH, function(nm) {
    have <- have_of(nm)
    sum(!vapply(want_of(nm), function(d)
          length(have) > 0 && any(abs(have - d) < 1e-9), logical(1)))
  }, 0L)
  todo <- WHICH[n_new > 0L]
  if (length(todo) < length(WHICH))
    cat(sprintf("   resuming: %d of %d parameters already cover this span (%s)\n",
                length(WHICH) - length(todo), length(WHICH),
                paste(setdiff(WHICH, todo), collapse = ", ")))
  for (nm in todo) if (n_new[[nm]] < PGRID)
    cat(sprintf("   %s: %d of %d grid points already present, adding %d at span %.4g\n",
                nm, PGRID - n_new[[nm]], PGRID, n_new[[nm]], span_of(nm)))

  do_param <- function(nm) {
    j   <- match(nm, KEEP)
    ctr <- stf$par[j]
    offs <- seq(-span_of(nm), span_of(nm), length.out = PGRID)
    offs <- offs[order(abs(offs))]       # walk OUTWARD from the optimum, so
                                         # each point warm-starts from its
                                         # nearest neighbour and the optimiser
                                         # never has to travel far
    done <- if (is.null(pf$res)) numeric(0) else pf$res$offset[pf$res$param == nm]
    warm <- list(`-` = stf$par[-j], `+` = stf$par[-j])
    acc <- NULL
    for (dl in offs) {
      if (length(done) && any(abs(done - dl) < 1e-9)) next
      side <- if (dl < 0) "-" else "+"
      fx <- ctr + dl
      # objective as a function of the OTHER parameters, with nm pinned
      sub <- function(rest) { full <- numeric(length(ki))
                              full[j] <- fx; full[-j] <- rest; nll(full) }
      f <- optim(warm[[side]], sub, method = "Nelder-Mead",
                 control = list(maxit = PIT, reltol = RELTOL))
      warm[[side]] <- f$par
      # v5b: keep the FULL parameter vector. Without it, a grid point that
      # beats the stored fit -- which is exactly the signal that the fit was
      # not converged -- is discovered, printed, and then thrown away, and the
      # only way back to it is to re-run the profile. The RDS carries it; the
      # CSV drops it (write.csv cannot serialise a list column).
      full <- numeric(length(ki)); full[j] <- fx; full[-j] <- f$par
      acc <- rbind(acc, data.frame(param = nm, offset = dl,
               value_t = fx, value = dec(nm, fx), nll = f$value,
               conv = f$convergence, par = I(list(full))))
      cat(sprintf("   %-8s %+6.2f  %-10.4g  nll %10.3f\n", nm, dl, dec(nm, fx), f$value))
      flush.console()
    }
    acc
  }

  if (length(todo)) {
    nw <- min(CORES, length(todo))
    if (nw > 1L) cat(sprintf("   running %d parameters across %d workers\n", length(todo), nw))
    out <- if (nw > 1L) parallel::mclapply(todo, do_param, mc.cores = nw)
           else lapply(todo, do_param)
    for (a in out) if (!inherits(a, "try-error") && !is.null(a)) pf$res <- rbind(pf$res, a)
    pf$sig <- SIGNATURE; saveRDS(pf, PF)
  }

  r <- pf$res[order(pf$res$param, pf$res$offset), ]
  # Reference is the LOWEST value seen anywhere, not the stored fit: if a
  # profile point beats the fit, the fit was not converged and every profile
  # drawn against it would be shifted.
  ref <- min(c(r$nll, stf$value), na.rm = TRUE)
  # v5c: 1e-6 ABSOLUTE was below the solver's own noise, so every profile run
  # cried wolf. Three floors, take the largest:
  #   * the ODE solver's noise (relative, measured and stored as $noise),
  #   * optim's own stopping resolution -- reltol is RELATIVE, so the simplex
  #     is allowed to span reltol*|f| when it declares convergence,
  #   * 1% of the chi2(1) threshold: a gap that cannot move a bound by one
  #     part in a hundred is not a convergence failure in any sense that
  #     matters for what gets quoted.
  # Anything smaller gets a one-line note instead of a warning.
  BEAT <- max(10 * (stf$noise %||% 4e-07) * abs(stf$value),
              10 * RELTOL * abs(stf$value),
              0.01 * THRESH, 1e-6)
  # ---- v5d: PRE-REGISTERED PROMOTION RULE ------------------------------
  # Three bands, not two. The old code promoted anything above the solver
  # floor (~0.02 at this objective), which is a rule about NUMERICS and not
  # about inference: a gain that size is real in the sense that the optimiser
  # can see it, and inert in the sense that no reported number moves. Chasing
  # them is an unbounded loop -- each refit finds another, the headline
  # metrics do not change, and there is no principled place to stop. The band
  # between "noise" and "matters" is now named and handled:
  #
  #   gain < BEAT      numerical noise             note only, nothing written
  #   BEAT <= gain     real descent, but below     noted and BANKED to a
  #        < PROMO     the resolution at which     non-promoting state file,
  #                    intervals are quoted        no refit advised
  #   gain >= PROMO    the fit is wrong at a       WARNING, promoted, refit
  #                    scale that moves bounds     before quoting anything
  #
  # PROMO defaults to chi2(1)/2 = 1.921 -- the SAME threshold these profiles
  # are read against. A descent smaller than the resolution of the interval it
  # would shift cannot change a published number, so declining to chase it is
  # a decision, not a concession, PROVIDED the rule was fixed before the
  # output was seen. promote= puts it in the manifest either way, which is
  # what makes that claim checkable rather than merely asserted.
  PROMO_LAB <- switch(opt$promote,
    chi2  = sprintf("chi2(1)/2 = %.3f, the interval resolution", THRESH),
    noise = sprintf("%.4g, the solver/optimiser floor", BEAT),
    sprintf("%s, set explicitly on the command line", opt$promote))
  PROMO <- switch(opt$promote,
    chi2  = THRESH,
    noise = BEAT,
    { v <- suppressWarnings(as.numeric(opt$promote))
      if (!is.finite(v) || v < 0)
        stop("promote= must be chi2, noise, or a non-negative number; got '",
             opt$promote, "'", call. = FALSE)
      v })
  # A promotion threshold below the solver floor would promote noise, which is
  # exactly what the floor exists to prevent. BEAT is a hard lower bound.
  if (PROMO < BEAT) {
    cat(sprintf("   NOTE promote=%s gives %.4g, below the solver floor %.4g. Using the floor.\n",
                opt$promote, PROMO, BEAT))
    PROMO <- BEAT
    PROMO_LAB <- sprintf("%.4g, the solver/optimiser floor -- promote=%s was below it",
                         BEAT, opt$promote)
  }
  cat(sprintf("   promotion rule: a grid point must beat the fit by %.4g (%s)\n",
              PROMO, PROMO_LAB))

  gain <- stf$value - ref                       # >= 0 by construction of ref
  bi   <- which.min(r$nll)
  hasp <- !is.null(r$par[[bi]]) && length(r$par[[bi]]) > 0
  # One writer for both banking branches: only the FILENAME and the advice
  # differ, so the state itself cannot drift between them.
  bank <- function(BF) {
    # v5c: the banked point inherits the fit's history. A promotion is a step
    # in one descent, not the start of a new one.
    ph <- stf$history
    if (!is.null(ph) && nrow(ph) && is.null(ph$link))
      ph$link <- basename(tagged("state_fit", ".rds"))
    # v5d: never clobber an existing seed. The file this would overwrite is
    # normally the ANCESTOR of the current fit and a link in the provenance
    # chain, so losing it costs more than the run that produced it did.
    if (file.exists(BF)) {
      bk <- sub("\\.rds$", sprintf("_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S")), BF)
      if (file.rename(BF, bk))
        cat("  (existing ", BF, " moved aside to ", bk, ")\n", sep = "")
      else stop("could not move ", BF, " aside; refusing to overwrite it", call. = FALSE)
    }
    saveRDS(list(par = r$par[[bi]], value = r$nll[bi],
                 it = if (is.null(ph)) 0L else max(ph$iters),
                 sig = SIGNATURE,
                 effw = stf$effw, truth = stf$truth, seed = SEED,
                 history = ph,
                 warm_from = sprintf("profile %s at offset %+.3f",
                                     r$param[bi], r$offset[bi])), BF)
    BF
  }
  no_par <- function()
    cat("  (that point predates v5b, so its parameter vector was not stored and\n",
        "   cannot be written out. Re-run this profile to capture it.)\n", sep = "")

  PROM_NOTE <- sprintf("rule promote=%s (%.4g); ", opt$promote, PROMO)
  if (gain <= 0) {
    PROM_NOTE <- paste0(PROM_NOTE, "no grid point beat the fit; nothing promoted.")

  } else if (gain < BEAT) {
    cat(sprintf(paste0(
      "\n  (best profile point is %.4g below the stored fit. That is inside the\n",
      "   solver/optimiser resolution of %.4g, so it is not evidence of\n",
      "   non-convergence and has not been promoted.)\n"), gain, BEAT))
    PROM_NOTE <- paste0(PROM_NOTE, sprintf(
      "best point %.4g below the fit, inside the solver floor %.4g; not promoted.",
      gain, BEAT))

  } else if (gain < PROMO) {
    cat(sprintf(paste0(
      "\n  A profile point (%.3f) beats the stored fit (%.3f) by %.4g.\n",
      "  That clears the solver floor (%.4g), so the descent is REAL. It is below\n",
      "  the promotion threshold of %.4g (%s).\n",
      "  A gain of %.4g cannot move any bound read off these profiles by more than\n",
      "  itself, and those bounds are nominal already, so a refit would change no\n",
      "  number that reaches the text.\n",
      "  NOT PROMOTED. This is the pre-registered rule, not a failure to converge --\n",
      "  the profiles below are still drawn against the better value (%.3f), which\n",
      "  is the correct reference whether or not the fit is ever rerun.\n"),
      ref, stf$value, gain, BEAT, PROMO, PROMO_LAB, gain, ref))
    if (!hasp) { no_par()
      PROM_NOTE <- paste0(PROM_NOTE, sprintf(
        "best point %.4g below the fit (sub-threshold); parameter vector unavailable.", gain))
    } else {
      SB <- bank(tagged("state_profile_subthreshold", ".rds"))
      cat(sprintf(paste0(
        "  Banked to %s (%s at offset %+.3f) so the point is not lost.\n",
        "  It is NOT named state_profile_best and nothing will warm-start from it\n",
        "  by accident. If you later decide to chase it anyway:\n",
        "      Rscript run_all.R profile <same args> promote=noise\n"),
        SB, r$param[bi], r$offset[bi]))
      PROM_NOTE <- paste0(PROM_NOTE, sprintf(
        "best point %.4g below the fit (%s at offset %+.3f), above the solver floor %.4g but below threshold; banked to %s, NOT promoted, fit NOT rerun.",
        gain, r$param[bi], r$offset[bi], BEAT, SB))
    }

  } else {
    cat(sprintf(paste0("\n  WARNING: a profile point (%.3f) BEATS the stored fit (%.3f) by %.4g,\n",
        "  which exceeds the promotion threshold %.4g (%s).\n",
        "  The fit was not converged at a scale that MOVES reported bounds. Profiles\n",
        "  are drawn against the better value, but rerun `fit maxit=auto` before\n",
        "  quoting anything from them.\n"),
        ref, stf$value, gain, PROMO, PROMO_LAB))
    # v5b: write that better point out as a seedable state file. Nelder-Mead in
    # 12 dimensions collapses along flat ridges and reports convergence at a
    # point it can still improve on; a profile that steps off the ridge finds
    # the improvement. Restarting the fit in place will NOT find it, because
    # that is the same simplex in the same place. Seeding from here will.
    if (!hasp) { no_par()
      PROM_NOTE <- paste0(PROM_NOTE, sprintf(
        "best point %.4g below the fit (above threshold); parameter vector unavailable.", gain))
    } else {
      BF <- bank(tagged("state_profile_best", ".rds"))
      cat(sprintf(paste0("  wrote %s (%s at offset %+.3f).\n",
          "  Continue from it with:\n",
          "      Rscript run_all.R fit <same args> maxit=auto warm=%s\n",
          "  after moving the current %s aside -- warm= is only read when no\n",
          "  state file exists.\n"),
          BF, r$param[bi], r$offset[bi], BF, tagged("state_fit", ".rds")))
      PROM_NOTE <- paste0(PROM_NOTE, sprintf(
        "best point %.4g below the fit (%s at offset %+.3f) EXCEEDS threshold; promoted to %s, refit required before quoting.",
        gain, r$param[bi], r$offset[bi], BF))
    }
  }
  r$dnll <- r$nll - ref
  # drop the list column: write.csv cannot serialise it. The RDS keeps it.
  write.csv(r[, setdiff(names(r), "par")], tagged("profile", ".csv"),
            row.names = FALSE)

  cat("\n  === PROFILE LIKELIHOOD ===\n")
  cat("  threshold: delta-nll = 1.921 (chi2(1) 95%), NOMINAL only -- see the\n")
  cat("  header note on composite likelihood and residual autocorrelation.\n\n")
  cat("  param      MLE      lower      upper   verdict\n")
  verd <- character(0)
  for (nm in unique(r$param)) {
    z <- r[r$param == nm, ]
    z <- z[order(z$offset), ]
    # Anchor at the profile MINIMUM, not the grid centre. They coincide only if
    # the stored fit is exactly the optimum; whenever it is not, walking
    # outward from the centre searches the wrong direction on one side and
    # returns an "interval" that need not even contain the MLE.
    imin <- which.min(z$dnll)
    at_edge <- imin == 1L || imin == nrow(z)
    lo_s <- z[seq_len(imin), ]; hi_s <- z[imin:nrow(z), ]
    bound <- function(s, dir) {
      s <- if (dir < 0) s[order(-s$offset), ] else s[order(s$offset), ]
      k <- which(s$dnll > THRESH)
      if (!length(k)) return(NA_real_)          # never crosses on this side
      i <- k[1]
      if (i == 1L) return(dec(nm, s$value_t[1]))
      x0 <- s$value_t[i - 1]; x1 <- s$value_t[i]
      y0 <- s$dnll[i - 1];    y1 <- s$dnll[i]
      if (!is.finite(y1 - y0) || isTRUE(all.equal(y1, y0))) return(dec(nm, x1))
      dec(nm, x0 + (THRESH - y0) * (x1 - x0) / (y1 - y0))   # linear in TRANSFORMED space
    }
    lo_b <- bound(lo_s, -1); hi_b <- bound(hi_s, 1)
    rng  <- diff(range(z$dnll, na.rm = TRUE))
    # Three cases, not two. A profile that rises but never crosses within the
    # grid is NOT the same as a flat one: it may simply mean span= was too
    # narrow. Calling that "structurally non-identifiable" would be a much
    # stronger claim than the evidence supports, so it is reported as what it
    # is -- no crossing within the span searched -- with the number attached so
    # the reader can see how close it got.
    # v5c: report the range the GRID ACTUALLY COVERS, not the span= of this
    # launch. A resumed state can hold points added at a different span, and
    # "no crossing within span=0.10" would then be a false statement about
    # what was searched.
    cov <- max(abs(z$offset), na.rm = TRUE)
    v <- if (rng < 0.1 * THRESH)
           sprintf("STRUCTURALLY non-identifiable (flat: max delta %.3f)", rng)
         else if (is.na(lo_b) && is.na(hi_b))
           sprintf("no crossing within +/-%.4g (max delta %.2f) -- practically non-identifiable, or widen span=", cov, rng)
         else if (is.na(lo_b)) sprintf("one-sided: no LOWER bound within +/-%.4g", cov)
         else if (is.na(hi_b)) sprintf("one-sided: no UPPER bound within +/-%.4g", cov)
         else "identifiable"
    # v5c: an interval much narrower than the grid step is not a MEASURED
    # width -- it is a straight-line interpolation between the anchor and its
    # immediate neighbour, and "identifiable" then means "narrower than I can
    # resolve". Report the half-width in grid steps and say what span would
    # actually resolve it (~3x the half-width, the ratio that worked for c).
    hw <- NA_real_
    if (!is.na(lo_b) && !is.na(hi_b))
      hw <- max(abs(enc(nm, hi_b) - z$value_t[imin]),
                abs(z$value_t[imin] - enc(nm, lo_b)))
    step <- if (nrow(z) > 1) min(diff(sort(z$offset))) else NA_real_
    under <- is.finite(hw) && is.finite(step) && hw < step
    cat(sprintf("  %-8s %8.4g %10s %10s   %s%s\n", nm, dec(nm, z$value_t[imin]),
                if (is.na(lo_b)) "-inf" else sprintf("%.4g", lo_b),
                if (is.na(hi_b)) "+inf" else sprintf("%.4g", hi_b), v,
                if (at_edge) "  [!] minimum AT GRID EDGE: widen span= or refit" else ""))
    if (under)
      cat(sprintf(paste0(
        "           [!] half-width %.3g is %.2f of the grid step %.3g: the bound is\n",
        "               INTERPOLATED, not measured. Re-profile at span=%s:%.3g\n"),
        hw, hw / step, step, nm, 3 * hw))
    verd <- c(verd, sprintf("%s: %s%s%s", nm, v,
                            if (at_edge) " [minimum at grid edge]" else "",
                            if (under) sprintf(" [width %.2f of grid step -- interpolated]",
                                               hw / step) else ""))
  }
  cat("\n  A flat profile is a RESULT, not a failure: it says the data cannot\n",
      "  speak to that parameter, which is exactly the claim you need to make\n",
      "  about c / phi / eps_P if that is how they come out.\n", sep = "")

  np <- length(unique(r$param))
  png(tagged("profile", ".png"), width = 400 * min(np, 3),
      height = 380 * ceiling(np / 3), res = 120)
  op <- par(mfrow = c(ceiling(np / 3), min(np, 3)), mar = c(4.2, 4.2, 2.5, 1))
  for (nm in unique(r$param)) {
    z <- r[r$param == nm, ]; z <- z[order(z$value), ]
    plot(z$value, z$dnll, type = "b", pch = 16, cex = .7, col = "firebrick",
         xlab = nm, ylab = expression(Delta ~ "nll"), main = nm,
         ylim = range(c(0, z$dnll, THRESH * 1.3), na.rm = TRUE),
         log = if (TRANS[[nm]] == "log") "x" else "")
    abline(h = THRESH, lty = 2, col = "grey40")
    abline(v = dec(nm, stf$par[match(nm, KEEP)]), lty = 3, col = "steelblue")
    mtext("dashed: nominal 95% threshold", side = 3, line = -1.1, cex = .6, col = "grey40")
  }
  par(op); dev.off()
  cat("\nwrote ", tagged("profile", ".csv"), ", ", tagged("profile", ".png"), "\n", sep = "")
  write_manifest("profile", list(verdicts = verd,
    promotion = PROM_NOTE,
    note = "threshold nominal; composite likelihood + autocorrelated residuals => intervals too narrow"))
}

# =====================================================================
# MODE plot
# =====================================================================
if (MODE == "plot") {
  # v5b: state=FILE re-plots ANY saved fit, including one written before
  # adjust_s0 existed, without renaming it. The output suffix is taken from the
  # state's own filename, so re-plotting an archived fit cannot overwrite the
  # figures of the current one.
  SFP <- if (nzchar(STATE)) STATE else tagged("state_fit", ".rds")
  if (!file.exists(SFP)) stop("no ", SFP,
    if (nzchar(STATE)) "; check the state= path.\n" else "; run the matching `fit` first.\n",
    "  available: ", paste(list.files(pattern = "^state_fit.*\\.rds$"), collapse = ", "),
    "\n  (pass the same cost=/loss=/redundant= arguments you fitted with,",
    " or point at the file with state=)")
  if (nzchar(OUTSFX)) {
    CFG <- if (startsWith(OUTSFX, "__")) OUTSFX else paste0("__", OUTSFX)
    cat("   output suffix set by out=: '", CFG, "'\n", sep = "")
  } else if (nzchar(STATE)) {
    stem <- sub("\\.rds$", "", basename(SFP))
    CFG  <- if (grepl("^state_fit", stem)) sub("^state_fit", "", stem) else
            paste0("__", gsub("[^A-Za-z0-9_.-]+", "_", stem))
    cat("   output suffix taken from the state file: '", CFG, "'\n", sep = "")
    # A copy of a state file kept in another directory derives the SAME suffix
    # as the live one, so re-plotting an archive would overwrite the current
    # figures. Say so before doing it; out= overrides.
    if (normalizePath(dirname(SFP), mustWork = FALSE) !=
        normalizePath(".", mustWork = FALSE) && file.exists(tagged("fit_v2", ".png")))
      cat("   [!] ", tagged("fit_v2", ".png"), " already exists and WILL BE OVERWRITTEN.\n",
          "       This state file lives elsewhere but derives the same suffix. Pass\n",
          "       out=SUFFIX to write somewhere else, e.g. out=archive.\n", sep = "")
  }
  st <- readRDS(SFP)
  cat("   reading ", SFP, "\n", sep = "")
  # Take loss/cost/redundant from the STATE, not the command line, so a figure
  # can never be labelled with a different objective than the one that made it.
  # v5b: the y0 CONVENTION comes from the state too, and this one is not
  # cosmetic. y0 is part of the theta -> trajectory map, so re-plotting a
  # placeholder-y0 fit under data-derived stocks would draw a trajectory those
  # parameters were never at -- silently, with no error and no visible clue.
  # sig_norm() supplies "placeholder" for files written before the field
  # existed, which is what they must have been.
  if (!is.null(st$sig)) {
    sg <- sig_norm(st$sig)
    COST <- sg$cost; LOSS <- sg$loss; REDUND <- sg$redundant
    KEEP <- sg$keep; ki <- match(KEEP, PN)
    OBS  <- if (REDUND == "drop") OBS_ALL[setdiff(names(OBS_ALL), REDUNDANT)] else OBS_ALL
    was  <- ADJ_S0
    ADJ_S0 <- identical(sg$y0, "data-derived")
    cat("   settings taken from the saved fit: cost=", COST, " loss=", LOSS,
        " redundant=", REDUND, " adjust_s0=", tolower(ADJ_S0),
        if (is.null(st$sig$y0)) "  (inferred: pre-v5b file)" else "", "\n", sep = "")
    if (was != ADJ_S0)
      cat("   NOTE adjust_s0=", tolower(was), " on the command line was OVERRIDDEN by the\n",
          "        state file. The initial-stock convention is part of the objective,\n",
          "        so the fit's own setting is the only one that reproduces it.\n", sep = "")
  } else {
    cat("   WARNING this state file has no signature, so cost/loss/free and the\n",
        "        initial-stock convention cannot be recovered from it. Falling back\n",
        "        to the command line (adjust_s0=", tolower(ADJ_S0), "). If the curves\n",
        "        look wrong, that is why.\n", sep = "")
  }
  effw <- st$effw %||% setNames(rep(1, length(OBS)), names(OBS))

  v <- v0; v[ki] <- st$par
  o <- sim(v, TOL_HI); o_ref <- sim(v0, TOL_HI)
  if (is.null(o)) stop("the fitted parameters no longer solve; the state file may be stale")
  cat(sprintf("fitted objective %.1f  (loss=%s, cost=%s)\n", st$value, LOSS, COST))
  if (HOLDOUT > 0) {
    # The whole point of the exercise is the gap between these two tables.
    metrics(o, d, sprintf("IN-SAMPLE  (<= %d)", HOLDOUT), effw, win = TRAIN)
    metrics(o, d, sprintf("OUT-OF-SAMPLE  (> %d)  <-- the forecast", HOLDOUT),
            effw, win = TEST)
    okp <- !is.na(d[[OBS_ALL$Price]])
    tpk <- function(x) sum(diff(sign(diff(x))) != 0)
    cat(sprintf("\n  FORECAST CHECK: price turning points %d (model) vs %d (data) in %d..%d\n",
                tpk(o$Price[TEST & okp]), tpk(d[[OBS_ALL$Price]][TEST & okp]),
                HOLDOUT + 1L, max(d$Year)))
    cat("  Timing matters more than amplitude here: the claim is that the cycle is\n",
        "  ENDOGENOUS, so a forecast that turns at roughly the right time is the\n",
        "  evidence, and one that misses the turn is a real negative result.\n", sep = "")
  } else {
    metrics(o_ref, d, "REFERENCE parameters")
    metrics(o, d, "FITTED parameters", effw)
  }

  ttl <- c(Price = "Price (bn$/Tg)", Extraction = "Extraction (Tg/a)", f2 = "f2 (Tg/a)",
           Recycling = "Recycling R (Tg/a)", s4 = "s4 in-use (Tg)", s8 = "s8 landfill (Tg)",
           f9 = "f9 EoL flow (Tg/a)", semis = "semis (Tg/a)", f3 = "f3 new scrap (Tg/a)",
           f13 = "f13 direct melt (Tg/a)", f14 = "f14 to refining (Tg/a)")
  ser <- names(OBS_ALL)                       # plot ALL series, fitted or not
  png(tagged("fit_v2", ".png"), width = 1500, height = 1350, res = 130)
  op <- par(mfrow = c(4, 3), mar = c(4, 4.2, 2.5, 1))
  for (k in ser) { col <- OBS_ALL[[k]]; yr <- d$Year
    rng <- range(c(d[[col]], o[[k]], o_ref[[k]]), na.rm = TRUE)
    plot(yr, d[[col]], type = "n", xlab = "", ylab = "",
         main = paste0(ttl[[k]], if (!k %in% names(OBS)) "  [not fitted]" else ""), ylim = rng)
    if (HOLDOUT > 0) {                  # shade the forecast period
      rect(HOLDOUT + .5, rng[1] - abs(rng[1]) - 1e3, max(yr) + 1,
           rng[2] + abs(rng[2]) + 1e3, col = "grey93", border = NA)
      abline(v = HOLDOUT + .5, col = "grey55", lty = 2)
      box() }
    points(yr, d[[col]], pch = 16, cex = .55, col = "grey25")
    lines(yr, o_ref[[k]], col = "grey65", lwd = 1.6, lty = 2)
    lines(yr, o[[k]], col = "firebrick", lwd = 2) }
  plot.new(); legend("center", c("Gloser data (rebuilt)", "reference parms", "fitted"),
    pch = c(16, NA, NA), lty = c(NA, 2, 1), lwd = c(NA, 1.6, 2),
    col = c("grey25", "grey65", "firebrick"), bty = "n", cex = 1.05)
  par(op); dev.off()

  # Residual diagnostics: the figure that shows whether the independent-Gaussian
  # error assumption behind the point-wise term is tenable. It generally is not,
  # which is why n_eff is reported and why intervals computed from this
  # objective without correction would be too narrow.
  png(tagged("fit_v2_residuals", ".png"), width = 1500, height = 1150, res = 130)
  op <- par(mfrow = c(3, 4), mar = c(4, 4, 2.5, 1))
  for (k in ser) { col <- OBS_ALL[[k]]; ok <- !is.na(d[[col]])
    r <- d[[col]][ok] - o[[k]][ok]
    plot(d$Year[ok], r, type = "h", col = "grey40", xlab = "", ylab = "residual", main = k)
    abline(h = 0, col = "firebrick")
    mtext(sprintf("ACF1 %+.2f", stats::cor(r[-1], r[-length(r)])),
          side = 3, line = -1.2, cex = .7) }
  okp <- !is.na(d[[OBS_ALL$Price]])
  acf(d[[OBS_ALL$Price]][okp] - o$Price[okp], main = "price residual ACF")
  par(op); dev.off()

  # ---- Q-Q of residuals ----------------------------------------------
  # The ACF panel above tests INDEPENDENCE; this tests the Gaussian SHAPE the
  # point-wise term assumes, and tells you whether the error is broad-based or
  # carried by a handful of years. Residuals are scaled by their own RMSE, so
  # what is being read is shape, not size.
  png(tagged("resid_qq", ".png"), width = 1750, height = 760, res = 130)
  op <- par(mfrow = c(3, 4), mar = c(4, 4, 2.6, .8))
  for (k in ser) { col <- OBS_ALL[[k]]; ok <- !is.na(d[[col]])
    r <- d[[col]][ok] - o[[k]][ok]; z <- r / sqrt(mean(r^2))
    qqnorm(z, main = k, pch = 16, cex = .6, col = "firebrick",
           xlab = "theoretical quantile", ylab = "residual / RMSE")
    qqline(z, col = "grey40", lty = 2)
    if (length(z) >= 3 && length(z) <= 5000) {
      sw <- suppressWarnings(shapiro.test(z)$p.value)
      mtext(sprintf("Shapiro-Wilk p = %.3f", sw), side = 3, line = -1.2,
            cex = .62, col = if (sw < .05) "firebrick" else "grey35") }
    if (k == "Price") { w <- order(abs(z), decreasing = TRUE)[1:3]
      q <- qqnorm(z, plot.it = FALSE)
      text(q$x[w], q$y[w], d$Year[ok][w], pos = 4, cex = .68, col = "grey20") } }
  par(op); dev.off()

  # ---- residual against fitted ----------------------------------------
  # A lowess that drifts monotonically is MEAN-STRUCTURE misspecification, not
  # a distributional problem. Series with a loose sigma are barely policed by
  # the objective and are the ones that tend to drift.
  png(tagged("resid_vs_fitted", ".png"), width = 1750, height = 760, res = 130)
  op <- par(mfrow = c(3, 4), mar = c(4, 4, 2.6, .8))
  for (k in ser) { col <- OBS_ALL[[k]]; ok <- !is.na(d[[col]])
    fv <- o[[k]][ok]; r <- d[[col]][ok] - fv
    plot(fv, r, pch = 16, cex = .6, col = "firebrick", main = k,
         xlab = "fitted", ylab = "residual")
    abline(h = 0, col = "grey40", lty = 2)
    lines(lowess(fv, r), col = "steelblue", lwd = 1.6) }
  par(op); dev.off()

  # =====================================================================
  # PAPER FIGURES -- Gambaro et al. (2025), Figures 3 to 10
  # =====================================================================
  # Deliberately NOT the diagnostic panels above. No titles (the LaTeX caption
  # carries the description), panels lettered a), b), ... so the caption can
  # refer to them, and axis labels in the "quantity, symbol (unit)" form.
  #
  # The two kinds of observation in the rebuilt dataset are drawn differently,
  # because they are not equally good:
  #
  #   CONTINUOUS BLACK LINE   E, f2, semis and the two price series. Printed
  #                           numbers (Gloser SI Table S2; World Bank; Cortez
  #                           et al.), one per year, no digitisation anywhere.
  #   BLACK DOTS, ONE A YEAR  R, f3, f9, f13, f14, s4, s8. Traced off Gloser's
  #                           Figures 2, 4 and 5. Joining a digitised trace
  #                           into a curve asserts a between-point precision
  #                           the source does not have, and these series carry
  #                           the loosest sigmas in SIG for exactly that reason.
  #
  # The red curve is THIS fit, not the paper's reference parameters, so the
  # chapter's figures and its metrics table come from the same theta. Say so
  # in the captions: the published Figures 3-6 are reference-case runs, these
  # are fitted, and that is the substantive difference between the two.
  if (PAPER) {
    PFIG <- character(0)
    fig <- function(stem, w, h) {
      f <- tagged(stem, if (PDEV == "pdf") ".pdf" else ".png")
      if (PDEV == "pdf") pdf(f, width = w, height = h, onefile = FALSE)
      else png(f, width = round(w * 160), height = round(h * 160), res = 160)
      PFIG <<- c(PFIG, f); invisible(f)
    }
    PPAR <- function(...) par(mar = c(4.2, 5.4, 2.2, 1.2), mgp = c(3, .8, 0),
                              cex.lab = 1.15, cex.axis = 1, ...)
    panlab  <- function(txt) mtext(txt, side = 3, line = .4, adj = 0,
                                   cex = 1.1, font = 2)
    ax      <- function(ylab) { title(xlab = "Year", mgp = c(2.6, 1, 0))
                                title(ylab = ylab,  mgp = c(3.6, 1, 0)) }
    hosplit <- function() if (HOLDOUT > 0) abline(v = HOLDOUT + .5,
                                                  col = "grey55", lty = 3)

    EXACT <- c("Price", "Extraction", "f2", "semis")   # printed, not digitised
    YL <- list(
      Price      = expression("copper price, " * italic(P) * " (bn$/Tg)"),
      Extraction = expression("mining rate, " * italic(E) * " (Tg/a)"),
      Recycling  = expression("recycling rate, " * italic(R) * " (Tg/a)"),
      s4         = expression("in-use stock, " * italic(s)[4] * " (Tg)"),
      s8         = expression("landfill stock, " * italic(s)[8] * " (Tg)"),
      f9         = expression("end-of-life flow, " * italic(f)[9] * " (Tg/a)"),
      f2         = expression("refining flow, " * italic(f)[2] * " (Tg/a)"),
      semis      = expression("semis fabrication, " * italic(f)[8] +
                              italic(f)[3] * " (Tg/a)"),
      f3         = expression("new scrap, " * italic(f)[3] * " (Tg/a)"),
      f13        = expression("direct-melt old scrap, " * italic(f)[13] * " (Tg/a)"),
      f14        = expression("old scrap to refining, " * italic(f)[14] * " (Tg/a)"))

    # Put the legend where the curves are not. paper1_plotting.R hard-coded
    # legend coordinates, which land on top of the data the moment a refit
    # moves a curve -- and every figure here comes from a refit.
    corner <- function(x, ys) {
      ux <- range(x, na.rm = TRUE); uy <- range(unlist(ys), na.rm = TRUE)
      if (!all(is.finite(c(ux, uy))) || diff(ux) <= 0 || diff(uy) <= 0)
        return("topleft")
      fx <- (x - ux[1]) / diff(ux)
      fy <- lapply(ys, function(y) (y - uy[1]) / diff(uy))
      cnt <- function(bx, by) sum(vapply(fy, function(y)
        sum(fx >= bx[1] & fx <= bx[2] & y >= by[1] & y <= by[2], na.rm = TRUE),
        numeric(1)))
      z <- c(topleft     = cnt(c(0, .45), c(.55, 1)),
             topright    = cnt(c(.55, 1), c(.55, 1)),
             bottomleft  = cnt(c(0, .45), c(0, .45)),
             bottomright = cnt(c(.55, 1), c(0, .45)))
      names(z)[which.min(z)]
    }

    yr <- d$Year
    # ---- one reference-case panel: data in black, this fit in red -------
    pan <- function(k, lab) {
      cn <- OBS_ALL[[k]]
      plot(yr, d[[cn]], type = "n", xlab = "", ylab = "",
           ylim = range(c(d[[cn]], o[[k]]), na.rm = TRUE))
      ax(YL[[k]]); hosplit()
      if (k %in% EXACT) lines(yr, d[[cn]], col = "black", lwd = 2)
      else              points(yr, d[[cn]], pch = 16, cex = .7, col = "black")
      lines(yr, o[[k]], col = "red", lwd = 3)
      legend(corner(yr, list(d[[cn]], o[[k]])), bty = "n", cex = .95,
             legend = c("Gl\u00f6ser et al. (2013)", "Model output"),
             col = c("black", "red"),
             lty = if (k %in% EXACT) c(1, 1) else c(NA, 1),
             pch = if (k %in% EXACT) c(NA, NA) else c(16, NA),
             lwd = c(2, 3))
      panlab(lab)
    }

    # Figure 5 -- copper prices. One panel, so no panel letter.
    # Three series fill this panel corner to corner, so corner() has no clear
    # quadrant to find. Band the legend across the top instead and buy the
    # room for it by extending ylim, rather than letting it sit on a curve.
    fig("paper_prices", 8.5, 5.4); op <- PPAR(mfrow = c(1, 1))
    cw <- OBS_ALL$Price; cc <- "Real Prices (Cortez et al., 2018) $/Tg billion"
    pry <- range(c(d[[cw]], d[[cc]], o$Price), na.rm = TRUE)
    plot(yr, d[[cw]], type = "n", xlab = "", ylab = "",
         ylim = c(pry[1], pry[2] + .15 * diff(pry)))
    ax(YL$Price); hosplit()
    lines(yr, d[[cw]], col = "black", lwd = 2)
    lines(yr, d[[cc]], col = "black", lwd = 2, lty = 2)
    lines(yr, o$Price, col = "red", lwd = 3)
    legend("top", horiz = TRUE, bty = "n", cex = .95, seg.len = 2.2,
           x.intersp = .7, text.width = NA,
           legend = c("World Bank", "Cortez et al. (2018)", "Model output"),
           col = c("black", "black", "red"), lty = c(1, 2, 1), lwd = c(2, 2, 3))
    par(op); dev.off()

    # Figure 6 -- supply flows: E (a) and R (b)
    fig("paper_supply", 11, 4.8); op <- PPAR(mfrow = c(1, 2))
    pan("Extraction", "a)"); pan("Recycling", "b)")
    par(op); dev.off()

    # Figure 3 -- anthropogenic stocks: s4 (a) and s8 (b)
    fig("paper_stocks", 11, 4.8); op <- PPAR(mfrow = c(1, 2))
    pan("s4", "a)"); pan("s8", "b)")
    par(op); dev.off()

    # Figure 4 -- anthropogenic flows: f9 (a) and f2 (b)
    fig("paper_flows", 11, 4.8); op <- PPAR(mfrow = c(1, 2))
    pan("f9", "a)"); pan("f2", "b)")
    par(op); dev.off()

    # NEW -- the four series the rebuilt dataset added, which the paper could
    # not show. semis is EXACT and constrains f8 + f3; f13 and f14 are what
    # make gamma estimable instead of assumed at 1/3.
    fig("paper_newflows", 11, 8.6); op <- PPAR(mfrow = c(2, 2))
    pan("semis", "a)"); pan("f3", "b)"); pan("f13", "c)"); pan("f14", "d)")
    par(op); dev.off()

    # ---- Figures 7 to 10: the mining lead time --------------------------
    # tau_E is FIXED (it is taken from the industry data, not estimated), so
    # it is not in PN and cannot be moved through v. Override it on the
    # parameter LIST instead. y0 does not depend on tau_E, so data_y0() is
    # unaffected and these runs differ from the fit in that one number alone.
    tau_ref <- base$tau_E
    if (!(PTAUS[2] < tau_ref && PTAUS[3] > tau_ref))
      cat("   [!] taus= does not straddle the reference tau_E = ", tau_ref,
          ";\n       the 'shorter' and 'longer' panels will not mean that.\n", sep = "")

    # ---- horizon -------------------------------------------------------
    # end_year runs these panels past the last observation. What that is NOT:
    #
    #   * a forecast. funGDP is approxfun(..., rule = 2), so beyond the data
    #     the GDP growth forcing is HELD FLAT at its final observed value. The
    #     demand path after that year is an assumption, not an input.
    #   * validated. Under cost=slade the anchor is C0*exp(c1 t + c2 t^2) with
    #     c2 > 0, which is a local fit over 1961-2022 and diverges when
    #     extrapolated far. The further out, the more the level is driven by
    #     that extrapolation rather than by the cycle.
    #
    # What it IS: a statement about the delay-induced OSCILLATION -- period,
    # amplitude and phase are properties of theta and tau_E, and reading them
    # off a longer window is legitimate in a way that reading the level is
    # not. Report timing, not magnitudes, from anything right of the rule.
    Y1   <- min(d$Year)
    PEND <- if (ENDYR > 0L) ENDYR else max(d$Year)
    if (PEND < max(d$Year)) {
      cat("   [!] end_year=", PEND, " is inside the data window; using ",
          max(d$Year), " instead.\n", sep = ""); PEND <- max(d$Year)
    }
    tt  <- seq(0, PEND - Y1, 1)
    yrx <- Y1 + tt
    EXT <- PEND > max(d$Year)

    dsim <- function(tau) {
      p <- setp(v); p$tau_E <- tau
      r <- NULL
      invisible(capture.output(suppressWarnings(
        r <- try(dmmcm_v2(p, data_y0(p), tt, funGDP, COST,
                          eps_sw = .02, atol = TOL_HI, rtol = TOL_HI,
                          maxsteps = 3000), silent = TRUE))))
      if (inherits(r, "try-error") || is.null(r) || any(!is.finite(as.matrix(r))))
        NULL else r
    }
    cat(sprintf("   paper figures: re-solving at tau_E = %s (reference %g) to %d\n",
                paste(PTAUS, collapse = ", "), tau_ref, PEND))
    if (EXT)
      cat("       beyond ", max(d$Year), " the GDP forcing is held flat at its\n",
          "       final observed value and the cost anchor is extrapolated --\n",
          "       read the oscillation, not the level. A rule marks the join.\n", sep = "")
    dr  <- lapply(PTAUS, dsim)
    bad <- PTAUS[vapply(dr, is.null, logical(1))]
    if (length(bad))
      cat("   [!] no solution at tau_E = ", paste(bad, collapse = ", "),
          " under the fitted parameters;\n       those curves are omitted",
          " rather than drawn from a failed solve.\n", sep = "")
    # The reference curve has to be re-solved on the SAME grid, otherwise it
    # is a 62-point vector plotted against a longer x. o is reused untouched
    # when the horizon is the data window, so the unextended figures are
    # bit-identical to before.
    o_ref_tau <- if (EXT) dsim(tau_ref) else o
    if (is.null(o_ref_tau)) {
      cat("   [!] the reference tau_E = ", tau_ref, " does not solve to ", PEND,
          "; falling back\n       to the data window for the delay figures.\n", sep = "")
      tt <- seq(0, 61, 1); yrx <- Y1 + tt; EXT <- FALSE
      o_ref_tau <- o; dr <- lapply(PTAUS, dsim)
    }

    # Paper convention: reference case red and solid, the intermediate delay
    # black and dashed, the extreme one black and solid.
    SHORT <- list(list(o = o_ref_tau, tau = tau_ref,  col = "red",   lty = 1, lwd = 3),
                  list(o = dr[[2]],   tau = PTAUS[2], col = "black", lty = 2, lwd = 2),
                  list(o = dr[[1]],   tau = PTAUS[1], col = "black", lty = 1, lwd = 2))
    LONG  <- list(list(o = o_ref_tau, tau = tau_ref,  col = "red",   lty = 1, lwd = 3),
                  list(o = dr[[3]],   tau = PTAUS[3], col = "black", lty = 2, lwd = 2),
                  list(o = dr[[4]],   tau = PTAUS[4], col = "black", lty = 1, lwd = 2))

    dpan <- function(k, lab, sel) {
      sel <- Filter(function(z) !is.null(z$o), sel)
      ys  <- lapply(sel, function(z) z$o[[k]])
      plot(yrx, ys[[1]], type = "n", xlab = "", ylab = "",
           ylim = range(unlist(ys), na.rm = TRUE))
      ax(YL[[k]])
      # Mark where the observations stop and the extrapolation begins.
      if (EXT) abline(v = max(d$Year) + .5, col = "grey55", lty = 3)
      for (z in rev(sel)) lines(yrx, z$o[[k]], col = z$col, lty = z$lty, lwd = z$lwd)
      legend(corner(yrx, ys), bty = "n", cex = .95,
             legend = do.call(expression, lapply(sel, function(z)
               bquote(tau[italic("E")] ~ "=" ~ .(z$tau) ~ "a"))),
             col = vapply(sel, function(z) z$col, character(1)),
             lty = vapply(sel, function(z) as.numeric(z$lty), numeric(1)),
             lwd = vapply(sel, function(z) as.numeric(z$lwd), numeric(1)))
      panlab(lab)
    }

    # Figure 7 -- prices, shorter (a) and longer (b) delays
    fig("paper_delay_prices", 8.5, 8.6); op <- PPAR(mfrow = c(2, 1))
    dpan("Price", "a)", SHORT); dpan("Price", "b)", LONG)
    par(op); dev.off()

    # Figure 8 -- supply flows
    fig("paper_delay_supply", 11, 8.6); op <- PPAR(mfrow = c(2, 2))
    dpan("Extraction", "a)", SHORT); dpan("Recycling", "b)", SHORT)
    dpan("Extraction", "c)", LONG);  dpan("Recycling", "d)", LONG)
    par(op); dev.off()

    # Figure 9 -- anthropogenic flows
    fig("paper_delay_flows", 11, 8.6); op <- PPAR(mfrow = c(2, 2))
    dpan("f9", "a)", SHORT); dpan("f2", "b)", SHORT)
    dpan("f9", "c)", LONG);  dpan("f2", "d)", LONG)
    par(op); dev.off()

    # Figure 10 -- anthropogenic stocks
    fig("paper_delay_stocks", 11, 8.6); op <- PPAR(mfrow = c(2, 2))
    dpan("s4", "a)", SHORT); dpan("s8", "b)", SHORT)
    dpan("s4", "c)", LONG);  dpan("s8", "d)", LONG)
    par(op); dev.off()

    cat("   paper figures written: ", paste(PFIG, collapse = ", "), "\n", sep = "")
  }

  # ---- convergence evidence -------------------------------------------
  # (a) what the multistart stage found; (b) the descent, run past the point
  # where further progress could move any reported number.
  msf <- tagged("multistart", ".csv")

  # v5d: splice in the ancestors named by chain=. Every warm= start before v5c
  # dropped $history, so a fit promoted through the provenance chain carries
  # only the last link's record -- and if that link began AT the optimum, the
  # record is one flat point. Reading the archived states back gives the real
  # descent without re-fitting anything.
  #
  # v5d: a link is only spliceable if its SIGNATURE matches THIS run. An
  # objective with a different y0 convention (or loss, or free set) is a
  # DIFFERENT function, so its values are not commensurable with this one and
  # `value - BST` is meaningless for them. Mixing the two is what made panel
  # (b) look flat: the ancestor's own descent got parked at a near-constant
  # offset from a minimum it was never descending towards, and the
  # reparameterisation gap between the two objectives was then drawn as if it
  # were optimiser progress. check_state() already refuses such a state for a
  # resume; the figure now refuses it for the same reason, and says so.
  h <- st$history
  if (length(CHAIN)) {
    acc <- NULL; off <- 0L; drop_sig <- character(0)
    # basename() is NOT a usable label: the provenance chain is a set of
    # directories that all hold a file of the SAME name, so every link came
    # out identically tagged, length(unique(h$link)) collapsed to 1 and the
    # boundary rules in panel (b) were never drawn. Label by the part of the
    # path that actually distinguishes the links.
    linklab <- function(f) {
      if (is.na(f)) return("this fit")
      d <- dirname(f)
      if (d %in% c(".", "", "/")) sub("\\.rds$", "", basename(f)) else d
    }
    for (f in c(CHAIN, NA_character_)) {
      sf <- if (is.na(f)) st else tryCatch(readRDS(f), error = function(e) NULL)
      hh <- sf$history
      if (is.null(hh) || !nrow(hh)) {
        if (!is.na(f)) cat("   chain: ", f, " has no $history -- skipped\n", sep = "")
        next
      }
      if (!is.na(f)) {
        sg <- sig_norm(sf$sig)
        if (is.null(sg) || !identical(sg, SIGNATURE)) {
          drop_sig <- c(drop_sig, f)
          cat("   chain: ", f, "\n     was fitted under a DIFFERENT objective (",
              if (is.null(sg)) "no signature: pre-v3 file" else
                paste(names(SIGNATURE)[!vapply(names(SIGNATURE), function(n)
                  identical(sg[[n]], SIGNATURE[[n]]), logical(1))], collapse = ", "),
              " differ);\n     its objective values are not comparable with this fit's",
              " and it is\n     EXCLUDED from the convergence trace.\n", sep = "")
          next
        }
      }
      # v5d: sort on the recorded order, not on iters. iters is a cumulative
      # BUDGET counter (see run_opt), and it is re-based per link below, so
      # ordering on it can interleave links if any state was ever resumed under
      # a different maxit. Rows are already in round order within a file.
      lab <- linklab(f)
      # v5d: overwrite rather than fill. Rows written by run_opt carry
      # $link = basename(SF), which is ambiguous across the chain for exactly
      # the reason above; the label that matters here is which chain entry the
      # row was READ from, and that is unambiguous.
      hh$link <- lab
      # v5d: re-base BEFORE de-duplication. Taking min() after the dedup made
      # the surviving head row land at off+1 and silently deleted the budget
      # the dropped rows had consumed.
      hh$iters <- hh$iters - min(hh$iters) + off + 1L
      # v5c: now that warm= carries history forward, a later link ALREADY
      # contains its ancestors' rounds, so splicing archives in front of it
      # would count them twice. Drop any incoming row already accumulated --
      # carry-forward copies rows verbatim, so exact key matches are exactly
      # the duplicates. This must apply to the current fit too, not only to
      # the archives, which is where the first version of this got it wrong.
      # v5d: minutes is rounded to 2dp AND resets to per-invocation elapsed on
      # every resume, so two stalled rounds inside one session can collide on
      # the old three-field key. iters and conv are copied verbatim by the
      # carry-forward and make the key injective in practice. round cannot be
      # used: pre-v5c states wrote 1, 3, 5, ...
      key <- function(z) paste(z$value, z$gain, z$minutes, z$conv)
      if (!is.null(acc)) hh <- hh[!(key(hh) %in% key(acc)), , drop = FALSE]
      if (!nrow(hh)) next
      off <- max(hh$iters)
      # v5d: union, not intersection. intersect() silently stripped a column
      # from the WHOLE trace as soon as one link predated it; a link that is
      # missing a column should gain it as NA instead.
      cols <- union(names(acc %||% hh), names(hh))
      fill <- function(z) { for (cn in setdiff(cols, names(z))) z[[cn]] <- NA
                            z[, cols, drop = FALSE] }
      acc <- if (is.null(acc)) fill(hh) else rbind(fill(acc), fill(hh))
    }
    if (!is.null(acc) && nrow(acc)) {
      h <- acc
      cat(sprintf("   convergence figure: %d rounds spliced across %d links%s\n",
                  nrow(h), length(unique(h$link)),
                  if (length(drop_sig))
                    sprintf(" (%d excluded on signature)", length(drop_sig)) else ""))
    } else if (length(drop_sig)) {
      cat("   convergence figure: every chain link was excluded on signature;\n",
          "   falling back to this fit's own history.\n", sep = "")
    }
  }

  if (!is.null(h) && nrow(h) && file.exists(msf)) {
    # v5d: do NOT re-sort on iters. Rows are written in round order inside a
    # file and the splice above builds links in chain order, so the frame is
    # already correct; iters is a re-based BUDGET counter, and sorting on it
    # silently interleaves links if any state was ever resumed under a
    # different maxit (it is a running total, not a per-round count).
    #
    # v5d: prefer a real evaluation count when every plotted row carries one.
    # run_opt records optim()'s f$counts from this version on; states written
    # before it have no such column, and mixing the two on one axis would
    # compare a 500-per-round budget stride with actual work. All-or-nothing,
    # and the axis label says which is in use.
    if (!is.null(h$fevals) && all(is.finite(h$fevals))) {
      h$iters <- cumsum(pmax(h$fevals, 0L)); attr(h, "fevals") <- TRUE
    } else if (!is.null(h$fevals)) {
      cat("   convergence figure: some links predate the evaluation counter --\n",
          "   the x axis falls back to the 500-per-round budget stride.\n", sep = "")
    }
    ms  <- read.csv(msf)
    fin <- sort(ms$final[is.finite(ms$final) & ms$final < 1e9])
    BST <- min(c(st$value, h$value)); THR <- 1.921
    NF  <- max((st$noise %||% 2e-07) * BST, 1e-8)
    # v5c: a LOCAL multistart (warm= + local=) perturbs around a known optimum
    # and skips the coverage grid. It is a basin-robustness check, NOT a global
    # search, and panel (a) must not be captioned as one.
    LOC <- st$local_sd %||% 0
    png(tagged("convergence", ".png"), width = 1500, height = 640, res = 130)
    op <- par(mfrow = c(1, 2), mar = c(4.4, 4.6, 3, 1.2))
    plot(seq_along(fin), fin, type = "h", lwd = 3, col = "grey60",
         ylim = c(BST * .98, max(fin)), xlab = "candidate (ranked)",
         ylab = "final objective",
         main = sprintf("(a) %s: %d of %d feasible",
                        if (LOC > 0) sprintf("local restarts (sd %.2f)", LOC)
                        else "multistart", length(fin), nrow(ms)))
    points(seq_along(fin), fin, pch = 16, cex = .6, col = "grey30")
    points(1, fin[1], pch = 16, cex = 1.3, col = "firebrick")
    abline(h = BST, col = "firebrick", lty = 2)
    legend("topleft", bty = "n", cex = .78, text.col = "grey20",
           legend = c(sprintf("best %.1f -> polished to %.1f (dashed)", fin[1], BST),
                      if (length(fin) > 1)
                        sprintf("runner-up %.1f (gap %.1f)", fin[2], fin[2] - fin[1]),
                      sprintf("%d of %d infeasible", nrow(ms) - length(fin), nrow(ms)),
                      if (LOC > 0) "centred on the warm start: LOCAL, not global"))
    # v5c: start the trace at the objective the descent actually STARTED from.
    # gain = prev - value, so prev is recoverable from the first row; the old
    # code used the multistart best instead, which is only correct for a cold
    # fit and collapses the panel to a point for a warm one.
    v0 <- h$value[1] + (if (is.null(h$gain)) 0 else h$gain[1])
    if (!is.finite(v0) || v0 < h$value[1]) v0 <- h$value[1]
    tr <- data.frame(it = c(0, h$iters), v = c(v0, h$value))
    yy <- pmax(tr$v - BST, NF / 4)

    # v5d: find the transitions the history never recorded. gain = prev - value
    # is stored at full precision, so value[i] + gain[i] IS the objective the
    # round started from; whenever that disagrees with value[i-1] the objective
    # moved with no round to account for it. Two things do that:
    #   * a profile PROMOTION -- bank() writes a state whose $value is the
    #     promoted grid point, and the next fit warm-starts there. No row ever
    #     records the drop.
    #   * a chain link whose predecessor was overwritten, so the descent
    #     between the two archives is simply not on disk any more.
    # Either way it is NOT optimiser progress and must not be drawn as descent.
    # v5d: without gain there is no way to know what a round started from, so
    # no step can be detected. Claim none rather than flagging every genuine
    # descent as one -- imp would collapse to h$value and every drop would
    # register as unrecorded.
    if (is.null(h$gain)) {
      imp  <- tr$v[-1]
      step <- rep(FALSE, length(tr$v))
    } else {
      imp  <- tr$v[-1] + h$gain                               # implied prev, per row
      step <- c(FALSE, abs(imp[-1] - tr$v[-c(1, length(tr$v))]) > 1e-6)
      step <- c(FALSE, step)                                  # align to tr rows
    }
    # a promotion is a step that lands exactly on the $value of the state the
    # link was warm-started from; anything else is an un-archived gap.
    promoted <- unique(unlist(lapply(c(CHAIN, NA_character_), function(f) {
      s <- if (is.na(f)) st else tryCatch(readRDS(f), error = function(e) NULL)
      w <- s$warm_from
      if (is.null(w) || !is.character(w) || !nzchar(w) || !file.exists(w)) return(NULL)
      tryCatch(readRDS(w)$value, error = function(e) NULL) })))
    is_promo <- function(k) length(promoted) &&
      any(abs(promoted - (tr$v[k] + h$gain[k - 1])) < 1e-6)

    # v5d: the y-axis floor was NF/8 while nothing is ever drawn below NF/4,
    # so an eighth of a decade was dead space under a clamp that only the
    # exactly-converged rounds touch.
    ylo <- min(yy) / 2.5
    plot(tr$it, yy, type = "n", log = "y", col = "firebrick",
         ylim = c(ylo, max(yy) * 10^0.75),
         xlab = if (isTRUE(attr(h, "fevals"))) "objective evaluations"
                else "cumulative optimiser budget (iterations)",
         ylab = expression("objective" - hat(f)),
         main = "(b) descent past the inferential resolution")
    # v5d: break the line at every unrecorded step so the curve only ever
    # connects points the optimiser actually walked between.
    seg <- which(!step[-1])                     # i -> i+1 is a real transition
    segments(tr$it[seg], yy[seg], tr$it[seg + 1], yy[seg + 1], col = "firebrick")
    points(tr$it, yy, pch = 16, cex = .55, col = "firebrick")
    for (k in which(step)) {
      segments(tr$it[k], yy[k - 1], tr$it[k], yy[k], col = "grey45", lty = 2)
      points(tr$it[k], yy[k], pch = 1, cex = 1.1, col = "grey30")
    }
    # v5d: rule and label the provenance boundaries. A link boundary and an
    # unrecorded step sit at the SAME x by construction -- the step is what
    # separates the two archives -- so they are annotated once, together,
    # instead of writing two rotated labels over each other.
    ann <- NULL
    if (!is.null(h$link) && length(unique(h$link)) > 1) {
      keep_i <- c(TRUE, h$link[-1] != h$link[-nrow(h)])
      # v5c: state filenames carry the full variant suffix and run vertically
      # off the panel. Strip the boilerplate and cap the length.
      lb <- h$link[keep_i]
      lb <- sub("\\.rds$", "", lb)
      lb <- sub(paste0(CFG, "$"), "", lb)
      lb <- sub("^state_(fit|profile_best|profile_subthreshold)_*", "", lb)
      lb <- ifelse(nchar(lb) > 14, paste0(substr(lb, 1, 13), "…"), lb)
      lb[!nzchar(lb)] <- "fit"
      ann <- data.frame(x = h$iters[keep_i], lab = lb, stringsAsFactors = FALSE)
    }
    for (k in which(step)) {
      # v5d: report the UNRECORDED jump, not the drawn drop. imp[k-1] is the
      # objective the round actually started from, so tr$v[k-1] -> imp[k-1] is
      # the part with no round behind it; the remainder down to tr$v[k] is
      # round k-1's own recorded gain and belongs to the trace, not the step.
      txt <- sprintf("%s %+.3f", if (is_promo(k)) "promote" else "unrecorded",
                     imp[k - 1] - tr$v[k - 1])
      j <- if (is.null(ann)) integer(0) else which(ann$x == tr$it[k])
      if (length(j)) ann$lab[j[1]] <- paste0(ann$lab[j[1]], ": ", txt)
      else ann <- rbind(ann, data.frame(x = tr$it[k], lab = txt,
                                        stringsAsFactors = FALSE))
    }
    if (!is.null(ann) && nrow(ann)) {
      ann <- ann[order(ann$x), , drop = FALSE]
      # v5d: links that are one row apart (a carried head row and the archive
      # that carries it) would print on top of each other. Merge any boundary
      # closer than 4% of the axis to the last one drawn.
      minsep <- 0.04 * diff(range(tr$it)); keep <- 1L
      for (k in seq_len(nrow(ann))[-1]) {
        if (ann$x[k] - ann$x[keep[length(keep)]] < minsep)
          ann$lab[keep[length(keep)]] <-
            paste0(ann$lab[keep[length(keep)]], " / ", ann$lab[k])
        else keep <- c(keep, k)
      }
      ann <- ann[keep, , drop = FALSE]
      abline(v = ann$x, col = "grey85")
      text(ann$x, 10^par("usr")[4], ann$lab, srt = 90, adj = c(1, -0.35),
           cex = .52, col = "grey40", xpd = NA)
    }
    abline(h = THR, lty = 2, col = "grey40"); abline(h = NF, lty = 3, col = "steelblue")
    i <- which(yy < THR)[1]
    # v5d: a crossing is only a crossing if the trace was ever above the line.
    # A warm-started chain can begin below it (i == 1), and a chain whose first
    # sub-threshold point sits on an unrecorded step did not descend across it
    # either -- it resumed there. Both used to be reported as "crossed at N",
    # quoting a budget figure the descent never earned.
    crossed <- !is.na(i) && i > 1L && !isTRUE(step[i])
    if (crossed) abline(v = tr$it[i], lty = 3, col = "grey60")
    xed <- if (is.na(i)) "never crosses"
           else if (i == 1L) "below threshold throughout"
           else if (isTRUE(step[i])) "already below on resume (see step)"
           else sprintf("crossed at %s", format(tr$it[i], big.mark = ","))
    # v5d: bottomleft. The trace lives in the top-left and bottom-right of this
    # panel, and the old topright box was printed over the descent.
    legend("bottomleft", bty = "o", bg = "white", box.col = "white",
           cex = .74, lty = c(2, 3, 3, 2),
           col = c("grey40", "steelblue", "grey60", "grey45"),
           legend = c(expression(chi^2*"(1) 95% threshold"),
                      sprintf("solver noise floor (%.1e)", NF),
                      xed,
                      if (any(step)) "step with no recorded rounds"))
    par(op); dev.off()
  } else if (!is.null(h) && nrow(h) && nrow(h) < 2) {
    cat("   (convergence figure: this fit's history is a single round -- it was\n",
        "    warm-started at its optimum. Pass chain=<ancestor states, oldest\n",
        "    first> to splice in the descent that produced it.)\n", sep = "")
  } else cat("   (no convergence figure: needs $history and ", msf, ")\n", sep = "")

  # ---- initialisation-transient check ---------------------------------
  # With data-derived y0 these rows should be FLAT. A column that still falls
  # sharply with k marks a series whose opening years are not yet consistent.
  KS <- 0:3; Y1 <- min(d$Year)
  cat("\n  INITIALISATION CHECK: NRMSE with the first k years dropped\n")
  cat("  series      ", paste(sprintf("   k=%d", KS), collapse = ""), "\n", sep = "")
  for (k in ser) { col <- OBS_ALL[[k]]
    row <- vapply(KS, function(b) { ok <- !is.na(d[[col]]) & d$Year >= Y1 + b
      100 * sqrt(mean((d[[col]][ok] - o[[k]][ok])^2)) / mean(abs(d[[col]][ok])) },
      numeric(1))
    cat(sprintf("  %-12s%s%s\n", k, paste(sprintf("%7.1f", row), collapse = ""),
                if (row[1] - row[length(row)] > 2) "   <-- still transient" else "")) }

  write.csv(o, tagged("fitted_trajectory", ".csv"), row.names = FALSE)
  jp <- setNames(as.list(vapply(seq_along(ki), function(i) dec(KEEP[i], st$par[i]),
                                numeric(1))), KEEP)
  writeLines(paste0("{\n", paste(sprintf('  "%s": %.6g', names(jp), unlist(jp)),
             collapse = ",\n"), "\n}"), tagged("params_fitted", ".json"))
  # Full parameter set (free AND fixed) plus provenance, for the appendix.
  allp <- vapply(seq_along(PN), function(i) dec(PN[i], v[i]), numeric(1))
  writeLines(c("{",
    sprintf('  "cost": "%s",', COST), sprintf('  "loss": "%s",', LOSS),
    sprintf('  "objective": %.6g,', st$value),
    sprintf('  "free": [%s],', paste(sprintf('"%s"', KEEP), collapse = ", ")),
    '  "parameters": {',
    paste(sprintf('    "%s": %.6g', PN, allp), collapse = ",\n"),
    "  },",
    # y0 is derived, not fixed, so the appendix needs the values actually used.
    '  "initial_stocks": {',
    paste(sprintf('    "s%d": %.6g', 1:8, data_y0(setp(v))$s), collapse = ",\n"),
    "  }", "}"), tagged("params_full", ".json"))
  cat("\nwrote ", paste(tagged(c("fit_v2", "fit_v2_residuals", "resid_qq",
      "resid_vs_fitted", "convergence", "fitted_trajectory", "params_fitted",
      "params_full"), c(".png", ".png", ".png", ".png", ".png", ".csv",
      ".json", ".json")), collapse = ", "), "\n", sep = "")
  write_manifest("plot")
}
