# DMMCM v5 -- the commands behind thesis Chapter 3 ("Simulating resources").
# `make cycle` re-runs the reported estimation end to end; `make sweep` runs the
# fixed-parameter delay / demand-form experiments quoted in the discussion.
# Every run_all.R target resumes from its state_*.rds, so an interrupted run is
# safe to repeat. Do NOT change starts=, seed= or free= on a resume.
#
# The state files shipped in the repository are the reported results. The exact
# arguments of the run that last wrote each of them are in the corresponding
# manifest_<mode>__slade_cycle_drop_core.txt, which is the authoritative record
# (see README.md, "Provenance of the reported fit").

R      := Rscript run_all.R
COMMON := free=core loss=cycle eff=off
CORES  := 4

.PHONY: all cycle fit diag profile plot figures sweep clean-outputs

all: cycle figures

cycle: fit diag profile plot

# Latin-hypercube multistart followed by Nelder-Mead refinement to convergence.
fit:
	$(R) fit $(COMMON) maxit=auto starts=24 stage1=600 cores=$(CORES) cap=240

# Local sensitivities and collinearity index at the fitted parameters.
diag:
	$(R) diag $(COMMON) at=fit

# Profile likelihoods. Spans are set per parameter and the stored profile was
# assembled over several resumed invocations; the last one is recorded in
# manifest_profile__slade_cycle_drop_core.txt.
profile:
	$(R) profile $(COMMON) which=c,phi,C0,c1 grid=9 span=0.3 pit=1500 cores=$(CORES)

# Fitted trajectories, error metrics and the chapter figures (paper_*.pdf,
# including the tau_E = 2, 5, 16, 32 delay figures).
plot:
	$(R) plot $(COMMON)

# Residual normality and outlier diagnostics (resid_qq, resid_vs_fitted).
figures:
	Rscript residual_qq.R

# Delay sweep at the fitted parameters: eps_P sweep, published (v1) vs present
# (v2) demand form, cost-anchor specifications, c2 sensitivity. No refitting.
sweep:
	Rscript tau_sweep.R 2>/dev/null | grep -vE "DLSODA|In above message|such that|step size|issued|precision|TOLSF"

# Removes derived figures only. State files are deliberately left alone:
# deleting them throws away days of compute.
clean-outputs:
	rm -f fit_v2*.png profile*.png convergence*.png resid_*.png paper_*.pdf
