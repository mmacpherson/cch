// Usage forecasting in cch: model, estimation, and code.
//
// Figures are simulated from the model with made-up parameters (figures.py);
// no real usage data appears in this document.
//
//   ./figures.py && typst compile usage-model.typ usage-model.pdf
//   typst compile --input target=remarkable usage-model.typ usage-model-remarkable.pdf

#import "@local/mm:0.1.0": *
#let pal = palette("indigo")
#show: brief.with(
  palette: "indigo",
  fonts: "source",
  title: "Forecasting agent quota usage with a dynamic gamma process",
  date: "cch usage model · draft, September 2026",
)
#set math.equation(numbering: "(1)")
#set figure(gap: 0.5em)
#show figure.caption: set text(size: 8pt, fill: pal.muted)

#let Ga = math.op("Gamma")
#let Be = math.op("Beta")
#let Bern = math.op("Bernoulli")
#let Dir = math.op("Dirichlet")
#let N = math.op(math.cal("N"))
#let E = math.op("E")
#let ind = math.bb("1")

= Summary

Coding agents such as Claude Code and Codex meter use against rate-limit
*windows*: a rolling 5-hour window and a 7-day window, each reported as a
percentage of the plan's budget that resets at a known time. cch forecasts, for
each agent and window, the *demand* at the next reset: the percentage the meter
would show if nothing stopped it. We report the median, 50% and 90% predictive
intervals, and the probability of reaching the cap.

The forecast rests on one generative model of usage in absolute (local clock)
time: an hour-of-week activity profile, a latent intensity that evolves across
resets, and a per-block on/off state for idle periods. Windows are only
accounting on top of that process. The model is estimated two ways: a fast
*maximum-likelihood fit* with an empirical-Bayes activity profile, which serves
live forecasts, and a *hierarchical posterior* in Stan, which pools agents and
windows fully. Both share the same likelihood. This note states the model,
both estimators, how forecasts are evaluated, and where each piece lives in
the code.

#note(pal)[All figures use simulated data from the model with made-up parameters.]

= The data

Each observation is a tuple $(t, W, m)$: at time $t$, window $W$ reads $m in
{0, 1, dots, 100}$ percent. Four features shape the model.

- *Quantization.* The meter reports whole percent, so an increment of $y$ means
  true usage in $[y - 1/2, y + 1/2]$.
- *Irregular windows.* A 7-day window may reset early, and some providers start
  a window at first use. We therefore model usage in absolute time and treat a
  window as the interval $[s_W, e_W)$ from its start to its (possibly early) end.
- *The cap.* At 100% the provider stops use, so demand after the cap time $c_W$
  is unobserved (right-censored). Hours in $[c_W, e_W)$ carry no likelihood.
- *Idle days and bursts.* Usage is zero on many days and heavy on others
  (@fig-daily). A single gamma distribution cannot represent both.

#figure(
  image("fig-daily.svg", width: 55%),
  caption: [Daily usage simulated from the model: a point mass at zero from
    'off' days, and a long right tail from bursty 'on' days.],
) <fig-daily>

= Model

== Activity profile and mass

Let $h(t) in {0, dots, 167}$ be the local hour of the week at time $t$. An
activity profile $a = (a_0, dots, a_167)$ with $a_h > 0$ and
$1/168 sum_h a_h = 1$ gives the relative rate of use at each hour. For a time
interval $I$ made of hours $u$ with live fractions $ell_u in [0,1]$ (the part of
the hour inside a live window), the *profile mass* is
$ A(I) = sum_(u in I) ell_u a_(h(u)). $ <eq-mass>

== Gamma process with a latent intensity

Usage over an interval $I$ is modelled as a gamma process in profile time:
$ Y(I) | lambda ~ Ga(kappa A(I), lambda), $ <eq-gp>
with shape $kappa A(I)$ and rate $lambda$ (so $E[Y(I) | lambda] = kappa
A(I) \/ lambda$), and independent increments over disjoint intervals. The
burstiness $kappa$ is small when usage clusters in a few hours. The process is
closed under splitting: given a total $Y(I)$, its division over sub-intervals
$I_1, dots, I_n$ is $Dir(kappa A(I_1), dots, kappa A(I_n))$. This is what lets a
forecast be simulated hour by hour from a model fitted on longer blocks.

Integrating the intensity against a gamma belief
$lambda ~ Ga(alpha, beta)$ gives the compound-gamma (beta-prime) predictive
$ Y / (Y + beta) ~ Be(kappa A, alpha), $ <eq-bp>
whose density is
$ p(y) = (y^(kappa A - 1) beta^alpha) / (B(kappa A, alpha) (y + beta)^(kappa A + alpha)), quad y > 0. $

== Blocks, zero inflation, and dynamic states

Time is cut into *blocks* $b = 1, 2, dots$: 24-hour blocks anchored at 04:00
local time for the 7-day window, and 1-hour blocks for the 5-hour window. Each
block has mass $A_b$, live hours $L_b = sum_(u in b) ell_u$, and observed usage
$y_b$. A block is *on* with probability $pi$ and otherwise has no usage:
$ z_b ~ Bern(pi), quad y_b = z_b Y_b. $

Both $lambda$ and $pi$ drift over time. We carry conjugate beliefs
$lambda ~ Ga(alpha_b, beta_b)$ and $pi ~ Be(a_b, b_b)$ through the blocks with a
*discounted* update in the spirit of West and Harrison's dynamic models.
Before each block the beliefs relax toward the prior with discount
$d in (0, 1)$:
$ alpha_b^- = d alpha_(b-1)^+ + (1 - d) alpha_0, quad
  beta_b^- = d beta_(b-1)^+ + (1 - d) beta_0, $ <eq-relax>
and likewise for $(a_b, b_b)$ toward $(a_0, b_0)$. Memory is thus a half-life of
$ln(1/2) \/ ln d$ blocks. After observing $y_b$, the posterior probability
that the block was on is
$ r_b = (pi_b^- P_b) / (pi_b^- P_b + (1 - pi_b^-) ind[y_b < 1/2]), quad
  pi_b^- = a_b^- / (a_b^- + b_b^-), $ <eq-resp>
where $P_b$ is the quantized likelihood of @eq-quant. The beliefs update by
responsibility-weighted conjugate steps:
$ alpha_b^+ = alpha_b^- + r_b kappa A_b, quad beta_b^+ = beta_b^- + r_b y_b,
  quad a_b^+ = a_b^- + r_b, quad b_b^+ = b_b^- + 1 - r_b. $ <eq-update>
This is an assumed-density filter: exact conjugacy within a block and a
moment-preserving projection across the on/off mixture. Intensity therefore
carries across resets; windows do not reset it.

*Sliver blocks.* A partial block at a window edge with few live hours
($L_b < ell_min$) updates the intensity but not the on/off state. The test uses
live hours, which are data, rather than the mass $A_b$, which depends on the
profile; a mass test makes the likelihood discontinuous in the profile.

== Observation model

With whole-percent reporting, the probability of an on-block's observation is
the mass of @eq-bp on the reporting interval:
$ P_b = I_(x^+)(kappa A_b, alpha_b^-) - I_(x^-)(kappa A_b, alpha_b^-), quad
  x^plus.minus = (y_b plus.minus 1/2)_+ / ((y_b plus.minus 1/2)_+ + beta_b^-), $ <eq-quant>
where $I_x (p, q)$ is the regularized incomplete beta function. The
one-step-ahead predictive probability of block $b$ is
$ p(y_b | y_(1:b-1), theta) = pi_b^- P_b + (1 - pi_b^-) ind[y_b < 1/2], $ <eq-pred>
with parameters $theta = (kappa, alpha_0, beta_0, a_0, b_0, d)$.

#aside(pal, title: "Numerics")[When both CDF values in @eq-quant are near one
(a heavy block the beliefs did not expect), their difference cancels to zero in
double precision. We difference in the smaller tail instead, using
$I_x (p, q) = 1 - I_(1-x)(q, p)$ with $1 - x = beta \/ (y + beta)$ computed
without subtraction. Against a 40-digit reference this is exact to about
$10^(-12)$, where naive differencing returned zero for about 4% of random
parameter settings.]

== The forecast

At time $t$ inside window $W$ with meter reading $m_W (t)$, the demand at the
scheduled reset $T_W$ is
$ D_W = m_W (t) + sum_("future pieces " j) z_(b(j)) Y_j, $
simulated by Monte Carlo from the filtered beliefs: draw $lambda$ and $pi$,
draw each remaining block's on/off state (the current block is on if it has
already seen use), and draw each piece's usage from @eq-gp. Pieces are hours
(7-day) or five-minute steps (5-hour), which the splitting property makes
consistent with the block model. The summaries are the median and quantiles of
$D_W$ and $P(D_W >= 100)$. The same draws give the predictive path shown as a
fan chart (@fig-fan).

#figure(
  image("fig-fan.svg", width: 100%),
  caption: [A simulated 7-day window: the observed meter to 'now', then the
    median path and 50% and 90% bands of demand from the filtered beliefs. The
    bands flatten overnight because the profile does.],
) <fig-fan>

= Maximum-likelihood fit

The live forecast uses point estimates, refit lazily at most once a day in the
background.

== Activity profile: empirical Bayes

For each agent/window *cell* $c$, let $u_(c h w)$ be usage in hour-of-week bin
$h$ during week $w$, $e_(c h w)$ its live hours, and $overline(r)_c$ the cell's
mean rate. Normalized weekly rates $r_(c h w) = u_(c h w) \/ (e_(c h w)
overline(r)_c)$ are comparable across cells measured in different units. The
fleet shape $f_h$ is their exposure-weighted mean over all cells, renormalized
to mean one.

Each cell is shrunk toward the fleet with a strength estimated by moments, in
the manner of Fay and Herriot. Let $m_(c h)$ be the mean of $r_(c h w)$ over
the $n_(c h)$ weeks observed. Bursty usage has variance roughly proportional to
its rate, $"Var"(r_(c h w)) approx phi r$, so the bin mean has sampling noise
$v_(c h) = phi (m_(c h) + f_h) \/ (2 n_(c h))$. Real departures from the fleet
are modelled as variance $t f_h$. Since $E[(m_(c h) - f_h)^2] = t f_h + v_(c h)$,
$ hat(t) = max(0, (sum_h (m_(c h) - f_h)^2 - v_(c h)) / (sum_h f_h)), quad
  hat(a)_(c h) = f_h + w_(c h) (m_(c h) - f_h), quad
  w_(c h) = (hat(t) f_h) / (hat(t) f_h + v_(c h)), $ <eq-eb>
with $phi$ from the pooled within-bin variance. The result is circularly
smoothed (Gaussian kernel, $sigma = 1.5$ h) and normalized. The equivalent
pseudo-exposure is $k = phi \/ hat(t)$, so a cell with no detectable difference
($hat(t) = 0$) pools fully (@fig-profile).

#figure(
  image("fig-profile.svg", width: 100%),
  caption: [Empirical-Bayes shrinkage of a sparse simulated cell whose true
    rhythm runs three hours later than the fleet. The estimated strength
    ($k approx 20$ pseudo-hours per bin) keeps the cell's shape where the
    evidence is strong and pulls noisy peaks toward the fleet.],
) <fig-profile>

== Parameters: prequential likelihood

Given the profile, $theta$ maximizes the log of the one-step-ahead predictive
likelihood @eq-pred over the fit span (all history for 7-day cells; the last
42 days for 5-hour cells):
$ hat(theta) = arg max_theta sum_b log p(y_b | y_(1:b-1), theta). $
The optimization runs in unconstrained coordinates
$(log kappa, log alpha_0, log beta_0, log(a_0 + b_0), "logit" (a_0 \/ (a_0 + b_0)), "logit" d)$
by Nelder-Mead with one restart from the best point, warm-started from the
previous fit. The objective is cheap (one filter pass), and derivative-free
search avoids differentiating @eq-quant. The block length matters: burstiness
is scale dependent, and 24-hour blocks for the 7-day window gave calibrated
intervals where shorter blocks were overconfident.

= Hierarchical posterior

The Stan model keeps the likelihood (@eq-resp, @eq-update, @eq-quant) unchanged and replaces
the plug-in estimates with a joint posterior over all cells.

== Profiles

Profiles live on a harmonic hour-of-week basis split into within-day columns
$B^"day"$ (daily harmonics, plus daily harmonics active on weekends) and weekly
columns $B^"week"$ (weekly harmonics and a weekend level):
$ log a_c = B^"day" (beta^"day" + delta_(g(c))^"day") + B^"week" (beta^"week" + delta_c^"week") + "const", $
where $g(c)$ is the cell's agent. Day-long 7-day blocks cannot identify the
within-day shape, because daily harmonics integrate to zero over a day. The
within-day deviation therefore belongs to the *agent* and is identified by its
hourly 5-hour cell (both meters count the same usage), while each cell deviates
from the fleet only in weekly shape. With $z ~ N(0, I)$,
$ delta_g^"day" = tau_"day" z_g, quad delta_c^"week" = tau_"week" z_c, quad
  tau_"day", tau_"week" ~ N^+(0, 0.5^2), $
so full pooling ($tau -> 0$) stays possible, and
$beta = hat(beta)_"fleet" + 0.5 z_beta$ is centered on the least-squares fit of
the pooled empirical profile.

== Parameters in identified coordinates

The natural coordinates of $theta$ have strong posterior ridges (for example
$alpha_0$ and $beta_0$ trade off to keep the mean fixed). Each cell is
parameterized instead by quantities the data pin down directly:
$ phi_c = (log p + log kappa + log beta_0 - psi(alpha_0),
  log alpha_0, log kappa, "logit" p, log s, log h_(1\/2)), $
the log geometric-mean usage rate, the intensity shape (any $alpha_0 > 0$,
including heavy tails), burstiness, the on-probability $p = a_0 \/ s$ and prior
strength $s = a_0 + b_0$, and the memory half-life $h_(1\/2)$ in blocks, with
$d = 2^(-1 \/ h_(1\/2))$. Here $psi$ is the digamma function. Cells are centered
on a mean for their window type $k(c) in {5"h", 7"d"}$:
$ phi_c = phi_(k(c))^0 + s_phi dot.o eta_c, quad
  eta_c ~ N(mu_(k(c)), "diag"(sigma_(k(c))^2)), quad
  mu_k ~ N(0, I), quad sigma_(k j) ~ "LogNormal"(log 0.5, 0.5), $
with centers $phi^0$ taken from the maximum-likelihood starting point and scales
$s_phi$ of one to one and a half units. Two or three cells per window type
cannot estimate the spread $sigma$, so its lognormal prior is the stated
pooling assumption: agents differ by about half a prior unit. Keeping it off
zero also avoids the funnel at zero spread.

== Computation

Sampling uses NUTS with `adapt_delta` 0.95. Where hours split crisply into on
and off (large $kappa$, weak on/off prior) the posterior is sharply curved, and
the default target produced divergences. Almost all run time was the incomplete
beta in @eq-quant and its gradients with respect to the shape parameters. Three
changes made fits about six times faster with the same posterior:

+ Zero-usage blocks use the power series of $I_x (p, q)$, reflected to
  $1 - I_(1-x)(q, p)$ where the direct series converges slowly. The two
  convergence regions are complementary.
+ Blocks with usage use 5-point Gauss-Legendre quadrature of the closed-form
  density where it is smooth over the unit interval (within about $2 dot 10^(-5)$
  of exact).
+ Where the density is steep, they use an exact incomplete-beta difference in
  the smaller tail.

Forecasts from the posterior are mixtures: each retained draw of $(theta_c, a_c)$
runs the filter and the simulation above, and the paths are pooled.

= Evaluation

Forecasts are scored by a rolling-origin backtest. Each window is forecast at
fixed checkpoints (every 12 h for 7-day windows, every 30 min for 5-hour ones)
using only data observed before the checkpoint, and fits use only data before
the window starts (or the refit week). The target is the meter at the window's
actual end. Windows that hit the cap are scored on the capped meter, which keeps
the score proper under censoring.

The primary score is the continuous ranked probability score, estimated from
the $n$ sorted draws $x_((1)) <= dots <= x_((n))$ of the capped forecast
$min(D_W, 100)$:
$ "CRPS"(F, y) = E|X - y| - 1/2 E|X - X'|
  approx 1/n sum_i |x_((i)) - y| - 1/n^2 sum_i (2i - n - 1) x_((i)). $
CRPS is proper, is in percentage points, and reduces to the absolute error for
a point forecast. Secondary scores are 50% and 90% interval coverage, the
median's absolute error, and the Brier score of $P(D_W >= 100)$. Comparisons are
paired on identical checkpoints, with standard errors clustered by window
because checkpoints within a window are correlated. Model changes are adopted
only when a rule fixed before the run is met, typically a CRPS gain beyond one
standard error on the best-observed cell with no loss elsewhere and 90%
coverage between 85% and 95%.

= Code

The model is implemented in Clojure inside cch, with the hierarchical fit as
an offline Stan job. Everything under `src/cch` is pure except where noted.

#tbl-alt(pal,
  columns: (auto, 1fr),
  table.header[*Location*][*Role*],
  [`cch.usage-model`], [The model: windows from hourly aggregates, hour series, profiles (`bin-stats`, `eb-rates`, `fleet-profiles`), blocks, the filter (`run-filter`), `interval-prob`, the maximum-likelihood `fit`, and forecasts (`predict`, `predict-path`, `forecast`, `crps`).],
  [`cch.numeric`], [Log-gamma, regularized incomplete beta, seeded gamma and beta samplers, sample quantiles, and Nelder-Mead. Dependency-free, since cch ships as a jlink image.],
  [`cch.forecast`], [Runtime: incremental hourly aggregates from SQLite, fits cached per cell and refit in the background after 24 h (`cached-fit`), and the statusline and `/usage` bundles.],
  [`cch.control.usage-forecast`], [The same forecast over the broker's fleet read model, which carries hourly per-reset maxima.],
  [`cch.usage-stan`], [Exports cells' blocks for Stan (built by the same code as the fit) and loads posterior draws; `mixture-forecast` pools draws.],
  [`resources/stan/usage_model.stan`], [The hierarchical model.],
  [`bin/cch-usage-stan-fit`], [The offline fit job (CmdStan via cmdstanpy): threading over cells, draws written before diagnostics.],
  [`cch.usage-backtest`], [Rolling-origin replays: `run` (model against the earlier rate projection), `run-pooling` (profile variants), `run-momentum` (block-length experiment), `run-stan` (maximum likelihood against the posterior).],
)

The heart of the filter is a fold over blocks that returns the updated
beliefs and the log predictive likelihood:

```clojure
(reduce
  (fn [{[al be a b] :state ll :loglik} [_ A y live-hours]]
    (let [al (relax al al0) be (relax be be0) a (relax a a0) b (relax b b0)]
      (if (< live-hours min-live-hours)               ; sliver: intensity only
        {:state [(+ al (* kappa A)) (+ be y) a b] :loglik ll}
        (let [pi (/ a (+ a b))
              pg (interval-prob y A al be kappa)       ; quantized likelihood P_b
              tot (+ (* pi pg) (* (- 1.0 pi) (if (< y 0.5) 1.0 0.0)))
              r (/ (* pi pg) tot)]                     ; responsibility r_b
          {:state [(+ al (* r kappa A)) (+ be (* r y)) (+ a r) (+ b (- 1.0 r))]
           :loglik (+ ll (Math/log tot))}))))
  {:state [al0 be0 a0 b0] :loglik 0.0}
  blocks)
```

The Stan likelihood is the same recursion, written once per cell and summed
across cells with `reduce_sum`:

```stan
if (lo > 0) {
  lp_g = interval_logprob(lo, hi, k, al, be);          // quadrature or tail difference
} else if (x_hi < (k + 1) / (k + al + 2)) {
  lp_g = log_inc_beta_series(k, al, x_hi);             // zero block, direct series
} else {
  lp_g = log1m_exp(log_inc_beta_series(al, k, be / (hi + be)));   // reflected
}
real lp = y[i] < 0.5 ? log_sum_exp(log_pi + lp_g, log1m(pi)) : log_pi + lp_g;
real r = exp(log_pi + lp_g - lp);
```

The backtests and the Stan fit are local tools:

```bash
just usage-backtest            # maximum-likelihood model against the rate projection
just usage-backtest-pooling    # activity-profile pooling variants
bin/cch-usage-stan-fit data.json draws.json --warmup 300 --samples 200
```

= Limitations and next steps

- *Observation coverage.* Hours when no collector was running are treated as
  zero usage. They should be missing (dropped from the likelihood), and a jump
  after a gap should enter as an observed total over the gap, which the gamma
  process handles exactly.
- *Calendar blocks.* The gamma process @eq-gp is invariant to aggregation:
  increments over adjacent intervals add up exactly. The block structure is not.
  The on/off state is defined per block, the discount $d$ counts blocks, and the
  intensity is constant within a block. This is why the burstiness $kappa$ is
  scale dependent, and why the block length (24 h at a 04:00 anchor for the
  7-day window, 1 h for the 5-hour window) is a discrete model choice made by
  predictive comparison rather than something the data identify. A
  continuous-time formulation removes it. The discounted update
  @eq-relax is the moment-matched filter of a mean-reverting gamma-type
  intensity (a CIR or gamma-OU process) with $d = e^(-theta Delta t)$. With
  $d$ indexed by elapsed time, irregular observation intervals enter natively.
  On/off becomes a two-state continuous-time Markov chain whose transition over
  $Delta t$ is a closed-form $2 times 2$ matrix exponential.
- *Two timescales.* Usage has session momentum within a day and persistence
  across days. One discount cannot hold both, and hourly blocks for the 7-day
  window were worse for weekly totals. In continuous time this is a sum of a
  fast and a slow intensity component with separate decay rates. With
  parameters as per-hour rates, an agent's 5-hour and 7-day meters could share
  one parameter set, fitted jointly. The likelihood stays a deterministic
  filter recursion, so both the maximum-likelihood fit and the Stan model carry
  over. The latent paths are marginalized, not sampled.
- *Sequential updating.* Between weekly posterior fits, draws could be
  reweighted daily by the likelihood of new blocks (PSIS, refitting when the
  Pareto $hat(k)$ exceeds about 0.7). Parameter drift, as a slow random walk,
  would replace the fixed 42-day lookback.
- *Cold start.* The hierarchical posterior gives new agents a partially pooled
  forecast from their first window. The maximum-likelihood fit still waits for
  five completed windows.

= References

#set text(size: 8pt)
#set par(spacing: 0.4em)
- Barndorff-Nielsen, O. E. and Shephard, N. (2001). Non-Gaussian
  Ornstein-Uhlenbeck-based models and some of their uses in financial
  economics. _JRSS B_ 63, 167--241.
- Cox, J. C., Ingersoll, J. E. and Ross, S. A. (1985). A theory of the term
  structure of interest rates. _Econometrica_ 53, 385--407.
- Fay, R. E. and Herriot, R. A. (1979). Estimates of income for small places:
  an application of James-Stein procedures to census data. _JASA_ 74, 269--277.
- Gneiting, T. and Raftery, A. E. (2007). Strictly proper scoring rules,
  prediction, and estimation. _JASA_ 102, 359--378.
- McElreath, R. (2020). _Statistical Rethinking_, 2nd ed., ch. 13 and 15. CRC Press.
- Vehtari, A., Simpson, D., Gelman, A., Yao, Y. and Gabry, J. (2024). Pareto
  smoothed importance sampling. _JMLR_ 25(72).
- West, M. and Harrison, J. (1997). _Bayesian Forecasting and Dynamic Models_,
  2nd ed. Springer.
