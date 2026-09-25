// Hierarchical zero-inflated dynamic gamma model of agent quota usage.
//
// Same likelihood as cch.usage-model (see its docstring): per cell (an
// agent/window pair), usage over a block with profile mass A is
// Gamma(kappa*A, lambda) when the block is 'on', and 0 otherwise; the
// intensity lambda ~ Gamma(alpha, beta) and on-probability pi ~ Beta(a, b)
// are filtered through the blocks, relaxing toward their priors with
// discount d between blocks. Block likelihoods are interval probabilities
// of the compound-gamma predictive y/(y+beta) ~ Beta(kappa*A, alpha), since
// usage is reported in whole percent.
//
// Hierarchy:
//   * Activity profiles: log a_c = B (beta_g + delta_c), normalized to mean
//     1, over a smooth hour-of-week basis B: n_daily daily harmonics,
//     n_weekly weekly harmonics, and a weekend effect with its own n_weekend
//     daily harmonics (sizes are data). beta_g is the fleet profile and delta_c a cell
//     deviation whose scale tau is learned: the full-Bayes analogue of the
//     empirical-Bayes shrinkage strength k. A low-dimensional basis keeps the
//     posterior well conditioned; 168 free bins behind a smoothing kernel
//     left most directions identified only by the prior.
//   * Within-block split: day-long (7d) blocks cannot see the within-day
//     shape, since daily harmonics integrate to zero over a day. Each block's
//     hourly counts therefore also enter through the gamma-process split
//     (a Dirichlet-multinomial given the block total), with its own
//     concentration kappa_w, since burstiness depends on the time scale.
//   * Hierarchy scales (sigma_g, tau, sigma_theta) have lognormal priors that
//     keep them off zero: with 2-3 cells per level, half-normal priors let the
//     posterior fall into the funnel at zero spread (divergences).
//   * Parameters: each cell's six parameters are drawn around a mean for its
//     window type (5h or 7d), with a firmly regularized spread (2-3 cells
//     per type cannot estimate it). The centered form is used because every
//     cell carries hundreds of blocks; the non-centered form left an additive
//     ridge between type means and cell offsets. They live on quantities the data
//     identifies directly, because the natural (kappa, alpha0, beta0, a0, b0,
//     d) form has strong ridges (posterior corr alpha0-beta0 +0.85, d-s
//     +0.79, kappa-p -0.74):
//       phi = [log overall mean usage per unit profile mass,
//              log CV of the intensity (alpha0 = 1 + 1/CV^2),
//              log kappa (within-block burstiness),
//              logit on-probability,
//              log on/off prior strength,
//              log memory half-life in blocks (d = 0.5^(1/half-life))].
//     `theta` reports them in cch.usage-model's unconstrained convention
//     [log kappa, log alpha0, log beta0, log s, logit p, logit d].
functions {
  // log I_x(a, b) by its power series, for x below (a+1)/(a+b+2). Most
  // blocks have zero usage, where the likelihood is I_x(kappa*A, alpha) at a
  // small x; this is exact to ~1e-12 there and far cheaper to differentiate
  // than beta_lcdf, whose shape-parameter gradients dominated run time.
  real log_inc_beta_series(real a, real b, real x) {
    real term = 1;
    real total = 1;
    for (n in 0:200) {
      term *= (a + b + n) / (a + 1 + n) * x;
      total += term;
      if (term < 1e-12 * total) break;
    }
    return a * log(x) + b * log1m(x) - log(a) - lbeta(a, b) + log(total);
  }

  real cell_loglik(vector y, array[] int block_ptr, array[] int hbin, vector live, vector yh,
                   int start, int len, vector prof, real kappa, real al0, real be0,
                   real a0, real b0, real d, real min_mass, real kappa_w) {
    real al = al0; real be = be0; real a = a0; real b = b0;
    real lp_total = 0;
    for (i in start:(start + len - 1)) {
      real A = 0;
      for (j in block_ptr[i]:(block_ptr[i + 1] - 1)) A += live[j] * prof[hbin[j]];
      al = d * al + (1 - d) * al0; be = d * be + (1 - d) * be0;
      a = d * a + (1 - d) * a0;    b = d * b + (1 - d) * b0;
      if (A < min_mass) {
        al += kappa * A; be += y[i];
      } else {
        real k = kappa * A;
        real hi = y[i] + 0.5;
        real lo = fmax(y[i] - 0.5, 0);
        real x_hi = hi / (hi + be);
        real lp_g;
        if (lo > 0) {
          lp_g = log_diff_exp(beta_lcdf(x_hi | k, al), beta_lcdf(lo / (lo + be) | k, al));
        } else if (x_hi < (k + 1) / (k + al + 2)) {
          lp_g = log_inc_beta_series(k, al, x_hi);
        } else {
          lp_g = beta_lcdf(x_hi | k, al);
        }
        real log_pi = log(a / (a + b));
        real lp = y[i] < 0.5 ? log_sum_exp(log_pi + lp_g, log1m(a / (a + b))) : log_pi + lp_g;
        real r = exp(log_pi + lp_g - lp);
        lp_total += lp;
        // Within-block split: a gamma process shares a block total out as
        // Dirichlet(kappa_w * live_h * a_h); with whole-percent ticks that is
        // a Dirichlet-multinomial on the hourly counts, carrying the within-day
        // shape that day-long blocks cannot see. kappa_w is separate from the
        // block-level kappa because burstiness depends on the time scale.
        // Zero-usage hours contribute lgamma(a) - lgamma(a) = 0, so only
        // nonzero hours are evaluated. Single-hour blocks contribute nothing.
        if (y[i] >= 0.5 && block_ptr[i + 1] - block_ptr[i] > 1) {
          real conc = 0;
          real dm = 0;
          for (j in block_ptr[i]:(block_ptr[i + 1] - 1)) {
            real aj = kappa_w * live[j] * prof[hbin[j]];
            conc += aj;
            if (yh[j] > 0 && aj > 0) dm += lgamma(yh[j] + aj) - lgamma(aj);
          }
          lp_total += dm + lgamma(conc) - lgamma(y[i] + conc);
        }
        al += r * k; be += r * y[i]; a += r; b += 1 - r;
      }
    }
    return lp_total;
  }

  real partial_sum(array[] int cells_slice, int s, int e,
                   vector y, array[] int block_ptr, array[] int hbin, vector live, vector yh,
                   array[] int cell_start, array[] int cell_len, array[] vector prof,
                   array[] vector theta, vector min_mass, vector kappa_w) {
    real total = 0;
    for (n in 1:size(cells_slice)) {
      int c = cells_slice[n];
      vector[6] th = theta[c];
      real s_on = exp(th[4]);
      real p_on = inv_logit(th[5]);
      total += cell_loglik(y, block_ptr, hbin, live, yh, cell_start[c], cell_len[c], prof[c],
                           exp(th[1]), exp(th[2]), exp(th[3]),
                           s_on * p_on, s_on * (1 - p_on), inv_logit(th[6]), min_mass[c], kappa_w[c]);
    }
    return total;
  }
}
data {
  int<lower=1> C;                        // cells
  int<lower=1> T;                        // window types (7d, 5h)
  array[C] int<lower=1, upper=T> ctype;
  array[T] vector[6] x0;                 // prior centers per type (Clojure spec x0)
  int<lower=1> NB;                       // blocks, all cells
  vector<lower=0>[NB] y;
  array[C] int<lower=1> cell_start;
  array[C] int<lower=0> cell_len;
  int<lower=1> NW;                       // (block, hour) entries
  array[NB + 1] int<lower=1> block_ptr;
  array[NW] int<lower=1, upper=168> hbin;
  vector<lower=0, upper=1>[NW] live;
  vector<lower=0>[NW] yh;                // usage in each (block, hour) entry
  vector<lower=0>[C] min_mass;
  vector[168] g_init;                    // log fleet profile estimate (centering)
  int<lower=1> n_daily;                  // profile basis: daily harmonics
  int<lower=0> n_weekly;                 //                weekly harmonics
  int<lower=0> n_weekend;                //                weekend daily harmonics
}
transformed data {
  array[C] int cell_ids;
  for (c in 1:C) cell_ids[c] = c;
  int K = 2 * n_daily + 2 * n_weekly + 1 + 2 * n_weekend;
  matrix[168, K] B;
  for (h in 1:168) {
    real t = h - 1;
    real weekend = t >= 120 ? 1 : 0;   // Saturday and Sunday (bin 0 = Monday 00:00)
    int col = 1;
    for (m in 1:n_daily) {
      B[h, col] = cos(2 * pi() * m * t / 24); B[h, col + 1] = sin(2 * pi() * m * t / 24); col += 2;
    }
    for (m in 1:n_weekly) {
      B[h, col] = cos(2 * pi() * m * t / 168); B[h, col + 1] = sin(2 * pi() * m * t / 168); col += 2;
    }
    B[h, col] = weekend - 2.0 / 7; col += 1;  // mean-zero weekend level
    for (m in 1:n_weekend) {
      B[h, col] = weekend * cos(2 * pi() * m * t / 24); B[h, col + 1] = weekend * sin(2 * pi() * m * t / 24); col += 2;
    }
  }
  // Center the fleet coefficients on the least-squares fit of the pooled
  // log profile the in-JVM estimate starts from.
  vector[K] beta_init = mdivide_left_spd(crossprod(B), B' * (g_init - mean(g_init)));
  // Prior centers for phi, converted from the in-JVM starting point x0.
  array[T] vector[6] phi0;
  for (t in 1:T) {
    real kappa0 = exp(x0[t][1]);
    real alpha0 = exp(x0[t][2]);    // spec starting points all have alpha0 > 1
    real beta0 = exp(x0[t][3]);
    real p0 = inv_logit(x0[t][5]);
    real d0 = inv_logit(x0[t][6]);
    phi0[t][1] = log(p0 * kappa0 * beta0 / (alpha0 - 1));
    phi0[t][2] = -0.5 * log(alpha0 - 1);
    phi0[t][3] = x0[t][1];
    phi0[t][4] = x0[t][5];
    phi0[t][5] = x0[t][4];
    phi0[t][6] = log(log(0.5) / log(d0));
  }
  vector[6] phi_scale = [1.5, 1, 1.5, 1.5, 1, 1]';
}
parameters {
  vector[C] log_kappa_w;                 // within-block Dirichlet concentration per unit mass
  array[C] vector[6] eta;
  array[T] vector[6] mu;
  array[T] vector<lower=0>[6] sigma_theta;
  vector[K] z_g;
  real<lower=0> sigma_g;
  array[C] vector[K] z_prof;
  real<lower=0> tau;
}
transformed parameters {
  array[C] vector[168] prof;
  array[C] vector[6] theta;
  for (c in 1:C) {
    prof[c] = 168 * softmax(B * (beta_init + sigma_g * z_g + tau * z_prof[c]));
    vector[6] phi = phi0[ctype[c]] + phi_scale .* eta[c];
    real kappa = exp(phi[3]);
    real cv = exp(phi[2]);
    real alpha0 = 1 + 1 / square(cv);
    real p = inv_logit(phi[4]);
    real beta0 = exp(phi[1]) * (alpha0 - 1) / (p * kappa);
    real d = pow(0.5, 1 / exp(phi[6]));
    theta[c] = [phi[3], log(alpha0), log(beta0), phi[5], phi[4], logit(d)]';
  }
}
model {
  for (t in 1:T) {
    mu[t] ~ std_normal();
    sigma_theta[t] ~ lognormal(log(0.5), 0.5);
  }
  for (c in 1:C) eta[c] ~ normal(mu[ctype[c]], sigma_theta[ctype[c]]);
  log_kappa_w ~ normal(log(0.1), 1.5);
  for (c in 1:C) z_prof[c] ~ std_normal();
  z_g ~ std_normal();
  sigma_g ~ lognormal(log(0.5), 0.5);
  tau ~ lognormal(log(0.3), 0.5);
  target += reduce_sum(partial_sum, cell_ids, 1, y, block_ptr, hbin, live, yh,
                       cell_start, cell_len, prof, theta, min_mass, exp(log_kappa_w));
}
