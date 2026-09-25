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
//   * Activity profiles: log a_c = g + delta_c, normalized to mean 1, where
//     g is a fleet profile and delta_c a cell deviation. Both are smoothed
//     with the same circular Gaussian kernel as the Clojure estimate. The
//     deviation scale tau is learned: the full-Bayes analogue of the
//     empirical-Bayes shrinkage strength k.
//   * Parameters: the six unconstrained parameters of each cell are drawn
//     around a mean for its window type (5h or 7d), with learned spread.
functions {
  real cell_loglik(vector y, array[] int block_ptr, array[] int hbin, vector live,
                   int start, int len, vector prof, real kappa, real al0, real be0,
                   real a0, real b0, real d, real min_mass) {
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
        real lp_g = lo > 0
          ? log_diff_exp(beta_lcdf(hi / (hi + be) | k, al), beta_lcdf(lo / (lo + be) | k, al))
          : beta_lcdf(hi / (hi + be) | k, al);
        real log_pi = log(a / (a + b));
        real lp = y[i] < 0.5 ? log_sum_exp(log_pi + lp_g, log1m(a / (a + b))) : log_pi + lp_g;
        real r = exp(log_pi + lp_g - lp);
        lp_total += lp;
        al += r * k; be += r * y[i]; a += r; b += 1 - r;
      }
    }
    return lp_total;
  }

  real partial_sum(array[] int cells_slice, int s, int e,
                   vector y, array[] int block_ptr, array[] int hbin, vector live,
                   array[] int cell_start, array[] int cell_len, array[] vector prof,
                   array[] vector theta, vector min_mass) {
    real total = 0;
    for (n in 1:size(cells_slice)) {
      int c = cells_slice[n];
      vector[6] th = theta[c];
      real s_on = exp(th[4]);
      real p_on = inv_logit(th[5]);
      total += cell_loglik(y, block_ptr, hbin, live, cell_start[c], cell_len[c], prof[c],
                           exp(th[1]), exp(th[2]), exp(th[3]),
                           s_on * p_on, s_on * (1 - p_on), inv_logit(th[6]), min_mass[c]);
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
  vector<lower=0>[C] min_mass;
  matrix[168, 168] S;                    // circular smoothing kernel
  vector[168] g_init;                    // log fleet profile estimate (centering)
}
transformed data {
  array[C] int cell_ids;
  for (c in 1:C) cell_ids[c] = c;
}
parameters {
  vector[168] z_g;
  real<lower=0> sigma_g;
  array[C] vector[168] z_delta;
  real<lower=0> tau;
  array[T] vector[6] mu;
  array[T] vector<lower=0>[6] sigma_theta;
  array[C] vector[6] eta;
}
transformed parameters {
  vector[168] g = g_init + sigma_g * (S * z_g);
  array[C] vector[168] prof;
  array[C] vector[6] theta;
  for (c in 1:C) {
    vector[168] la = g + tau * (S * z_delta[c]);
    prof[c] = 168 * softmax(la);
    theta[c] = mu[ctype[c]] + sigma_theta[ctype[c]] .* eta[c];
  }
}
model {
  z_g ~ std_normal();
  sigma_g ~ normal(0, 0.5);
  for (c in 1:C) z_delta[c] ~ std_normal();
  tau ~ normal(0, 0.5);
  for (t in 1:T) {
    mu[t] ~ normal(x0[t], 2);
    sigma_theta[t] ~ normal(0, 0.5);
  }
  for (c in 1:C) eta[c] ~ std_normal();
  target += reduce_sum(partial_sum, cell_ids, 1, y, block_ptr, hbin, live,
                       cell_start, cell_len, prof, theta, min_mass);
}
