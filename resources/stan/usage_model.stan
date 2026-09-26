// Hierarchical zero-inflated dynamic gamma model of agent quota usage.
//
// Same likelihood as cch.usage-model (see its docstring): per cell (an
// agent/window pair), usage over a block with profile mass A is
// Gamma(kappa*A, lambda) when the block is 'on', and 0 otherwise; the
// intensity lambda ~ Gamma(alpha, beta) and on-probability pi ~ Beta(a, b)
// are filtered through the blocks, relaxing toward their priors with
// discount d between blocks. Block likelihoods are interval probabilities
// of the compound-gamma predictive y/(y+beta) ~ Beta(kappa*A, alpha), since
// usage is reported in whole percent. 7d cells use day-long blocks and 5h
// cells hourly ones, as in the in-JVM model (day blocks predict weekly
// totals better; see claude-code-hooks-w7v).
//
// Activity profiles, over a smooth hour-of-week basis split into within-day
// columns (daily harmonics, and daily harmonics on weekends) and weekly
// columns (weekly harmonics and a weekend level); basis sizes are data:
//   log a_c = B_day  (fleet_day  + agent_day[agent(c)])
//           + B_week (fleet_week + cell_week[c]),   normalized to mean 1.
// Day-long blocks cannot see within-day shape (daily harmonics integrate to
// zero over a day), so the within-day deviation belongs to the agent and is
// identified by its hourly 5h cell (the 5h and 7d meters count the same
// usage); a 7d cell deviates from the fleet only in weekly shape. An agent
// without a 5h cell keeps the fleet within-day shape. Deviation scales have
// half-normal priors, so full pooling stays possible. The fleet profile is
// well identified by all cells together, so its prior scale is fixed (0.5).
//
// Sliver blocks (partial blocks at window edges) skip the on/off update. They
// are detected from live hours, which are data: testing the profile-weighted
// mass instead made the log density jump as the profile moved, which
// collapsed the HMC step size.
//
// Cell parameters phi, around a mean for the window type (5h or 7d):
//   [log geometric-mean usage rate = log p + log kappa + log beta0 - digamma(alpha0),
//    log alpha0 (intensity shape; any alpha0 > 0, including heavy tails),
//    log kappa (within-block burstiness),
//    logit on-probability,
//    log on/off prior strength,
//    log memory half-life in blocks (d = 0.5^(1/half-life))].
// These are identified directly; the natural (kappa, alpha0, beta0, a0, b0, d)
// form had posterior ridges up to corr 0.85. Cells are centered on their type
// mean (each has hundreds of blocks). The spread sigma_theta cannot be
// estimated from 2-3 cells per type, so its lognormal prior is the pooling
// assumption: agents differ by about half a prior scale unit.
// `theta` reports each cell in cch.usage-model's unconstrained convention
// [log kappa, log alpha0, log beta0, log s, logit p, logit d].
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

  real cell_loglik(vector y, array[] int block_ptr, array[] int hbin, vector live,
                   int start, int len, vector prof, real kappa, real al0, real be0,
                   real a0, real b0, real d, real min_mass) {
    real al = al0; real be = be0; real a = a0; real b = b0;
    real lp_total = 0;
    for (i in start:(start + len - 1)) {
      real A = 0;
      real live_hours = 0;
      for (j in block_ptr[i]:(block_ptr[i + 1] - 1)) {
        A += live[j] * prof[hbin[j]];
        live_hours += live[j];
      }
      al = d * al + (1 - d) * al0; be = d * be + (1 - d) * be0;
      a = d * a + (1 - d) * a0;    b = d * b + (1 - d) * b0;
      if (live_hours < min_mass) {   // data-only test: no jump as the profile moves
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
  int<lower=1> A;                        // agents
  array[C] int<lower=1, upper=T> ctype;
  array[C] int<lower=1, upper=A> agent;
  array[T] vector[6] x0;                 // in-JVM starting point per type (centers the prior)
  int<lower=1> NB;                       // blocks, all cells
  vector<lower=0>[NB] y;
  array[C] int<lower=1> cell_start;
  array[C] int<lower=0> cell_len;
  int<lower=1> NW;                       // (block, hour) entries
  array[NB + 1] int<lower=1> block_ptr;
  array[NW] int<lower=1, upper=168> hbin;
  vector<lower=0, upper=1>[NW] live;
  vector<lower=0>[C] min_mass;
  vector[168] g_init;                    // log fleet profile estimate (centering)
  int<lower=1> n_daily;                  // profile basis: daily harmonics
  int<lower=0> n_weekly;                 //                weekly harmonics
  int<lower=0> n_weekend;                //                weekend daily harmonics
}
transformed data {
  array[C] int cell_ids;
  for (c in 1:C) cell_ids[c] = c;
  int KD = 2 * n_daily + 2 * n_weekend;  // within-day columns
  int KW = 2 * n_weekly + 1;             // weekly columns
  matrix[168, KD] B_day;
  matrix[168, KW] B_week;
  for (h in 1:168) {
    real t = h - 1;
    real weekend = t >= 120 ? 1 : 0;     // Saturday and Sunday (bin 0 = Monday 00:00)
    for (m in 1:n_daily) {
      B_day[h, 2 * m - 1] = cos(2 * pi() * m * t / 24);
      B_day[h, 2 * m] = sin(2 * pi() * m * t / 24);
    }
    for (m in 1:n_weekend) {
      B_day[h, 2 * n_daily + 2 * m - 1] = weekend * cos(2 * pi() * m * t / 24);
      B_day[h, 2 * n_daily + 2 * m] = weekend * sin(2 * pi() * m * t / 24);
    }
    for (m in 1:n_weekly) {
      B_week[h, 2 * m - 1] = cos(2 * pi() * m * t / 168);
      B_week[h, 2 * m] = sin(2 * pi() * m * t / 168);
    }
    B_week[h, KW] = weekend - 2.0 / 7;   // mean-zero weekend level
  }
  // Center the fleet coefficients on the least-squares fit of the pooled log
  // profile that the in-JVM estimate starts from.
  matrix[168, KD + KW] B = append_col(B_day, B_week);
  vector[KD + KW] beta_init = mdivide_left_spd(crossprod(B), B' * (g_init - mean(g_init)));
  // Prior centers for phi, converted from the in-JVM starting point x0.
  array[T] vector[6] phi0;
  for (t in 1:T) {
    phi0[t][1] = log_inv_logit(x0[t][5]) + x0[t][1] + x0[t][3] - digamma(exp(x0[t][2]));
    phi0[t][2] = x0[t][2];
    phi0[t][3] = x0[t][1];
    phi0[t][4] = x0[t][5];
    phi0[t][5] = x0[t][4];
    phi0[t][6] = log(log(0.5) / log(inv_logit(x0[t][6])));
  }
  vector[6] phi_scale = [1.5, 1, 1.5, 1.5, 1, 1]';
}
parameters {
  vector[KD + KW] z_g;
  array[A] vector[KD] z_agent_day;
  real<lower=0> tau_day;
  array[C] vector[KW] z_cell_week;
  real<lower=0> tau_week;
  array[C] vector[6] eta;                // cell phi, in prior-scale units (centered)
  array[T] vector[6] mu;                 // type means of phi, in prior-scale units
  array[T] vector<lower=0>[6] sigma_theta;
}
transformed parameters {
  array[C] vector[168] prof;
  array[C] vector[6] theta;
  {
    vector[KD + KW] fleet = beta_init + 0.5 * z_g;
    for (c in 1:C) {
      vector[168] la = B_day * (fleet[1:KD] + tau_day * z_agent_day[agent[c]])
                       + B_week * (fleet[(KD + 1):(KD + KW)] + tau_week * z_cell_week[c]);
      prof[c] = 168 * softmax(la);
    }
  }
  for (c in 1:C) {
    vector[6] phi = phi0[ctype[c]] + phi_scale .* eta[c];
    // phi[1] = log p + E[log(kappa / lambda)]: defined for every alpha0 > 0,
    // and it separates location from shape.
    real alpha0 = exp(phi[2]);
    real beta0 = exp(phi[1] - log_inv_logit(phi[4]) - phi[3] + digamma(alpha0));
    real d = pow(0.5, 1 / exp(phi[6]));
    theta[c] = [phi[3], phi[2], log(beta0), phi[5], phi[4], logit(d)]';
  }
}
model {
  z_g ~ std_normal();
  for (a in 1:A) z_agent_day[a] ~ std_normal();
  tau_day ~ normal(0, 0.5);
  for (c in 1:C) z_cell_week[c] ~ std_normal();
  tau_week ~ normal(0, 0.5);
  for (t in 1:T) {
    mu[t] ~ std_normal();
    sigma_theta[t] ~ lognormal(log(0.5), 0.5);
  }
  for (c in 1:C) eta[c] ~ normal(mu[ctype[c]], sigma_theta[ctype[c]]);
  target += reduce_sum(partial_sum, cell_ids, 1, y, block_ptr, hbin, live,
                       cell_start, cell_len, prof, theta, min_mass);
}
