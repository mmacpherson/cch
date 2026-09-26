#!/usr/bin/env -S uv run --quiet --script
# /// script
# requires-python = ">=3.11"
# dependencies = ["numpy", "scipy", "matplotlib"]
# ///
"""Toy figures for usage-model.typ.

Every figure is drawn from data simulated by the model's own generative
process with made-up parameters; no real usage data is involved.

    ./figures.py        # writes fig-*.svg next to this script
"""
import pathlib

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from scipy import special

OUT = pathlib.Path(__file__).resolve().parent
RNG = np.random.default_rng(20260925)
INK, ACCENT, MUTED = "#1f2544", "#3949ab", "#9aa0b8"

plt.rcParams.update({
    "svg.fonttype": "none", "font.size": 8, "axes.edgecolor": MUTED, "axes.labelcolor": INK,
    "xtick.color": INK, "ytick.color": INK, "axes.spines.top": False, "axes.spines.right": False,
})


def fleet_profile():
    """A toy hour-of-week profile (168 bins, mean 1): a workday bump, a quiet
    night, and lighter weekends."""
    h = np.arange(168)
    hod, dow = h % 24, h // 24
    day = np.exp(-0.5 * ((hod - 13) / 3.5) ** 2) + 0.05
    level = np.where(dow >= 5, 0.45, 1.0)
    a = day * level
    return a / a.mean()


def simulate_hours(profile, weeks, kappa=0.08, alpha=6.0, beta=40.0, p_on=0.8, rng=RNG):
    """Hourly usage from the generative model with 24h blocks: each day draws
    an on/off state and an intensity, and the day's usage is split across its
    hours as a gamma process with shape kappa * a_h per hour."""
    out = []
    for w in range(weeks):
        for d in range(7):
            lam = rng.gamma(alpha, 1 / beta)
            on = rng.random() < p_on
            for hr in range(24):
                a = profile[d * 24 + hr]
                out.append(rng.gamma(kappa * a, 1 / lam) if on else 0.0)
    return np.array(out)


def eb_rates(weekly_rates, target):
    """Moment (Fay-Herriot) shrinkage toward `target`, as in
    cch.usage-model/eb-rates."""
    n = np.array([len(w) for w in weekly_rates])
    m = np.array([np.mean(w) if len(w) else 0.0 for w in weekly_rates])
    s2 = np.array([np.var(w, ddof=1) if len(w) > 1 else 0.0 for w in weekly_rates])
    phi = s2[n > 1].sum() / ((m[n > 1] + target[n > 1]) / 2).sum()
    v = phi * ((m + target) / 2) / np.maximum(n, 1)
    t = max(0.0, ((m - target) ** 2 - v).sum() / target.sum())
    w = np.where(t * target > 0, t * target / (t * target + v), 0.0)
    return target + w * (m - target), (phi / t if t > 0 else np.inf)


def smooth(rate):
    ks = np.exp(-0.5 * (np.arange(-6, 7) / 1.5) ** 2)
    ks /= ks.sum()
    sm = np.array([sum(ks[j] * rate[(h + j - 6) % 168] for j in range(13)) for h in range(168)])
    sm = np.maximum(sm, 0.02 * sm.mean())
    return sm / sm.mean()


def fig_daily():
    prof = fleet_profile()
    hours = simulate_hours(prof, weeks=26)
    daily = hours.reshape(-1, 24).sum(1)
    fig, ax = plt.subplots(figsize=(3.3, 1.9))
    ax.hist(daily, bins=40, color=ACCENT, alpha=0.85)
    ax.set_xlabel("daily usage (% of weekly budget)")
    ax.set_ylabel("days")
    zero = (daily < 0.5).mean()
    ax.annotate(f"{zero:.0%} of days\nexactly zero", xy=(0.3, ax.get_ylim()[1] * 0.75),
                xytext=(0.28, 0.8), textcoords="axes fraction", fontsize=7, color=INK,
                arrowprops=dict(arrowstyle="->", color=MUTED))
    fig.tight_layout()
    fig.savefig(OUT / "fig-daily.svg")


def fig_profile():
    fleet = fleet_profile()
    # A sparse cell with a genuinely later workday (3h), five weeks of data.
    true_cell = np.roll(fleet, 3)
    weeks = 5
    hours = simulate_hours(true_cell, weeks=weeks, kappa=0.08, rng=np.random.default_rng(7))
    wk = hours.reshape(weeks, 168)
    mean_rate = hours.mean()
    weekly = [list(wk[:, h] / mean_rate) for h in range(168)]
    raw = np.array([np.mean(w) for w in weekly])
    shrunk, k = eb_rates(weekly, fleet)
    x = np.arange(168) / 24
    fig, ax = plt.subplots(figsize=(6.8, 2.0))
    ax.plot(x, smooth(raw), color=MUTED, lw=0.9, label=f"cell alone ({weeks} weeks, smoothed)")
    ax.plot(x, fleet, color=INK, lw=1.0, ls="--", label="fleet profile")
    ax.plot(x, smooth(shrunk), color=ACCENT, lw=1.6, label=f"shrunk toward fleet (k = {k:.0f})")
    ax.set_xticks(range(8), ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", ""])
    ax.set_ylabel("relative rate $a_h$")
    ax.legend(frameon=False, fontsize=7, ncol=3, loc="upper center", bbox_to_anchor=(0.5, 1.18))
    fig.tight_layout()
    fig.savefig(OUT / "fig-profile.svg")


def fig_fan():
    """A 7d window: three observed days, then the predictive fan from the
    discounted conjugate filter and Monte Carlo over the remaining hours."""
    prof = fleet_profile()
    kappa, al0, be0, a0, b0, d = 0.08, 6.0, 40.0, 3.2, 0.8, 0.6
    hours = simulate_hours(prof, weeks=1, kappa=kappa, alpha=al0, beta=be0, p_on=0.8,
                           rng=np.random.default_rng(3))
    now = 3 * 24 + 15
    cum = np.cumsum(hours)
    # Filter the three and a half observed days (24h blocks, then a partial one).
    al, be, a, b = al0, be0, a0, b0
    for blk in range(4):
        hs = range(blk * 24, min((blk + 1) * 24, now))
        A = sum(prof[h] for h in hs)
        y = sum(hours[h] for h in hs)
        al, be = d * al + (1 - d) * al0, d * be + (1 - d) * be0
        a, b = d * a + (1 - d) * a0, d * b + (1 - d) * b0
        k = kappa * A
        q = lambda u: special.betainc(k, al, u / (u + be))
        pg = q(y + 0.5) - q(max(y - 0.5, 0))
        pi = a / (a + b)
        tot = pi * pg + (1 - pi) * (y < 0.5)
        r = pi * pg / tot
        al, be, a, b = al + r * k, be + r * y, a + r, b + 1 - r
    on_now = hours[now - 15:now].sum() >= 0.5
    n = 3000
    rng = np.random.default_rng(11)
    lam, pis = rng.gamma(al, 1 / be, n), rng.beta(a, b, n)
    paths = np.zeros((n, 168 - now))
    for i in range(n):
        c, blk_on = cum[now - 1], None
        for j, h in enumerate(range(now, 168)):
            if h % 24 == 0 or blk_on is None:
                blk_on = on_now if blk_on is None else (rng.random() < pis[i])
            if blk_on:
                c += rng.gamma(kappa * prof[h], 1 / lam[i])
            paths[i, j] = c
    qs = np.quantile(paths, [0.05, 0.25, 0.5, 0.75, 0.95], axis=0)
    t_obs, t_fut = np.arange(now) / 24, np.arange(now, 168) / 24 + 1 / 24
    fig, ax = plt.subplots(figsize=(6.8, 2.3))
    ax.fill_between(t_fut, qs[0], qs[4], color=ACCENT, alpha=0.15, lw=0, label="90% band")
    ax.fill_between(t_fut, qs[1], qs[3], color=ACCENT, alpha=0.30, lw=0, label="50% band")
    ax.plot(t_fut, qs[2], color=ACCENT, lw=1.6, ls="--", label="median")
    ax.step(t_obs + 1 / 24, cum[:now], where="post", color=INK, lw=1.2, label="observed")
    ax.axhline(100, color="#b23b3b", lw=0.8)
    ax.axvline(now / 24, color=MUTED, lw=0.8, ls=":")
    ax.text(now / 24 + 0.05, 5, "now", color=MUTED, fontsize=7)
    ax.text(0.05, 102, "cap", color="#b23b3b", fontsize=7)
    ax.set_xlim(0, 7)
    ax.set_ylim(0, 125)
    ax.set_xticks(range(8), ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", ""])
    ax.set_ylabel("window usage (%)")
    ax.legend(frameon=False, fontsize=7, ncol=4, loc="upper left")
    fig.tight_layout()
    fig.savefig(OUT / "fig-fan.svg")
    return float(np.mean(paths[:, -1] >= 100)), float(qs[2, -1])


if __name__ == "__main__":
    fig_daily()
    fig_profile()
    p_cap, median = fig_fan()
    print(f"toy fan: median at reset {median:.0f}%, P(cap) {p_cap:.2f}")
