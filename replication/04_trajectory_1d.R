suppressPackageStartupMessages({
    library(here)
    source(here("replication/00_setup.R"))
    source("replication/utils/trajectory_1d.R")
})

ia <- alarm_50state_map("IA") |>
    set_pop_tol(0.05)
nd = attr(ia, "ndists")
m_plans_smc <- redist_smc(ia, 500, ref_name=FALSE, adapt_k_thresh=1) |>
    as.matrix()

score_dev <- scorer_pop_dev(ia)

traj_m = read_rds(here("data-out/trajectories/state=IA_2020_scorer=dev_bursts=20000_chains=200.rds"))
if (FALSE) {
    traj_m = run_trajectory(ia, m_plans_smc, score_dev, maximize=FALSE, bursts=20000, chains=200, ncores=4)

    path = paste0(
        "data-out/trajectories/",
        "state=", attributes(ia)$analysis_name, "_",
        "scorer=", "dev", "_",
        "bursts=", nrow(traj_m) - 1, "_",
        "chains=", ncol(traj_m),
        ".rds"
    )
    write_rds(traj_m, here(path), compress="gz")
}

# Does the starting point matter? No
cor.test(traj_m[1, ], traj_m[20001, ]) |>

matplot(log10(seq_len(nrow(traj_m))), traj_m, type="l", col="#00000040", lty="solid")
# matplot(log10(seq_len(nrow(traj_m))), traj_m[, 1:25], type="s", col="#00000080", lty="solid")
# matplot(log10(seq_len(nrow(traj_m) - 1)), diff(traj_m[, 1:25]), type="s", col="#00000080", lty="solid")

# Convert to run-length encoding
d_rl = apply(traj_m, 2, function(x) {
    rl = rle(x)
    new_tibble(list(
        start = rep(x[1], length(rl$values) - 1),
        value = rl$values[-1],
        wait = rl$lengths[-1],
        improve = -diff(rl$values)
    ))
}) |>
    bind_rows(.id = "chain") |>
    mutate(chain = as.integer(chain))

d = d_rl |>
    group_by(chain) |>
    mutate(step = cumsum(wait),
           l_improve = lag(improve),
           l_rel_improve = l_improve / lag(start - value),
           rel_improve = improve / (start - value),
           l_wait = lag(wait)) |>
    ungroup() |>
    drop_na()

ggplot(d, aes(l_rel_improve, 1/wait)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(method=lm, formula=y~x) +
    geom_point()
ggplot(d, aes(step, value)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_smooth(method=lm, formula=y~x) +
    geom_point()

bs = boot::boot(x, function(x, i) {
    x = x[i]
    rk = rank(x, ties.method="random")
    2 * x[rk == 1] - x[rk == 2]
}, R = 10000)


cutoffs = round(10^seq(1, 4, length.out=20))
d_oos <- map(cutoffs, function(xc) {
    idx_train = with(d, which(step < xc))
    idx_test = with(d, which(step >= xc))

    m_wait = glm(wait - 1 ~ log(l_rel_improve) + log(step), data=d[idx_train, ], family=poisson())
    m_ri = lm(log(rel_improve) ~ log(l_wait) + log(step), data=d[idx_train, ], y=TRUE)

    new_tibble(list(
        cutoff = xc,
        n_obs = length(idx_train),
        n_pred = length(idx_test),
        r2_wait = cor(d$wait[idx_test], predict(m_wait, newdata=d[idx_test, ]))^2,
        r2_ri = cor(log(d$rel_improve[idx_test]), predict(m_ri, newdata=d[idx_test, ]))^2,
        cor_resid = cor(resid(m_ri), resid(m_wait))
    ))
}) |>
    bind_rows()

d_oos |>
    pivot_longer(r2_wait:cor_resid, names_to="metric", values_to="value") |>
ggplot(aes(cutoff, value, color=metric)) +
    geom_line(linewidth=1) +
    geom_point(aes(size=n_obs)) +
    scale_size_area(max_size=8) +
    scale_x_log10()

