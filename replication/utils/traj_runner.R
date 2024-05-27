library(docopt)

"Generate and save shortburst trajectories.

Usage:
  trajectory_1d.R <state> (--dev | --comp | --mmd) [-b <bursts>] [-c <chains>]

Options:
  -h --help     S             Show this screen.
  -b, --max_bursts=<bursts>   Maximum number of bursts to run [default: 100].
  -c, --chains=<chains>       Number of chains to run [default: 10].

" -> doc
args <- docopt(doc)

suppressPackageStartupMessages({
    library(redist)
    library(here)
    source(here("replication/utils/trajectory_1d.R"))
})

rmap = alarmdata::alarm_50state_map(args$state)
m_plans_smc = alarmdata::alarm_50state_plans(args$state, stats=FALSE) |>
    as.matrix()

if (args$dev) {
    maximize = FALSE
    type = "dev"
    scorer = scorer_pop_dev(rmap)
} else if (args$comp) {
    maximize = TRUE
    type = "comp"
    scorer = scorer_frac_kept(rmap)
} else if (args$mmd) {
    maximize = TRUE
    type = "mmd"
    scorer = scorer_palmer_schneer(rmap, vap_black, vap)
}

bursts = as.integer(args$max_bursts)
chains = min(as.integer(args$chains), 5000L)

traj_m = run_trajectory(rmap, m_plans_smc, scorer, maximize, bursts, chains)

path = paste0(
    "data-out/trajectories/",
    "state=", attributes(rmap)$analysis_name, "_",
    "scorer=", "dev", "_",
    "bursts=", nrow(traj_m) - 1, "_",
    "chains=", ncol(traj_m),
    ".rds"
)
saveRDS(traj_m, here(path), compress="gzip")
