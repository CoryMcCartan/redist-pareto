run_trajectory <- function(rmap, m_init, scorer, maximize, bursts=100, chains=10,
                           ncores=parallel::detectCores()) {
    library(furrr)
    plan(multisession, workers=ncores)
    nd = attr(rmap, "ndists")

    plan(multisession, workers=4)
    out = future_map(seq_len(chains), function(i) {
        rev = redist_shortburst(rmap, scorer, max_bursts=bursts,
                                init_plan=m_plans_smc[, i], maximize=maximize,
                                fixed_k=1, reversible=FALSE, verbose=FALSE)
        by_plan(rev$score, nd)
    }, .options=furrr_options(seed=TRUE), .progress=TRUE) |>
        do.call(cbind, args=_)

    out
}

scorer_palmer_schneer <- function(map, group_pop, total_pop, thresh=0.5) {
    group_pop <- rlang::eval_tidy(rlang::enquo(group_pop), map)
    total_pop <- rlang::eval_tidy(rlang::enquo(total_pop), map)
    ndists <- attr(map, "ndists")

    fn <- function(m) {
        pct <- redist:::group_pct(m, group_pop, total_pop, ndists)
        n_above = colSums(pct >= thresh)
        pct[pct >= thresh] = 0
        n_above + redist:::colmax(pct)
    }
    class(fn) <- c("redist_scorer", "function")
    fn
}

