library(redistverse)
library(tidyverse)
library(alarmdata)

map = alarm_50state_map("AL") |>
    mutate(bvap = vap_black / vap,
           bvap_q = cume_dist(bvap))

map_m = map |>
    # mutate(bvap_grp = cut(bvap_q, c(0, 2/7, 5/7, 1))) |>
    mutate(bvap_grp = cut(bvap, c(0, seq(0.2, 0.5, length.out=100), 1), include.lowest=TRUE),
           merge_id = str_c(county, "_", as.integer(bvap_grp), "_",
                            geomander::check_contiguity(adj, str_c(county, "_", bvap_grp))$component)) |>
    merge_by(merge_id, drop_geom=FALSE) |>
    mutate(bvap = vap_black / vap)

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

res = map |>
    # redist_shortburst(scorer_group_pct(map, vap_black, vap, k=1),
    redist_shortburst(scorer_palmer_schneer(map, vap_black, vap),
                      max_bursts=5000, burst_size=15, counties=county) |>
    match_numbers(map$cd_2020) |>
    mutate(bvap = group_frac(map, vap_black, vap))
res_m = map_m |>
    # redist_shortburst(scorer_group_pct(map_m, vap_black, vap, k=1),
    redist_shortburst(scorer_palmer_schneer(map_m, vap_black, vap),
                      max_bursts=5000, burst_size=15, counties=county) |>
    match_numbers(map_m$cd_2020) |>
    mutate(bvap = group_frac(map_m, vap_black, vap))

plot(map, factor(last_plan(res)))
plot(map, factor(last_plan(res2)))
plot(map, factor(last_plan(pullback(res_m))))
plot(map_m, factor(last_plan(res_m)))
plot(map, bvap)
redist.plot.plans(res, 500, map, bvap)
redist.plot.plans(res_m, 500, map_m, bvap)


res2 = map |>
    redist_shortburst(scorer_group_pct(map, vap_black, vap, k=2),
                      init_plan=last_plan(res),
                      burst_size=30, counties=county) |>
    match_numbers(map$cd_2020) |>
    mutate(bvap = group_frac(map, vap_black, vap))
