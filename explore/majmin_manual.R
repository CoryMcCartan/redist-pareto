library(redistverse)
library(tidyverse)
library(alarmdata)
library(here)
source(here("replication/utils/trajectory_1d.R"))

map = alarm_50state_map("LA") |>
    set_pop_tol(0.01) |>
    mutate(bvap = vap_black / (vap + 0.0001),
           bvap_q = cume_dist(bvap))
nd = attr(map, "ndists")
m_plans_smc = alarm_50state_plans("LA", stats=FALSE) |>
    as.matrix()
g_ctr = sf::st_centroid(map$geometry)

res = redist::redist_shortburst(map, scorer_palmer_schneer(map, vap_black, vap),
                                init_plan = pl, max_bursts=500, maximize=TRUE,
                                fixed_k=1, reversible=FALSE)

# Some random flow stuff ---------------------------
pl = last_plan(res)
c(redist:::group_pct(matrix(last_plan(res), ncol=1), map$vap_black, map$vap, nd)) |> sort()

flow_potential <- function(pl) {
    d_bvap = c(redist:::group_pct(matrix(pl, ncol=1), map$vap_black, map$vap, nd))
    distr_to = which.max(d_bvap)
    thr_bvap = d_bvap[distr_to]

    g_distr = sf::st_union(map$geometry[pl == distr_to], is_coverage=TRUE)

    plot(map, bvap > thr_bvap) +
        geom_district(aes(group=pl == distr_to), color="white", fill=NA, linewidth=0.8)
}

adj_to = redist:::get_plan_graph(map$adj, nrow(map), pl, nd)[[distr_to]] + 1
idx = which(map$bvap > thr_bvap  &  pl %in% adj_to)
dists = st_distance(g_distr, g_ctr[idx])
dists = scale(as.numeric(dists), scale=TRUE, center=FALSE)
ker_dist = rep(0, nrow(map))
ker_dist[idx] = exp(-dists)

plot(map, ker_dist)

tapply(map$vap_black * ker_dist, pl, sum) / tapply(map$vap * ker_dist, pl, sum)
plot(map, pl==6)

traj_m = read_rds(here("data-out/trajectories/state=LA_2020_scorer=dev_bursts=5000_chains=20.rds"))
N = nrow(traj_m)
cor.test(traj_m[1, ], traj_m[N, ])
# plot(traj_m[1, ], traj_m[N, ])

matplot(log10(seq_len(nrow(traj_m))), traj_m, type="s", col="#00000080", lty="solid")

res_20 = redist::redist_shortburst(map, scorer_palmer_schneer(map, vap_black, vap),
                                   init_plan = map$cd_2020, max_bursts=500, maximize=TRUE,
                                   fixed_k=1, burst_size=15, reversible=FALSE)
res_20b = redist::redist_shortburst(map, scorer_palmer_schneer(map, vap_black, vap),
                                   init_plan = map$cd_2020, max_bursts=1500, maximize=TRUE,
                                   fixed_k=1, burst_size=2, reversible=FALSE)
res_20c = redist::redist_shortburst(map, scorer_palmer_schneer(map, vap_black, vap),
                                   init_plan = last_plan(res_20c), max_bursts=1500, maximize=TRUE,
                                   fixed_k=1, burst_size=3, reversible=FALSE)

plot(map, bvap) +
    geom_district(aes(group=last_plan(res_20c)), color="white", fill=NA, linewidth=0.8)

ggplot(map, aes(fill=vap_black, denom=vap, group=last_plan(res_20c))) +
    geom_district()

# Local maxima / saddle points ? ---------------------------

adj_edges = imap(map$adj, function(x, i) {
    cbind(i, x + 1, sum(map$vap_white[c(i, x+1)]))
}) |>
    do.call(rbind, args=_)

lcl_max = imap_lgl(map$adj, function(x, i) {
    all(map$bvap[i] > map$bvap[x + 1])
}) & (map$bvap > 0.5)
plot(map, lcl_max)

xy = st_coordinates(g_ctr[lcl_max])
delaunay = deldir::deldir(xy)
plot(delaunay)

relnbr_inc = pmap_lgl(delaunay$dirsgs, function(ind1, ind2, thirdv1, thirdv2, ...) {
    v1 = c(rep(ind1, 3), rep(ind2, 2))
    v2 = c(ind2, thirdv1, thirdv2, thirdv1, thirdv2)
    if (thirdv1 < 0) return(FALSE)
    if (thirdv2 < 0) return(FALSE)
    sides = rowSums((xy[v1, ] - xy[v2, ])^2)
    all(sides[1] < sides[2:3]) || all(sides[1] < sides[4:5])
})

relnbr = delaunay$dirsgs[relnbr_inc, ]
ggplot(relnbr, aes(x=xy[ind1, 1], xend=xy[ind2, 1], y=xy[ind1, 2], yend=xy[ind2, 2])) +
    geom_segment()

idx_lcl_max = which(lcl_max)
sp = pmap(delaunay$dirsgs[relnbr_inc, ], function(ind1, ind2, ...) {
    rlemon::ShortestPath(adj_edges[, 1], adj_edges[, 2], adj_edges[, 3],
                         nrow(map), idx_lcl_max[ind1], idx_lcl_max[ind2])$list_paths[[1]]
}) |>
    unlist() |>
    unique()

plot(map, lcl_max)
plot(map, lcl_max + (seq_len(nrow(map)) %in% sp))
plot(map, (map$bvap > 0.5) + (seq_len(nrow(map)) %in% sp))

pseudo_cty = 1L + ((map$bvap > 0.5) | ((seq_len(nrow(map)) %in% sp) & (map$bvap > 0.15)))
plot(map, pseudo_cty)

constr = redist_constr(map) |>
    add_constr_grp_hinge(8, vap_black, vap, 0.51) |>
    add_constr_grp_hinge(-8, vap_black, vap, 0.38)
plot(constr)

plans_raw = redist_smc(map, 250, adapt_k_thresh=0.95, seq_alpha=0.9, ncores=4) |>
    mutate(bvap = group_frac(map, vap_black, vap))
plans_mmd = redist_smc(map, 500, pseudo_cty, constraints=constr, ref_name=FALSE,
                       adapt_k_thresh=0.95, seq_alpha=0.9, ncores=4) |>
    mutate(bvap = group_frac(map, vap_black, vap))

# plans_raw |>
plans_mmd |>
    plot(bvap, geom="boxplot")
best_idx = as.integer(plans_mmd$draw[which.max(plans_mmd$bvap)])

ggplot(map, aes(fill=vap_black, denom=vap, group=as.matrix(plans_mmd)[, best_idx])) +
    geom_district()

res_mmd = redist_shortburst(map, init_plan = as.matrix(plans_mmd)[, best_idx],
                            counties = pseudo_cty,
                            score_fn = scorer_palmer_schneer(map, vap_black, vap),
                            # score_fn = scorer_group_pct(map, vap_black, vap, k=2),
                            burst_size = 4, max_bursts=1500, maximize=TRUE,
                            fixed_k=1, reversible=FALSE)



ggplot(map, aes(fill=vap_black, denom=vap, group=last_plan(res_mmd))) +
    geom_district()

boxplot(map$bvap ~ pseudo_cty)
boxplot(map$bvap ~ lcl_max)

res_mmd = redist_shortburst(map, init_plan = last_plan(res_mmd),
                            # counties = 1L + (map$bvap > 0.5),
                            score_fn = scorer_palmer_schneer(map, vap_black, vap),
                            burst_size = 5, max_bursts=1000, maximize=TRUE,
                            fixed_k=1, reversible=FALSE)

# plans_mmd |>
res_mmd |>
    add_reference(map$cd_2020) |>
    mutate(bvap = group_frac(map, vap_black, vap)) |>
    plot(bvap, geom="boxplot")

c(redist:::group_pct(matrix(last_plan(res_mmd), ncol=1), map$vap_black, map$vap, nd)) |> sort()
