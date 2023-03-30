# Load data ---------
map <- alarm_50state_map("IA") |>
    set_pop_tol(0.01)
perim_df <- redistmetrics::prep_perims(map)
areas <- as.numeric(sf::st_area(map))

score_dev <- scorer_pop_dev(map)
score_comp <- scorer_polsby_popper(map, perim_df, areas)

# Run MCMC ---------
set.seed(02138)
N_smc = 1000L
plans_mcmc <- redist_mergesplit(map, N_smc, init_name=FALSE)
m <- as.matrix(plans_mcmc)
plans_mcmc <- plans_mcmc |>
    as_tibble() |>
    filter(district == 1) |>
    mutate(dev = score_pop(m),
           comp = score_comp(m))

# Run Pareto bursts ---------
set.seed(02138)

N_sb = c(10, 100, 1000, 10000)
scorer <- combine_scorers(score_dev, score_comp)
plans_sb <-  map(N_sb, function(N) {
    cat(N, "\n")
    redist_shortburst(map, scorer, burst_size=10, max_bursts=N,
                      maximize=c(FALSE, TRUE), verbose=FALSE)
})

d_sb_pareto <- map(plans_sb, function(pl) {
    attr(pl, "pareto_scores") |>
        `colnames<-`(c("dev", "comp")) |>
        as_tibble()
}) |>
    tibble(N = N_sb, df=_) |>
    unnest(df)


# Comparison plot ----------
ggplot(d_sb_pareto, aes(dev, comp, color=N, group=N)) +
    geom_point(aes(dev, comp), data=plans_mcmc,
               color="#888888", size=0.8, inherit.aes=FALSE) +
    geom_textline(aes(label=str_c(comma(N), " total bursts")),
                  family="Times", size=3.0, hjust=0.90, linewidth=1.2) +
    scale_color_wa_c("sea_star", trans="log10", labels=comma) +
    scale_x_continuous("Maximum population deviation", labels=percent) +
    labs(y="Polsby-Popper compactness (higher is more compact)",
         color="Total bursts") +
    theme_paper()

ggsave(here("paper/figures/pareto_sb_demo.pdf"), width=6, height=4.5)

# Pareto front plot ----------
best <- tail(plans_sb, 1)[[1]]
m_pareto <- attr(best, "pareto_front")
sc_pareto <- attr(best, "pareto_score")

pl <- lapply(seq_len(ncol(m_pareto)),  function(i) {
    suppressMessages({
        plot(map, as.factor(m_pareto[, i])) +
            scale_fill_coast() +
            labs(title=str_glue("Pop. dev.: {percent(sc_pareto[i, 1], 0.001)}
                                P-P comp.:  {number(sc_pareto[i, 2], 0.01)}")) +
            coord_sf(expand=FALSE) +
            theme_void(base_family="Times", base_size=9) +
            theme(plot.margin=margin(r=15))
    })
})

wrap_plots(pl, nrow=2)
ggsave(here("paper/figures/pareto_maps.pdf"), width=8.0, height=2.375)

