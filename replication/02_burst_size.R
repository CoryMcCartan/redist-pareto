# Run Pareto bursts ---------
set.seed(02138)

param <- expand_grid(b=c(5, 10, 20), rep=1:10)
N <- 200

plans_sb_2 <- pmap(param, function(b, ...) {
    cat(".")
    redist_shortburst(map, scorer, burst_size=b, max_bursts=N,
                      maximize=c(FALSE, TRUE), verbose=FALSE)
})

d_sb_pareto_2 <- map(plans_sb_2, function(pl) {
    attr(pl, "pareto_scores") |>
        `colnames<-`(c("dev", "comp")) |>
        as_tibble()
}) |>
    tibble(param, df=_) |>
    unnest(df)

## plot ----
ggplot(d_sb_pareto_2, aes(dev, comp, color=b, group=rep)) +
    facet_wrap(~ b, labeller = function(d) {
        list(b=str_glue("{d$b} samples per burst"))
    }) +
    geom_line(linewidth=0.7) +
    scale_color_wa_c("sea_star") +
    scale_x_continuous("Maximum population deviation", labels=percent) +
    labs(y="Polsby-Popper compactness (higher is more compact)",
         color="Burst size") +
    theme_paper() +
    theme(legend.position=c(0.92, 0.4),
          panel.spacing.x=unit(0.5, "cm"),
          legend.background=element_blank())

ggsave(here("paper/figures/pareto_burst_size.pdf"), width=7, height=3.5)
