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
# get color to match other figure
pct_along <- (log10(200) - 1) / (4 - 1)
col <- do.call(rgb, as.list(grDevices::colorRamp(wacolors$sea_star)(pct_along) / 255))

ggplot(d_sb_pareto_2, aes(dev, comp, group=rep)) +
    facet_wrap(~ b, labeller = function(d) {
        list(b=str_glue("{d$b} samples per burst"))
    }) +
    geom_line(linewidth=0.6, color=col) +
    # scale_color_wa_c("sea_star", guide="none") +
    scale_x_continuous("Maximum population deviation", labels=percent) +
    labs(y="Polsby-Popper compactness (higher is more compact)",
         color="Burst size") +
    theme_paper() +
    theme(panel.spacing.x=unit(0.5, "cm"))

ggsave(here("paper/figures/pareto_burst_size.pdf"), width=7, height=3.5)
