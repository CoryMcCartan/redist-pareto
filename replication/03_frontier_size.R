
res <- expand_grid(n=c(10, 100, 300, 1000, 3000), k=1:10, rep=1:20)

frontier_size <- function(n, k, ...) {
    n - sum(redist:::pareto_dominated(rmvnorm(n, k)))
}

res <- res |>
    mutate(frontier = pmap_dbl(res, frontier_size))

ggplot(res, aes(k, frontier, color=n, group=n)) +
    geom_line(stat="smooth", formula=y ~ log(x), method="lm", lwd=1.2, alpha=0.2) +
    geom_count() +
    scale_color_wa_c("sea_star", name="Sample size", labels=comma, trans="log10") +
    scale_y_log10("Frontier size") +
    scale_size_area(max_size=4, guide="none") +
    labs(x="Dimension") +
    theme_paper()

ggsave(here("paper/figures/mvn_frontier_size.pdf"), width=7, height=5)
