# Finding Pareto Efficient Redistricting Plans with Short Bursts

Redistricting practitioners must balance many competing constraints and 
criteria when drawing district boundaries.
To aid in this process, researchers have developed many methods for optimizing
districting plans according to one or more criteria.
This research note extends a recently-proposed single-criterion optimization
method, *short bursts* [@cannon2023voting], to handle the multi-criterion case, 
and in doing so approximate the Pareto frontier for any set of constraints.
We study the empirical performance of the method in a realistic setting and 
find it behaves as expected and is not very sensitive to algorithmic parameters.
The proposed approach, which is implemented in open-source software, 
should allow researchers and practitioners 
to better understand the tradeoffs inherent to the redistricting process.

## Replication

To replicate the figures and analyses in the paper, run the scripts in `replication/` in order:

``` r
lapply(sort(Sys.glob("replication/*.R")), source)
```

Then run `quarto::quarto_render("paper/pareto.qmd")` to generate the paper.

