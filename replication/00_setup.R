suppressMessages({
    library(here)
    # additional packages here
    library(redist)
    library(alarmdata)
    library(tidyverse)
    library(wacolors)
    library(ggredist)
    library(geomtextpath)
    library(scales)
    library(patchwork)
})

stopifnot(packageVersion("redist") >= "4.1.0.9999")

# Color palettes, etc.

theme_paper = function() {
    theme_bw(base_family="Times", base_size=10) +
        theme(plot.background=element_blank())
}

# Helper functions ------
# Generate n random draws from spherical Gaussian in k dimensions
rmvnorm <- function(n, k=1) {
    matrix(rnorm(n*k), nrow=k, ncol=n)
}
