library(dplyr)
library(metanetwork)


nodes <- system.file("extdata", "nodes.csv", package = "metanetwork") |>
    read.csv()
links <- system.file("extdata", "links.csv", package = "metanetwork") |>
    read.csv()

nodeColPal <- c(
    "viridis",
    "magma",
    "inferno",
    "plasma",
    "cividis",
    "rocket",
    "mako",
    "turbo"
)

if ("col" %in% names(nodes)) {
    nodeColPal <- c("from file", nodeColPal)
}
