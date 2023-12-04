library(dplyr)
library(metanetwork)


nodes <- system.file("extdata", "nodes.csv", package = "metanetwork") |>
    read.csv()
links <- system.file("extdata", "links.csv", package = "metanetwork") |>
    read.csv()

grp_foc <- as.list(unique(nodes$subnetwork))
names(grp_foc) <- unlist(grp_foc)
grp_foc <- c(All = "", grp_foc)


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
    nodeColPal <- c("Depuis le fichier source", nodeColPal)
}
