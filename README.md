# metanetwork
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)


Package to generate circular hierarchical edge bundling graphs to represent metanetworks


# Installation 

Package must be installed from GitHub.

```R
install.packages("remotes")
remotes::install_github("inSilecoInc/metanetwork")
```

# How to 

## Generate the example

```R
library("metanetwork")
metanetwork(
    metanetwork:::nodes,
    metanetwork:::links, 
    focus = c("Species", "Drivers", "Managers"), 
    legend = FALSE,
    export = FALSE
)
```

## Shiny App

```R
library("metanetwork")
run_metanetwork_app()
```