# source("data-raw/example_data.R")
# Individual nodes
drivers <- data.frame(network = "Environmental", subnetwork = "Drivers", category = paste0("driver_", 1:15))
species <- data.frame(network = "Environmental", subnetwork = "Species", category = paste0("species_", 1:30))
actions <- data.frame(network = "Social", subnetwork = "Actions", category = paste0("action_", 1:9))
managers <- data.frame(network = "Social", subnetwork = "Managers", category = paste0("manager_", 1:9))
beneficiaries <- data.frame(network = "Social", subnetwork = "Beneficiaries", category = paste0("beneficiary_", 1:9))

# Nodes dataframe
nodes <- rbind(drivers, species, actions, managers, beneficiaries)

# Add colors and size
cols <- as.factor(nodes$subnetwork) |> as.numeric()
nodes$col <- viridis::viridis(max(cols))[cols]
nodes$size <- runif(nrow(nodes), .2, .75)

# Simulate links
links <- data.frame(
  from = sample(nodes$category, 300, replace = TRUE),
  to = sample(nodes$category, 300, replace = TRUE)
)
links <- links[!duplicated(links), ] # Remove duplicates
links <- links[!links$from == links$to, ] # Remove "cannibalism"

# Add colors and size
links$col <- "#876b40"
links$width <- .25

# # Combine in a single object
# metanetwork <- vector('list', 0)
# metanetwork$nodes <- nodes
# metanetwork$links <- links

# Export as system data
out <- "inst/extdata/"
if (!file.exists(out)) dir.create(out, recursive = TRUE)
write.csv(nodes, file = glue::glue("{out}nodes.csv"), row.names = FALSE)
write.csv(links, file = glue::glue("{out}links.csv"), row.names = FALSE)

# Export as internal data
usethis::use_data(nodes, links, overwrite = TRUE, internal = TRUE)
