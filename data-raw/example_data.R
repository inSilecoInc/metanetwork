# source("data-raw/example_data.R")
# Individual network nodes
drivers <- data.frame(network = 'Drivers', name = paste0('driver_',1:15))
species <- data.frame(network = 'Species', name = paste0('species_',1:30))
actions <- data.frame(network = 'Actions', name = paste0('action_',1:9))
managers <- data.frame(network = 'Managers', name = paste0('manager_',1:9))
beneficiaries <- data.frame(network = 'Beneficiaries', name = paste0('beneficiary_',1:9))

# Nodes dataframe
nodes <- rbind(drivers, species, actions, managers, beneficiaries)

# Simulate links
links <- data.frame(from = sample(nodes$name, 300, replace = TRUE),
                    to = sample(nodes$name, 300, replace = TRUE))
links <- links[!duplicated(links), ]   # Remove duplicates
links <- links[!links$from == links$to, ] # Remove "cannibalism"

# Combine in a single object
metanetwork <- vector('list', 0)
metanetwork$nodes <- nodes
metanetwork$links <- links

# Export
usethis::use_data(metanetwork, overwrite = TRUE, internal = TRUE)
