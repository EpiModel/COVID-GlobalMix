

# Characterization of nodal attributes and target statistics 
library("dplyr")

indiv_summary_stats <- readRDS("data/network_params/network_params.Rds") # load network parameters

source("R/node_attrib_target_pop.R") 
pct = 0.1 # percentage of target population, used to define the total number of node of the modeled population of a network
output <- node_attrib_target_pop(netstats = indiv_summary_stats, pct_target_pop = pct) # context can be "ten_percent_target_pop", "forty_percent_target_pop", or "all_target_pop"

file.name <-  paste0("data/network_params/node_attribute_target_stats", "__", pct, ".Rds")
saveRDS(output, file = file.name) 


