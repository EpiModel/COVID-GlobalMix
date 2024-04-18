

# Characterization of nodal attributes and target statistics 
library("tidyverse")

indiv_summary_stats <- readRDS("data/network_params/network_params.RData") # load network parameters

source("R/node_attrib_target_pop.R") 
output <- node_attrib_target_pop(netstats = indiv_summary_stats, context = "ten_percent_target_pop") # context can be "ten_percent_target_pop", "forty_percent_target_pop", or "all_target_pop"

saveRDS(output, file = "data/network_params/node_attribute_target_stats.RData")

