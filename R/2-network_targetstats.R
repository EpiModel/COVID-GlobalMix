# Note: the purpose of this script is to calculate target statistics

# Characterization of nodal attributes and target statistics 
library(dplyr)
library(tidyr)
library(tibble)

indiv_summary_stats <- readRDS("data/network_stats_attributes/network_params.Rds") # load network parameters

source("R/node_attrib_target_pop.R") 
pct = 0.1 # percentage of target population, used to define the total number of node of the modeled population of a network
output <- node_attrib_target_pop(netstats = indiv_summary_stats, pct_target_pop = pct) 

file.name <-  paste0("data/network_stats_attributes/node_attribute_target_stats", "__", pct, ".Rds")
saveRDS(output, file = file.name) 

