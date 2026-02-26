# Note: the purpose of this script is to calculate target statistics and generate nodal attributes

# Characterization of nodal attributes and target statistics 
library(dplyr)
library(tidyr)
library(tibble)
library(gbp)
library(purrr)


indiv_summary_stats <- readRDS("data/network_stats_attributes/network_params.Rds") # load network parameters


source("R/node_attrib_target_pop.R") # source functions for calculation target statistics


pct =0.1 # proportion of modeled population relative to target population E.g.,0.1 corresponds to 10%, 0.0005 correspond to 0.05%

# Defining categories of age and layer variables to be used inside functions
target_age_grp <-  c("0-9y",   "10-19y", "20-29y", "30-39y", "40-59y", "60+y") %>% factor() # the six age group
layers <-  c("Home",    "School",  "Work",    "Nonhome")

output <- node_attrib_target_pop(netstats = indiv_summary_stats, 
                                 pct_target_pop = pct
                                 ) 

file.name <-  paste0("data/network_stats_attributes/node_attribute_target_stats", "__", pct, ".Rds")
saveRDS(output, file = file.name) 

