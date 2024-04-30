

# Characterization of nodal attributes and target statistics 
library("dplyr")

indiv_summary_stats <- readRDS("data/network_params/network_params.Rds") # load network parameters

source("R/node_attrib_target_pop.R") 
pct = 0.1 # percentage of target population, used to define the total number of node of the modeled population of a network
output <- node_attrib_target_pop(netstats = indiv_summary_stats, pct_target_pop = pct) # context can be "ten_percent_target_pop", "forty_percent_target_pop", or "all_target_pop"

file.name <-  paste0("data/network_params/node_attribute_target_stats", "__", pct, ".Rds")
saveRDS(output, file = file.name) 

# check the mean of the attribute of contact
## rural school
output$attr$rural %>% group_by(node.age.grp) %>% summarize(s_by_w_avg = mean(contact_attribute_School)
                                                           )
indiv_summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day_age$s_by_w_age_md
## rural work
output$attr$rural %>% group_by(node.age.grp) %>% summarize(s_by_w_avg = mean(contact_attribute_Work)
)
indiv_summary_stats$formation$formation_stats_rural$layer_assoc_rural$mean_deg_1day_age$w_by_s_age_md

## urban school
output$attr$urban %>% group_by(node.age.grp) %>% summarize(s_by_w_avg = mean(contact_attribute_School)
)
indiv_summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day_age$s_by_w_age_md
## urban work
output$attr$urban %>% group_by(node.age.grp) %>% summarize(s_by_w_avg = mean(contact_attribute_Work)
)
indiv_summary_stats$formation$formation_stats_urban$layer_assoc_urban$mean_deg_1day_age$w_by_s_age_md
