# Note: the purpose of this script is to process big frp files at the HPC to runnable files at the local environment
# The following are arguments to be passed from the workflow to the HPC job,
# so not defined in this file:
#   network = "Rural"/"Urban"
#   layer = "Home"/"School"/"Work"/"Nonhome"
#   percent_target_pop = 0.5
#   n_reps = 100, number of simulation ran

# Packages
library(dplyr); library(tibble); library(tidyr); library(fs)



# Load network stats to retrieve the number of nodes at each network
## Target stats
tar_stats <-  
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats__", percent_target_pop, ".Rds"))


# Load FRP outputs
file.name_frp <- paste0(
  "data/frp_outputs/frp_length_",
  layer, "__",
  network,"__",
  percent_target_pop, ".Rds"
)


frp_layer_network <- readRDS(file.name_frp)

# file name of the output file
file.name_out <- paste0(
  "data/frp_outputs_processed/frp_processed_",
  layer, "__",
  network,"__",
  percent_target_pop, "__",
  n_reps,".Rds"
)



source("./R/result_helper_functions.R")

output <- list()


## Create a variable of nodal ID
tar_stats$attr[[tolower(network)]] <- tar_stats$attr[[tolower(network)]] %>% 
  rownames_to_column(var = "node_id") %>% 
  mutate(node_id = paste0("node_", node_id))



## Calculate proportion of population reached for each node
prop_reached <- list()

for (i in 1:n_reps) {
  
  prop_reached[[i]] <-
    frp_length_df_process(
      attr = tar_stats$attr[[tolower(network)]] %>% select(node_id, node.age.grp), 
      frp_length = frp_layer_network[[i]]
    )
  
}


# Table 2 snippet, proportion of population reached
output$prop_tb <- prop_table_layer(prop_reached = prop_reached)


# Data frame for Figure 1 (Percentages of populations reached over a 1-year period)
output$prop_figure_df <- 
prop_figure_layer(prop_reached = prop_reached)


out_dir <- "data/frp_outputs_processed"
if (!dir_exists(out_dir)) dir_create(out_dir)

saveRDS(output, file = file.name_out)









