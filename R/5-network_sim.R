# Note: the purpose of this script is to simulate networks from the school, 
# work, and nonhome layers of each network, which are outputted as edgelists
# The following are arguments to be passed from the workflow to the HPC job, 
# so not defined in this file:
#   network = "Rural"/"Urban
#   est_apch = "mcmle"/"sto_apoxy"
#   percent_target_pop = "0.1"/"0.4"/"1"
#   n_cores, number of HPC cores to use
#   n_reps = 100

options(future.globals.maxSize = 600 * 1024^2)  # sets limit to 600 MiB

# Packages
library(dplyr)
library(EpiModel)
library(fs)
library(gbp)
library(tidyr)
library(future.apply)
future::plan("multicore", workers = n_cores)

# Dynamic network simulation for school, work, and nonhome layers, 
# and simulate household ids and edgelists for home layer for n_reps times
## Load functions 
source("R/sim_network.R") 

# Load Data 
## Load netest-estimated items for school, work, and nonhome layers
layers <- c("Home", "School", "Work", "Nonhome")

est_files_path <- 
    paste0(
      "data/netest_outputs/netest_",
      layers[-1], "__", 
      network,"__", 
      est_apch,"__", 
      percent_target_pop, ".Rds"
           )

names(est_files_path) <- layers[-1]

ests <- 
  list(
    School = readRDS(est_files_path[["School"]]),
    Work = readRDS(est_files_path[["Work"]]),
    Nonhome = readRDS(est_files_path[["Nonhome"]])
  )


## Load necessary statistics to simulate edgelist for the home layer
### Observed proportions and frequency of different type of household members
hh_member_stat <- readRDS("data/network_stats_attributes/network_params.Rds")$prop_hh_members[[network]]


### Mean deg at home, calculated as (total edge count)/(total nodes)
mean_deg_home <- 
  readRDS(paste0("data/network_stats_attributes/node_attribute_target_stats", "__", percent_target_pop, ".Rds"))$node_hh_assign$mean_deg_home[[network]]


### Load initial age group of each node and recode them into 3 categories for simulating houshold ids
init_node.age.grp <- 
recode(get_vertex_attribute(ests$School$newnetwork,"age.grp"), # recode 6-category age groups to 3-category age groups consist of "0-19y", 20-59y", and "60-100y" for simulating household ids
       `0-9y` = "0-19y",
       `10-19y` = "0-19y",
       `20-29y` = "20-59y",
       `30-39y` = "20-59y",
       `40-59y` = "20-59y",
       `60+y` = "60+y")


set.seed(20250114) 

# Network simulation for school, work, and nonhome
## Run the simulations in parallel and store the 3 cumulative edge_lists for school, work, and nonhome
el_cumls <- future_replicate(
  n_reps,
  {
    ## Simulate network for school, work, and nonhome layers from day 1 to 365, for n_reps times
    sim <- sim_network(est = ests, nsteps = 365)
    ## Convert to edgelists
    list(
      School = as_cumulative_edgelist(sim$School),
      Work = as_cumulative_edgelist(sim$Work),
      Nonhome = as_cumulative_edgelist(sim$Nonhome)
    )
  },
  future.seed = TRUE
)

# Make an empty list - a list[3] with list[n_reps]
edge_lists <- list(
  School = vector(mode = "list", length = n_reps),
  Work = vector(mode = "list", length = n_reps),
  Nonhome = vector(mode = "list", length = n_reps)
)

for (i in seq_len(n_reps)) {
  edge_list$School[[i]] <- el_cumls[[i]]$School
  edge_list$Work[[i]] <- el_cumls[[i]]$Work
  edge_list$Nonhome[[i]] <- el_cumls[[i]]$Nonhome
}


# Network simulation for home
## define empty lists to store things 
hh_id_edgelist <- edge_list_Home <- hh_id_attr <-  hh_id_validation <- hh_id_attr_validation  <- list() 
for (i in 1:n_reps) {
  
## Simulate edgelist and household IDs for home layer for each iteration i
hh_id_edgelist[[i]] <-
  node_hh_assign(
      #observed proportions of households with children, adults, and elderly, respectively.
      prop.hh.with.child = hh_member_stat$proportions %>% filter(prop_type == "prop_hh_w_child") %>% pull(proportion),
      prop.hh.with.adult = hh_member_stat$proportions %>% filter(prop_type == "prop_hh_w_adult") %>% pull(proportion),
      prop.hh.with.elderly = hh_member_stat$proportions %>% filter(prop_type == "prop_hh_w_elderly") %>% pull(proportion),
      #observed proportions of children with adults.
      prop.children.with.adult = hh_member_stat$proportions %>% filter(prop_type == "prop_child_w_adult") %>% pull(proportion),
      #observed frequency if household only with children, not used in the calculation and for validation only
      freq.hh.child.only = hh_member_stat$freq_hh_only_w_child,
      # mean degree calculated from the age mixing matrix of edge count of the home layer of a network
      mean.deg =  mean_deg_home ,
      # age group of each node in the modeled population
      age.grp= init_node.age.grp
    )
  
  ### save home edgelist of each simulation
  edge_list_Home[[i]] <- hh_id_edgelist[[i]]$edgelist %>% 
    rename(head=head.node.ids, tail=tail.node.ids ) %>% select(head, tail) %>% mutate(start=1, stop=2) # rename to the variable names used in the FRP function and adding time
  
  ### save household ids of each simulation
  hh_id_attr[[i]] <- hh_id_edgelist[[i]]$assignments  %>% select(node.ids,hh.ids)
  
  ### save validation result of each simulation
  hh_id_validation[[i]] <- hh_id_edgelist[[i]]$validation
  
}

hh_id_attr_validation$attr <- hh_id_attr; hh_id_attr_validation$validation <- hh_id_validation

# Outputting items
## The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netsim_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

## Create file names to be saved for cumulative edgelist
el_file_path <- 
  paste0(
    "data/netsim_outputs/el_cuml__", 
    layers, "__", 
    network,"__",  
    percent_target_pop, ".Rds"
    )

names(el_file_path) <- layers

## Output the edgelist items
saveRDS(edge_list_Home, el_file_path[["Home"]]) # home
saveRDS(edge_list$School, el_file_path[["School"]]) # school
saveRDS(edge_list$Work, el_file_path[["Work"]]) # work
saveRDS(edge_list$Nonhome, el_file_path[["Nonhome"]]) #nonhome

## Output household id attribute and validation result for household id simulation
saveRDS(hh_id_attr_validation,  paste0("data/netsim_outputs/hh_id_attr_validation__", 
                                       network,"__",  percent_target_pop, ".Rds")
        )






