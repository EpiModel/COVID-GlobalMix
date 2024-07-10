# Note: the purpose of this script is to simulate networks from the 4 layers of each network
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Urban"/"Rural"
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = "0.1"/"0.4"/"1"

# Packages
suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))
suppressMessages(library(fs))

# Load Data 
layers <- c("Home", "School", "Work", "Nonhome")

file.name_in <- 
    paste0("data/netest_outputs/netest_",
           layers, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
           )

ests <- list()

ests$Home <- 
  readRDS(file.name_in[1]) 
ests$School <- 
  readRDS(file.name_in[2]) 
ests$Work <- 
  readRDS(file.name_in[3]) 
ests$Nonhome <- 
  readRDS(file.name_in[4]) 

# Dynamic network simulation
## Load function for dynamic network simulation
source("R/sim_network.R") 
## Simulate network
nw_sim <- sim_network(est = ests, nsteps = 365)

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netsim_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Create file names to be saved
file.name <- 
  paste0("data/netsim_outputs/el_cuml__", layers, "__", 
         network,"__", est_apch,"__", percent_target_pop, ".Rds")

# Load "netdyn2el_cuml" function
source("R/forward_reacheable_path.R")

# Transforming "networkdynamic" into a "cumulative edgelist" and save
saveRDS(netdyn2el_cuml(nw_sim$Home), file.name[1])
saveRDS(netdyn2el_cuml(nw_sim$School), file.name[2])
saveRDS(netdyn2el_cuml(nw_sim$Work), file.name[3])
saveRDS(netdyn2el_cuml(nw_sim$Nonhome), file.name[4])






