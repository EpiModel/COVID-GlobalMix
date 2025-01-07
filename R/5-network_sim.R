# Note: the purpose of this script is to simulate networks from the school, work, and nonhome layers of each network
# The following are arguments to be passed from the workflow to the HPC job, so not defined in this file
# network = "Urban"/"Rural"
# network = "Rural"/"Urban
# est_apch = "mcmle"/"sto_apoxy"
# percent_target_pop = "0.1"/"0.4"/"1"

# Packages
suppressMessages(library(dplyr))
suppressMessages(library(EpiModel))
suppressMessages(library(fs))

# Load Data 
layers <- c( "School", "Work", "Nonhome")

file.name_in <- 
    paste0("data/netest_outputs/netest_",
           layers, "__", network,"__", est_apch,"__", percent_target_pop, ".Rds"
           )

ests <- list()

ests$School <- 
  readRDS(file.name_in[1]) 
ests$Work <- 
  readRDS(file.name_in[2]) 
ests$Nonhome <- 
  readRDS(file.name_in[3]) 

# Dynamic network simulation
## Load function for dynamic network simulation
source("R/sim_network.R") 

nw_sim <- list() # define a list item to store simulated networks over 100 iterations

for (i in 1:100) {
## Simulate network, one iteration
nw_sim[[i]] <- sim_network(est = ests, nsteps = 365)

}

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netsim_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Create lists to save results 
edge_list_School <- edge_list_Work <- edge_list_Nonhome <- list()

# Concert networkDynamics items from the 100 run to edgelists
for(i in 1:100){
  edge_list_School[[i]] <- as_cumulative_edgelist(nw_sim[[i]]$School)
  edge_list_Work[[i]] <-  as_cumulative_edgelist(nw_sim[[i]]$Work)
  edge_list_Nonhome[[i]] <-  as_cumulative_edgelist(nw_sim[[i]]$Nonhome)
  
}

# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/netsim_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Create file names to be saved for cumulative edgelist
file.name <- 
  paste0("data/netsim_outputs/el_cuml__", layers, "__", 
         network,"__", est_apch,"__", percent_target_pop, ".Rds")


# Output the edgelist items
saveRDS(edge_list_School, file.name[1]) # school
saveRDS(edge_list_Work, file.name[2]) # work
saveRDS(edge_list_Nonhome, file.name[3]) #nonhome
# Saving "networkDynamic" for diagnosis
# saveRDS(nw_sim, 
#         paste0("data/netsim_outputs/networkdynamic__", 
#                network,"__", est_apch,"__", percent_target_pop, ".Rds")
#         )






