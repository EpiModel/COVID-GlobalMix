# Note: the purpose of this script is to simulate networks from the school,
# work, and nonhome layers of each network
# The following are arguments to be passed from the workflow to the HPC job,
# so not defined in this file:
#   network = "Urban"/"Rural"
#   network = "Rural"/"Urban
#   est_apch = "mcmle"/"sto_apoxy"
#   percent_target_pop = "0.1"/"0.4"/"1"
#   n_cores = 10
#   n_reps = 100

# Packages
library(dplyr)
library(EpiModel)
library(fs)
library(future.apply)
future::plan("multicore", workers = n_cores)

# Dynamic network simulation
## Load function for dynamic network simulation
source("R/sim_network.R")

# The following script is for github, which creates an folder at HPC
# when the corresponding folder at local is empty
out_dir <- "data/netsim_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Load Data
layers <- c( "School", "Work", "Nonhome")

est_files_path <- paste0(
  "data/netest_outputs/netest_",
  layers, "__",
  network, "__",
  est_apch, "__",
  percent_target_pop, ".Rds"
)
names(est_files_path) <- layers

ests <- list(
  School = readRDS(est_files_path$School),
  Work = readRDS(est_files_path$Work),
  Nonhome = readRDS(est_files_path$Nonhome)
)

# Run the simulations in parallel and store the 3 cumulative edge_lists
el_cumls <- future_replicate(
  n_reps,
  {
    sim <- sim_network(est = ests, nsteps = 365)
    list(
      School = as_cumulative_edgelist(sim$School),
      Work = as_cumulative_edgelist(sim$Work),
      Nonhome = as_cumulative_edgelist(sim$Nonhome)
    )
  },
  future.seed = TRUE
)
# el_cumls is a `n_reps` long list with the 3 cumulative edge_lists

# Make it into a list[3] of list[n_reps]
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

# Create file names to be saved for cumulative edgelist
el_file_path <- paste0(
  "data/netsim_outputs/el_cuml__",
  layers, "__",
  network,"__",
  est_apch,"__",
  percent_target_pop, ".Rds"
)
names(el_file_path) <- layers

# Output the edgelist items
saveRDS(edge_list$School, el_file_path$School)
saveRDS(edge_list$Work, el_file_path$Work)
saveRDS(edge_list$Nonhome, el_file_path$Nonhome)
# Saving "networkDynamic" for diagnosis
# saveRDS(nw_sim,
#         paste0("data/netsim_outputs/networkdynamic__",
#                network,"__", est_apch,"__", percent_target_pop, ".Rds")
#         )
