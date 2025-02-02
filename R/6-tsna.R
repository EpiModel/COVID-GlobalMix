# Note: the purpose of this script is to calculate the forward-reachable path
# (FRP) under different scenarios.
# The following are arguments to be passed from the workflow to the HPC job,
# so not defined in this file:
#   network = "Rural"/"Urban"
#   est_apch = "mcmle"/"sto_apoxy"/
#   layer = "All"/"Home"/"School"/"Work"/"Nonhome"/, whith: "ALL" = 4 layers
#   percent_target_pop = 0.1/0.4/1
#   n_reps = 100, number of simulation to run
#   n_cores, number of HPC core to use


# Packages
library(tsna)
library(EpiModel)
library(fs)
library(dplyr)
library(future.apply)
future::plan("multicore", workers = n_cores)

source("R/reachable.R")

layers <- c("Home", "School", "Work", "Nonhome")

# Loading data
## file name of the outputted files
file.name_out <- paste0(
  "data/frp_outputs/frp_length_",
  layer, "__", 
  network,"__", 
  percent_target_pop, ".Rds"
)

# Loading edgelist
 if (layer %in%  layers) { # when the analysis is conducted for each layer individually 
  el_cuml <- readRDS( paste0(
    "data/netsim_outputs/el_cuml__", 
    layer, "__", 
    network,"__",  
    percent_target_pop, ".Rds"
    ))
} else if (layer == "All") { # when the edgelist to be analyzed is the individual layer

  # reading in individual layers
  file.name_in <- paste0(
    "data/netsim_outputs/el_cuml__", 
    layers, "__", 
    network,"__",  
    percent_target_pop, ".Rds")
  
  names(file.name_in) <- layers
  
  el_cuml_home <- readRDS(file.name_in[["Home"]])
  el_cuml_school <- readRDS(file.name_in[["School"]])
  el_cuml_work <- readRDS(file.name_in[["Work"]])
  el_cuml_nonhome <- readRDS(file.name_in[["Nonhome"]])
  
  # combining edgelists of different layers
  el_all <- el_cuml <- list()
  
  el_cuml <- future_lapply(seq_len(n_reps), \(i) {
    dedup_cumulative_edgelist(dplyr::bind_rows(
      el_cuml_home[[i]],
      el_cuml_school[[i]],
      el_cuml_work[[i]],
      el_cuml_nonhome[[i]]
    ))
  })
  
}


# Calculating FRP length for each node and time step for all simulation iterations
frp_lengths <- future_lapply(seq_len(n_reps), \(i) {
  get_forward_reachable(
    el_cuml[[i]],
    from_step = 1,
    to_step = 365,
    nodes = NULL # the number of nodes with edges whose FRPs are calculated, the default setting is NULL, which means FRPs for all nodes are calculated
  )$lengths #outputting the FRP length data frame
})



# The following script is for github, which creates an folder at HPC when the corresponding folder at local is empty
out_dir <- "data/frp_outputs"
if (!dir_exists(out_dir)) dir_create(out_dir)

# Outputting FRP result
saveRDS(frp_lengths, file = file.name_out)

